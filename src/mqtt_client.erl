%%
%% Copyright (c) 2021 dushin.net
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------------
%% @doc An MQTT client for AtomVM.
%%
%% This module can be used to publish message to and subscribe to topics on
%% an MQTT broker.
%%
%% Much of the interaction with this module is via callback functions or via messages
%% delivered to a specified PID.  For example, the `start' method will start
%% and MQTT client instance, but notification that the client is connected to
%% the specified broker is performed asynchronously via a callback function
%% or a discriminatable message deievered to a designated process.  Similarly,
%% notifications of publishes (QoS1) and subscriptions are performed via callbacks.
%%
%% Most callback functions provide a referece to the MQTT client instance, along
%% with other information relevant to the specific context (e.g., the MQTT topic
%% or the message being delivered).  Generally, the return value from this function
%% is ignored.  See the documentation for each function for the arity and semantics
%% of specific callback function parameters.
%%
%% Alternatively, users may specify a PID to which messages should be delivered
%% when an event occurs.  For example, you can specify a PID that will be sent
%% a message when the MQTT client connects or disconnects to the MQTT broker.
%%
%% Messages are sent as tuples with the atom `mqtt' as the first element, and
%% subsequent elements relevant to the specific context (e.g., the MQTT topic
%% or the message being delivered).  See the documentation for each function
%% for expected message structure for such notification messages.
%% @end
%%-----------------------------------------------------------------------------
-module(mqtt_client).

-export([
    start/1, stop/1, disconnect/1, reconnect/1,
    get_config/1, get_pending_publishes/1, get_pending_subscriptions/1, get_pending_unsubscriptions/1,
    publish/3, publish/4, subscribe/3, unsubscribe/3
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behavior(gen_server).

% -define(TRACE(A, B), io:format(A, B)).
-define(TRACE(A, B), ok).

-record(state, {
    port,
    config,
    pending_publishes = #{},
    pending_subscriptions = #{},
    pending_unsubscriptions = #{},
    subscriber_map = #{}
}).

-record(subscriber, {
    msg_id,
    topic,
    subscribed_handler,
    data_handler
}).

-type binary_or_string() :: string() | binary().

-type mqtt() :: pid().
-type config() :: #{
    url => binary_or_string(),
    connected_handler => fun((mqtt()) -> any()),
    disconnected_handler => fun((mqtt()) -> any()),
    error_handler => fun((mqtt(), error()) -> any()),
    username => binary_or_string(),
    password => binary_or_string()
}.

-type error_type() :: esp_tls | connection_refused | undefined.
-type connect_return_code() :: connection_accepted | protocol | id_rejected | server_unavailable | bad_username | not_authorized | undefined.
-type tls_last_esp_err() :: integer().
-type tls_stack_err() :: integer().
-type tls_cert_verify_flags() :: integer().
-type error() :: {error_type(), connect_return_code(), tls_last_esp_err(), tls_stack_err(), tls_cert_verify_flags()}.

-type qos() :: at_most_once | at_least_once | exactly_once.

-type publish_options() :: #{
    published_handler => fun((mqtt(), topic(), msg_id()) -> any()) | pid(),
    qos => qos(),
    retain => boolean()
}.

-type subscribe_options() :: #{
    max_qos => qos(),
    subscribed_handler => fun((mqtt(), topic()) -> any()) | pid(),
    data_handler => fun((mqtt(), topic(), binary()) -> any()) | pid()
}.

-type unsubscribe_options() :: #{
    unsubscribed_handler => fun((mqtt(), topic()) -> any()) | pid()
}.

-type topic() :: binary() | string().
-type msg_id() :: integer().

%%-----------------------------------------------------------------------------
%% @param   Config      configuration
%% @returns ok | {error, Reason}
%% @doc     Start an instance of the MQTT client.
%%
%% This function will start an instance of an MQTT client.  Use the returned
%% reference in subsequent MQTT operations.
%%
%% This function takes a configuration map as a parameter.  This map configures
%% the client for connectivity to an MQTT broker, in addition to configuration
%% parameters that govern the behavior of the client.
%%
%% Specify a callback function or PID to which to send a message when the MQTT
%% client connects to the MQTT broker.  Once the client is connected, you can
%% publish messages to topics or subscribute to messages from topics.
%%
%% The following options are supported (or required):
%%
%% <table style="width:100%">
%%   <tr>
%%      <th>Key</th>
%%      <th>Type</th>
%%      <th>Default Value</th>
%%      <th>Required</th>
%%      <th>Description</th>
%%   </tr>
%%   <tr>
%%      <td>`url'</td>
%%      <td>string or binary</td>
%%      <td></td>
%%      <td>yes</td>
%%      <td>A URL designating the location of the MQTT broker</td>
%%   </tr>
%%   <tr>
%%      <td>`username'</td>
%%      <td>string or binary</td>
%%      <td></td>
%%      <td>only if authenticating to broker</td>
%%      <td>user name used to authenticate to the MQTT broker</td>
%%   </tr>
%%   <tr>
%%      <td>`password'</td>
%%      <td>string or binary</td>
%%      <td></td>
%%      <td>only if authenticating to broker</td>
%%      <td>password used to authenticate to the MQTT broker</td>
%%   </tr>
%%   <tr>
%%      <td>`connected_handler'</td>
%%      <td>pid or function</td>
%%      <td></td>
%%      <td>yes, typically</td>
%%      <td>A function to call or pid to send a message to when the client connects to the MQTT broker.
%%      </td>
%%   </tr>
%%   <tr>
%%      <td>`disconnected_handler'</td>
%%      <td>pid or function</td>
%%      <td></td>
%%      <td>no</td>
%%      <td>A function to call or pid to send a message to when the client disconnects from the MQTT broker.</td>
%%   </tr>
%%   <tr>
%%      <td>`error_handler'</td>
%%      <td>pid or function</td>
%%      <td></td>
%%      <td>no</td>
%%      <td>A function to call or pid to send a message to when an error occurs in communication with the MQTT broker.</td>
%%   </tr>
%% </table>
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config::config()) -> {ok, mqtt()} | {error, Reason::term()}.
start(Config) ->
    gen_server:start(?MODULE, Config, []).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @returns ok
%% @doc     Stop the specified MQTT.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(MQTT::mqtt()) -> ok.
stop(MQTT) ->
    gen_server:stop(MQTT).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @returns ok
%% @doc     Disconnect the specified MQTT from the MQTT broker.
%% @end
%%-----------------------------------------------------------------------------
-spec disconnect(MQTT::mqtt()) -> ok.
disconnect(MQTT) ->
    gen_server:call(MQTT, disconnect).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @returns ok
%% @doc     Reconnect the specified MQTT to the MQTT broker.
%% @end
%%-----------------------------------------------------------------------------
-spec reconnect(MQTT::mqtt()) -> ok | error.
reconnect(MQTT) ->
    gen_server:call(MQTT, reconnect).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @returns Configuration used to initialize this MQTT instance
%% @doc     Get the configuation used to initialize this MQTT instance
%% @end
%%-----------------------------------------------------------------------------
-spec get_config(MQTT::mqtt()) -> config().
get_config(MQTT) ->
    gen_server:call(MQTT, config).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @returns list of message ids representing pending publishes
%% @doc     Get the list of pending publishes associated with this MQTT instance
%% @end
%%-----------------------------------------------------------------------------
-spec get_pending_publishes(MQTT::mqtt()) -> [msg_id()].
get_pending_publishes(MQTT) ->
    gen_server:call(MQTT, pending_publishes).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @returns list of message ids representing pending subscriptions
%% @doc     Get the list of pending subscriptions associated with this MQTT instance
%% @end
%%-----------------------------------------------------------------------------
-spec get_pending_subscriptions(MQTT::mqtt()) -> [msg_id()].
get_pending_subscriptions(MQTT) ->
    gen_server:call(MQTT, pending_subscriptions).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @returns list of message ids representing pending unsubscriptions
%% @doc     Get the list of pending unsubscriptions associated with this MQTT instance
%% @end
%%-----------------------------------------------------------------------------
-spec get_pending_unsubscriptions(MQTT::mqtt()) -> [msg_id()].
get_pending_unsubscriptions(MQTT) ->
    gen_server:call(MQTT, pending_unsubscriptions).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @param   Topic   the MQTT topic (string or binary)
%% @param   Message the message to publish (string or binary)
%% @returns ok | {error, Reason}
%% @doc     Publish a message to a topic.
%%
%% This function is equivalent to `publish(MQTT, Topic, Message, #{})'
%% @end
%%-----------------------------------------------------------------------------
-spec publish(MQTT::mqtt(), Topic::topic(), Message::binary_or_string()) -> ok | {error, Reason::term()}.
publish(MQTT, Topic, Message)
    when is_pid(MQTT) andalso (is_binary(Topic) orelse is_list(Topic)) andalso (is_binary(Message) orelse is_list(Message)) ->
    ?MODULE:publish(MQTT, Topic, Message, maps:new());
publish(_,_,_) ->
    throw(badarg).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @param   Topic   the MQTT topic (string or binary)
%% @param   Message the message to publish (string or binary)
%% @param   PublishOptions additional options to specify when publishing
%% @returns ok | {error, Reason}
%% @doc     Publish a message to a topic.
%%
%% This function will publish a message (string or binary) to an MQTT topic.
%%
%% The MQTT client should be connected before calling this function.
%%
%% The following options are supported (or required):
%%
%% <table style="width:100%">
%%   <tr>
%%      <th>Key</th>
%%      <th>Type</th>
%%      <th>Default Value</th>
%%      <th>Required</th>
%%      <th>Description</th>
%%   </tr>
%%   <tr>
%%      <td>`published_handler'</td>
%%      <td>pid or function</td>
%%      <td></td>
%%      <td>no</td>
%%      <td>A function to call or pid to send a message to when the client publish is acknowledged by the MQTT broker.
%%          This function will only be called when `qos' is greater than 1.
%%      </td>
%%   </tr>
%%   <tr>
%%      <td>`qos'</td>
%%      <td>`{0..2}'</td>
%%      <td>0</td>
%%      <td>no</td>
%%      <td>Qos.</td>
%%   </tr>
%%   <tr>
%%      <td>`retain'</td>
%%      <td>boolean</td>
%%      <td>false</td>
%%      <td>no</td>
%%      <td>Whether to retain the message.</td>
%%   </tr>
%% </table>
%% @end
%%-----------------------------------------------------------------------------
-spec publish(MQTT::mqtt(), Topic::topic(), Message::binary_or_string(), PublishOptions::publish_options()) -> msg_id() | {error, Reason::term()}.
publish(MQTT, Topic, Message, PublishOptions)
    when is_pid(MQTT) andalso (is_binary(Topic) orelse is_list(Topic)) andalso (is_binary(Message) orelse is_list(Message)) andalso is_map(PublishOptions) ->
    gen_server:call(MQTT, {publish, Topic, Message, validate_publish_options(PublishOptions)}, 30000);
publish(_,_,_, _) ->
    throw(badarg).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @param   Topic   the MQTT topic (string or binary)
%% @param   SubscribeOptions additional options to specify when subscribing
%% @returns ok | {error, Reason}
%% @doc     Subscribe to a topic.
%%
%% This function will subscribe to an MQTT topic.  Applications are notified that
%% a subscription is successful via `subscribed_handler' callback, and applications
%% receive data via the `data_handler' callback.
%%
%% The MQTT client should be connected before calling this function.
%%
%% The following options are supported (or required):
%%
%% <table style="width:100%">
%%   <tr>
%%      <th>Key</th>
%%      <th>Type</th>
%%      <th>Default Value</th>
%%      <th>Required</th>
%%      <th>Description</th>
%%   </tr>
%%   <tr>
%%      <td>`subscribed_handler'</td>
%%      <td>pid or function</td>
%%      <td></td>
%%      <td>no</td>
%%      <td>A function to call or pid to send a message to when the client subscriptions is acknowledged by the MQTT broker.
%%      </td>
%%   </tr>
%%   <tr>
%%      <td>`data_handler'</td>
%%      <td>pid or function</td>
%%      <td></td>
%%      <td>no</td>
%%      <td>A function to call or pid to send a message to when a message is recieved on the topic from the MQTT broker.
%%      </td>
%%   </tr>
%%   <tr>
%%      <td>`qos'</td>
%%      <td>`{0..2}'</td>
%%      <td>0</td>
%%      <td>no</td>
%%      <td>Qos.</td>
%%   </tr>
%% </table>
%% @end
%%-----------------------------------------------------------------------------
-spec subscribe(MQTT::mqtt(), Topic::topic(), SubscribeOptions::subscribe_options()) -> ok | {error, Reason::term()}.
subscribe(MQTT, Topic, SubscribeOptions)
    when is_pid(MQTT) andalso (is_binary(Topic) orelse is_list(Topic)) andalso is_map(SubscribeOptions) ->
    gen_server:call(MQTT, {subscribe, Topic, validate_subscribe_options(SubscribeOptions)}, 30000);
subscribe(_,_,_) ->
    throw(badarg).

%%-----------------------------------------------------------------------------
%% @param   MQTT    the MQTT client instance created via `start/1'
%% @param   Topic   the MQTT topic (string or binary)
%% @param   UnSubscribeOptions additional options to specify when unsubscribing
%% @returns ok | {error, Reason}
%% @doc     Subscribe to a topic.
%%
%% This function will unsubscribe from an MQTT topic.  Applications are notified that
%% a subscription is removed via `unsubscribed_handler' callback.
%%
%% The MQTT client should be subscribed and connected before calling this function.
%%
%% The following options are supported (or required):
%%
%% <table style="width:100%">
%%   <tr>
%%      <th>Key</th>
%%      <th>Type</th>
%%      <th>Default Value</th>
%%      <th>Required</th>
%%      <th>Description</th>
%%   </tr>
%%   <tr>
%%      <td>`unsubscribed_handler'</td>
%%      <td>pid or function</td>
%%      <td></td>
%%      <td>no</td>
%%      <td>A function to call or pid to send a message to when the client unsubscriptions is acknowledged by the MQTT broker.
%%      </td>
%%   </tr>
%% </table>
%% @end
%%-----------------------------------------------------------------------------
-spec unsubscribe(MQTT::mqtt(), Topic::topic(), UnSubscribeOptions::unsubscribe_options()) -> ok | {error, Reason::term()}.
unsubscribe(MQTT, Topic, UnSubscribeOptions)
    when is_pid(MQTT) andalso (is_binary(Topic) orelse is_list(Topic)) andalso is_map(UnSubscribeOptions) ->
    gen_server:call(MQTT, {unsubscribe, Topic, validate_unsubscribe_options(UnSubscribeOptions)}, 30000);
unsubscribe(_,_,_) ->
    throw(badarg).


%% ============================================================================
%%
%% gen_server API
%%
%% ============================================================================

%% @hidden
init(Config) ->
    try
        Self = self(),
        Port = erlang:open_port({spawn, "atomvm_mqtt_client"}, [{receiver, Self}, {url, maps:get(url, Config)}]),
        % Port = mqtt_simulator:start(Self),
        {ok, #state{
            port=Port,
            config=Config
        }}
    catch
        _:Error ->
            {stop, Error}
    end.

%% @hidden
handle_call(disconnect, _From, State) ->
    Reply = do_disconnect(State#state.port),
    {reply, Reply, State};
handle_call(reconnect, _From, State) ->
    Reply = do_reconnect(State#state.port),
    {reply, Reply, State};
handle_call(config, _From, State) ->
    {reply, State#state.config, State};
handle_call(pending_publishes, _From, State) ->
    {reply, maps:keys(State#state.pending_publishes), State};
handle_call(pending_subscriptions, _From, State) ->
    {reply, maps:keys(State#state.pending_subscriptions), State};
handle_call(pending_unsubscriptions, _From, State) ->
    {reply, maps:keys(State#state.pending_unsubscriptions), State};
handle_call({publish, Topic, Message, PublishOptions}, _From, State) ->
    ?TRACE("Handling call for publish.  Topic=~p Message: ~p PublishOptions=~p~n", [Topic, Message, PublishOptions]),
    Qos = maps:get(qos, PublishOptions, at_most_once),
    Retain = maps:get(retain, PublishOptions, false),
    MsgId = do_publish(State#state.port, Topic, Message, Qos, Retain),
    case Qos of
        at_most_once ->
            {reply, ok, State};
        _ ->
            PendingPublishes = State#state.pending_publishes,
            NewPendingPublishes = PendingPublishes#{MsgId => {Topic, PublishOptions}},
            {reply, MsgId, State#state{pending_publishes=NewPendingPublishes}}
    end;
handle_call({subscribe, Topic, Options}, _From, State) ->
    ?TRACE("Handling call for subscribe.  Topic=~p Options=~p~n", [Topic, Options]),
    SubscriberMap = State#state.subscriber_map,
    case maps:get(Topic, SubscriberMap, undefined) of
        undefined ->
            Qos = maps:get(qos, Options, at_most_once),
            case do_subscribe(State#state.port, Topic, Qos) of
                MsgId when is_integer(MsgId) ->
                    ?TRACE("Subscription msg_id: ~p~n", [MsgId]),
                    NewSubscriber = #subscriber{
                        msg_id=MsgId,
                        topic=Topic,
                        subscribed_handler = maps:get(subscribed_handler, Options, undefined),
                        data_handler = maps:get(data_handler, Options, undefined)
                    },
                    PendingSubscriptions = State#state.pending_subscriptions,
                    NewPendingSubscriptions = PendingSubscriptions#{MsgId => NewSubscriber},
                    {reply, ok, State#state{pending_subscriptions=NewPendingSubscriptions}};
                Error ->
                    {reply, Error, State}
            end;
        _Subscriber ->
            {reply, {error, already_subscribed}, State}
    end;
handle_call({unsubscribe, Topic, Options}, _From, State) ->
    ?TRACE("Handling call for unsubscribe.  Topic=~p Options=~p~n", [Topic, Options]),
    SubscriberMap = State#state.subscriber_map,
    case maps:get(Topic, SubscriberMap, undefined) of
        undefined ->
            {reply, {error, not_subscribed}, State};
        _ ->
            case do_unsubscribe(State#state.port, Topic) of
                MsgId when is_integer(MsgId) ->
                    PendingUnsubscriptions = State#state.pending_unsubscriptions,
                    NewPendingUnsubscriptions = PendingUnsubscriptions#{MsgId => {Topic, Options}},
                    {reply, ok, State#state{pending_unsubscriptions=NewPendingUnsubscriptions}};
                Error ->
                    {reply, Error, State}
            end
    end;
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({mqtt, connected}, State) ->
    ?TRACE("handle_info: Received {mqtt, connected}~n", []),
    Config = State#state.config,
    Self = self(),
    case maps:get(connected_handler, Config, undefined) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            Pid ! {mqtt, connected, self()};
        Fun when is_function(Fun) ->
            spawn(fun() -> Fun(Self) end)
    end,
    {noreply, State};
handle_info({mqtt, disconnected}, State) ->
    ?TRACE("handle_info({mqtt, disconnected}~n", []),
    Config = State#state.config,
    Self = self(),
    case maps:get(disconnected_handler, Config, undefined) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            Pid ! {mqtt, disconnected, self()};
        Fun when is_function(Fun) ->
            spawn(fun() -> Fun(Self) end)
    end,
    {noreply, State};
handle_info({mqtt, error, Error}, State) ->
    ?TRACE("handle_info({mqtt, disconnected}~n", []),
    Config = State#state.config,
    Self = self(),
    case maps:get(error_handler, Config, undefined) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            Pid ! {mqtt, error, self(), Error};
        Fun when is_function(Fun) ->
            spawn(fun() -> Fun(Self, Error) end)
    end,
    {noreply, State};
handle_info({mqtt, published, MsgId}, State) ->
    ?TRACE("handle_info({mqtt, published, ~p}~n", [MsgId]),
    PendingPublishes = State#state.pending_publishes,
    NewPendingPublishes = case maps:get(MsgId, PendingPublishes, undefined) of
        undefined ->
            io:format("WARNING: `published` message received but no subscriber was found for msg id ~p~n", [MsgId]),
            PendingPublishes;
        {Topic, PublishOptions} ->
            ?TRACE("Found pending publish.  Topic=~p PublishOptions=~p~n", [Topic, PublishOptions]),
            Self = self(),
            case maps:get(published_handler, PublishOptions, default) of
                default ->
                    ok;
                Pid when is_pid(Pid) ->
                    Pid ! {mqtt, published, Self, Topic, MsgId};
                Fun when is_function(Fun) ->
                    spawn(fun() -> Fun(Self, Topic, MsgId) end)
            end,
            maps:remove(MsgId, PendingPublishes)
    end,
    {noreply, State#state{pending_publishes=NewPendingPublishes}};
handle_info({mqtt, subscribed, MsgId}, State) ->
    ?TRACE("handle_info({mqtt, subscribed, ~p}, State~n", [MsgId]),
    SubscriberMap = State#state.subscriber_map,
    PendingSubscriptions = State#state.pending_subscriptions,
    NewSubscriberMap = case maps:get(MsgId, PendingSubscriptions, undefined) of
        undefined ->
            io:format("WARNING: `subscribed` message received but no pending subscription was found for msg id ~p~n", [MsgId]),
            SubscriberMap;
        Subscriber ->
            ?TRACE("Found subscriber.  Subscriber=~p~n", [Subscriber]),
            Topic = Subscriber#subscriber.topic,
            Self = self(),
            case Subscriber#subscriber.subscribed_handler of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    Pid ! {mqtt, subscribed, Self, Topic};
                Fun when is_function(Fun) ->
                    spawn(fun() -> Fun(Self, Topic) end)
            end,
            SubscriberMap#{Topic => Subscriber}
    end,
    {noreply, State#state{
        subscriber_map=NewSubscriberMap,
        pending_subscriptions=maps:remove(MsgId, PendingSubscriptions)
    }};
handle_info({mqtt, unsubscribed, MsgId}, State) ->
    ?TRACE("handle_info({mqtt, unsubscribed, ~p}~n", [MsgId]),
    SubscriberMap = State#state.subscriber_map,
    PendingUnSubscriptions = State#state.pending_unsubscriptions,
    NewSubscriberMap = case maps:get(MsgId, PendingUnSubscriptions, undefined) of
        undefined ->
            io:format("WARNING: `unsubscribed` message received but no pending unsubscription was found for msg id ~p~n", [MsgId]),
            SubscriberMap;
        {Topic, Options} ->
            Self = self(),
            case maps:get(unsubscribed_handler, Options, default) of
                default ->
                    ok;
                Pid when is_pid(Pid) ->
                    Pid ! {mqtt, unsubscribed, Self, Topic};
                Fun when is_function(Fun) ->
                    spawn(fun() -> Fun(Self, Topic) end)
            end,
            maps:remove(Topic, SubscriberMap)
    end,
    {noreply, State#state{
        subscriber_map=NewSubscriberMap,
        pending_unsubscriptions=maps:remove(MsgId, PendingUnSubscriptions)
    }};
handle_info({mqtt, data, Topic, Data}, State) ->
    ?TRACE("handle_info({mqtt, data, ~p ~p}~n", [Topic, Data]),
    SubscriberMap = State#state.subscriber_map,
    case maps:get(Topic, SubscriberMap, undefined) of
        undefined ->
            io:format("WARNING: `data` message received but no subscriber was found for topic ~p~n", [Topic]);
        Subscriber ->
            Self = self(),
            case Subscriber#subscriber.data_handler of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    Pid ! {mqtt, data, Self, Topic, Data};
                Fun when is_function(Fun) ->
                    spawn(fun() -> Fun(Self, Topic, Data) end)
            end
    end,
    {noreply, State};
handle_info(Info, State) ->
    io:format("Unexpected INFO message: ~p~n", [Info]),
    {noreply, State}.

%% @hidden
terminate(Reason, State) ->
    io:format("mqtt gen_server process ~p terminated with reason ~p.  State: ~p~n", [self(), Reason, State]),
    do_stop(State#state.port),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal operations
%%

%% @private
do_publish(Port, Topic, Message, Qos, Retain) ->
    call(Port, {publish, Topic, Message, qos_to_int(Qos), Retain}).

%% @private
do_subscribe(Port, Topic, Qos) ->
    call(Port, {subscribe, Topic, qos_to_int(Qos)}).

%% @private
do_unsubscribe(Port, Topic) ->
    call(Port, {unsubscribe, Topic}).

%% @private
do_stop(Port) ->
    do_disconnect(Port),
    Port ! stop.

%% @private
do_disconnect(Port) ->
    call(Port, disconnect).

%% @private
do_reconnect(Port) ->
    call(Port, reconnect).

%% @private
call(Port, Msg) ->
    Ref = make_ref(),
    Port ! {self(), Ref, Msg},
    receive
        {Ref, Ret} ->
            Ret
    end.

validate_publish_options(Options) ->
    validate_function_or_pid_or_undefined(published_handler, Options),
    validate_qos_or_undefined(qos, Options),
    validate_is_boolean_or_undefined(retain, Options),
    Options.

validate_subscribe_options(Options) ->
    validate_function_or_pid_or_undefined(subscribed_handler, Options),
    validate_function_or_pid_or_undefined(data_handler, Options),
    validate_qos_or_undefined(qos, Options),
    Options.

validate_unsubscribe_options(Options) ->
    validate_function_or_pid_or_undefined(unsubscribed_handler, Options),
    Options.

validate_function_or_pid_or_undefined(Key, Options) ->
    case maps:get(Key, Options, undefined) of
        undefined -> ok;
        Value when is_function(Value) orelse is_pid(Value) -> ok;
        _ ->
            throw(badarg)
    end.

validate_qos_or_undefined(Key, Options) ->
    case maps:get(Key, Options, undefined) of
        undefined -> ok;
        at_most_once -> ok;
        at_least_once -> ok;
        exactly_once -> ok;
        _ ->
            throw(badarg)
    end.

qos_to_int(Qos) ->
    case Qos of
        at_most_once -> 0;
        at_least_once -> 1;
        exactly_once -> 2
    end.

validate_is_boolean_or_undefined(Key, Options) ->
    case maps:get(Key, Options, undefined) of
        undefined -> ok;
        true -> ok;
        false -> ok;
        _ ->
            throw(badarg)
    end.
