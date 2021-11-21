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
-module(mqtt_client_example).

-export([start/0]).

start() ->
    %%
    %% Start the network
    %%
    ok = start_network(<<"my-ssid">>, <<"my-psk">>),
    %%
    %% Start the MQTT client.
    %%
    Config = #{
        url => "mqtt://mqtt.eclipseprojects.io",
        connected_handler => fun handle_connected/1
    },
    {ok, _MQTT} = mqtt_client:start(Config),
    io:format("MQTT started.~n"),

    loop_forever().

loop_forever() ->
    receive
        halt -> halt
    end.

%%
%% connected callback.  This function will be called
%%
handle_connected(MQTT) ->
    Config = mqtt_client:get_config(MQTT),
    Topic = <<"atomvm/qos0">>,
    io:format("Connected to ~p~n", [maps:get(url, Config)]),
    io:format("Subscribing to ~p...~n", [Topic]),
    ok = mqtt_client:subscribe(MQTT, Topic, #{
        subscribed_handler => fun handle_subscribed/2,
        data_handler => fun handle_data/3
    }).

handle_subscribed(MQTT, Topic) ->
    io:format("Subscribed to ~p.~n", [Topic]),
    io:format("Spawning publish loop on topic ~p~n", [Topic]),
    spawn(fun() -> publish_loop(MQTT, Topic, 1) end).

handle_data(_MQTT, Topic, Data) ->
    io:format("Received data on topic ~p: ~p ~n", [Topic, Data]),
    % io:format("Pending publishes: ~p~n", [mqtt_client:get_pending_publishes(MQTT)]),
    % io:format("Pending subscriptions: ~p~n", [mqtt_client:get_pending_subscriptions(MQTT)]),
    % io:format("Pending unsubscriptions: ~p~n", [mqtt_client:get_pending_unsubscriptions(MQTT)]),
    io:format("process count: ~p~n", [erlang:system_info(process_count)]),
    io:format("Free heap on handle_data: ~p~n", [erlang:system_info(esp32_free_heap_size)]),
    ok.

start_network(SSID, Psk) ->
    case network_fsm:wait_for_sta([{ssid, SSID}, {psk, Psk}]) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~s Netmask: ~s Gateway: ~s~n",
                [Address, Netmask, Gateway]
            ),
            ok;
        Error ->
            throw({unable_to_start_network, Error})
    end.

publish_loop(MQTT, Topic, Seq) ->
    io:format("Publishing data on topic ~p~n", [Topic]),
    _ = mqtt_client:publish(MQTT, Topic, list_to_binary("echo" ++ integer_to_list(Seq))),
    timer:sleep(5000),
    io:format("process count: ~p~n", [erlang:system_info(process_count)]),
    io:format("Free heap after publish: ~p~n", [erlang:system_info(esp32_free_heap_size)]),
    publish_loop(MQTT, Topic, Seq + 1).
