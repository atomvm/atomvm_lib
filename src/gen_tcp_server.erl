%%
%% Copyright (c) 2023 dushin.net
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

-module(gen_tcp_server).

-export([start/1, start_link/1, stop/1]).

%% internal APIs
-export([get_recv_timeout_ms/1, set_recv_timeout_ms/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type config() :: #{
    bind_address => socket:sockaddr(),
    socket_options => [socket:socket_option()],
    handler => {
        Module :: module(),
        Args :: term()
    },
    recv_timeout_ms => non_neg_integer() | infinity,
    recv_len => non_neg_integer()
}.
-opaque gen_tcp_server() :: pid().

-export_type([
    config/0,
    gen_tcp_server/0
]).

%%
%% gen_tcp_server behavior
%%

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_receive(Socket :: term(), Packet :: binary(), State :: term()) ->
    {reply, Packet :: iolist(), NewState :: term()} | {noreply, NewState :: term()} | {close, Packet :: iolist()} | close.

-callback handle_tcp_closed(Socket :: term(), State :: term()) -> ok.

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_BIND_OPTIONS, #{
    family => inet,
    addr => any
}).
-define(DEFAULT_SOCKET_OPTIONS, #{}).
-define(DEFAULT_RECV_TIMEOUT_MS, infinity).
-define(DEFAULT_RECV_LEN, 0).

%%
%% API
%%

-spec start_link(Config :: config()) -> {ok, gen_tcp_server()} | {error, Reason :: term()}.
start_link(Config) ->
    internal_start(Config, fun gen_server:start_link/3).

-spec start(Config :: config()) -> {ok, gen_tcp_server()} | {error, Reason :: term()}.
start(Config) ->
    internal_start(Config, fun gen_server:start/3).

%% @private
internal_start(Config, StartFun) ->
    StartFun(?MODULE, Config, []).

stop(Server) ->
    gen_server:stop(Server).

get_recv_timeout_ms(Server) ->
    gen_server:call(Server, get_recv_timeout_ms).

set_recv_timeout_ms(Server, RecvTimeoutMs) ->
    gen_server:call(Server, {set_recv_timeout_ms, RecvTimeoutMs}).

%%
%% gen_server implementation
%%

-record(state, {
    config,
    handler,
    handler_state,
    recv_timeout_ms
}).

%% @hidden
init(Config) ->
    Self = self(),
    case socket:open(inet, stream, tcp) of
        {ok, Socket} ->
            SocketOptions = maps:get(socket_options, Config, ?DEFAULT_SOCKET_OPTIONS),
            ok = set_socket_options(Socket, SocketOptions),
            BindAddress = maps:merge(?DEFAULT_BIND_OPTIONS, maps:get(bind_address, Config, #{})),
            case socket:bind(Socket, BindAddress) of
                ok ->
                    case socket:listen(Socket) of
                        ok ->
                            RecvLen = maps:get(recv_len, Config, ?DEFAULT_RECV_LEN),
                            RecvTimeoutMs = maps:get(recv_timeout_ms, Config, ?DEFAULT_RECV_TIMEOUT_MS),
                            %% TODO case match
                            {Handler, HandlerArgs} = maps:get(handler, Config),
                            spawn_opt(fun() -> accept(Self, Socket, {Handler, HandlerArgs}, {RecvLen, RecvTimeoutMs}) end, [link]),
                            % case Handler:init(HandlerArgs) of
                            %     {ok, HandlerState} ->
                            %         {ok, #state{
                            %             config = Config,
                            %             handler = Handler,
                            %             handler_state = HandlerState
                            %         }};
                            %     HandlerError ->
                            %         try_close(Socket),
                            %         {stop, {handler_error, HandlerError}}
                            % end;
                            {ok, #state{config = Config}};
                        ListenError ->
                            try_close(Socket),
                            {stop, {listen_error, ListenError}}
                        end;
                BindError ->
                    try_close(Socket),
                    {stop, {bind_error, BindError}}
            end;
        OpenError ->
            {stop, {open_error, OpenError}}
    end.

%% @hidden
handle_call(get_recv_timeout_ms, _From, State) ->
    io:format("wtf ~p~n", [State#state.recv_timeout_ms]),
    {reply, State#state.recv_timeout_ms, State};
handle_call({set_recv_timeout_ms, RecvTimeoutMs}, _From, State) ->
    {reply, ok, State#state{recv_timeout_ms = RecvTimeoutMs}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
% handle_info({tcp_closed, Socket}, State) ->
%     ?LOG_DEBUG("TCP Socket closed ~p", [Socket]),
%     #state{handler=Handler, handler_state=HandlerState} = State,
%     NewHandlerState = Handler:handle_tcp_closed(Socket, HandlerState),
%     {noreply, State#state{handler_state=NewHandlerState}};
% handle_info({tcp, Socket, Packet}, State) ->
%     #state{handler=Handler, handler_state=HandlerState} = State,
%     ?LOG_DEBUG("received packet: len(~p) from ~p", [erlang:byte_size(Packet), socket:peername(Socket)]),
%     case Handler:handle_receive(Socket, Packet, HandlerState) of
%         {reply, ResponsePacket, ResponseState} ->
%             ?LOG_DEBUG("Sending reply to endpoint ~p", [socket:peername(Socket)]),
%             try_send(Socket, ResponsePacket),
%             {noreply, State#state{handler_state=ResponseState}};
%         {noreply, ResponseState} ->
%             ?LOG_DEBUG("no reply", []),
%             {noreply, State#state{handler_state=ResponseState}};
%         {close, ResponsePacket} ->
%             ?LOG_DEBUG("Sending reply to endpoint ~p and closing socket: ~p", [socket:peername(Socket), Socket]),
%             try_send(Socket, ResponsePacket),
%             % timer:sleep(500),
%             try_close(Socket),
%             {noreply, State};
%         close ->
%             ?LOG_DEBUG("Closing socket ~p", [Socket]),
%             try_close(Socket),
%             {noreply, State};
%         SomethingElse ->
%             ?LOG_ERROR("Unexpected response from handler ~p: ~p", [Handler, SomethingElse]),
%             try_close(Socket),
%             {noreply, State}
%     end;
handle_info(Info, State) ->
    ?LOG_WARNING("Received spurious info msg: ~p", [Info]),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%
%% internal functions
%%

%% @private
try_send(Socket, Packet) when is_binary(Packet) ->
    ?LOG_DEBUG(
        "Trying to send binary packet data to socket ~p.  Packet (or len): ~p", [
        Socket, case byte_size(Packet) < 32 of true -> Packet; _ -> byte_size(Packet) end
    ]),
    case socket:send(Socket, Packet) of
        ok ->
            ?LOG_DEBUG("sent.", []),
            ok;
        {ok, Rest} ->
            ?LOG_DEBUG("sent.  remaining: ~p", [Rest]),
            try_send(Socket, Rest);
        Error ->
            ?LOG_ERROR("Send failed due to error ~p", [Error])
    end;
try_send(Socket, Char) when is_integer(Char) ->
    %% TODO handle unicode
    ?LOG_DEBUG("Sending char ~p as ~p", [Char, <<Char:8>>]),
    try_send(Socket, <<Char:8>>);
try_send(Socket, List) when is_list(List) ->
    case is_string(List) of
        true ->
            try_send(Socket, list_to_binary(List));
        _ ->
            try_send_iolist(Socket, List)
    end.

try_send_iolist(_Socket, []) ->
    ok;
try_send_iolist(Socket, [H | T]) ->
    try_send(Socket, H),
    try_send_iolist(Socket, T).

is_string([]) ->
    true;
is_string([H | T]) when is_integer(H) ->
    is_string(T);
is_string(_) ->
    false.

%% @private
try_close(Socket) ->
    case socket:close(Socket) of
        ok ->
            ok;
        Error ->
            ?LOG_WARNING("Close failed due to error ~p", [Error])
    end.

%% @private
set_socket_options(Socket, SocketOptions) ->
    maps:fold(
        fun(Option, Value, Accum) ->
            erlang:display({setopt, Socket, Option, Value}),
            ok = socket:setopt(Socket, Option, Value),
            Accum
        end,
        ok,
        SocketOptions
    ).

-record(loop_state, {
    handler, handler_state
}).

%% @private
accept(ControllingProcess, ListenSocket, RecvLen, RecvTimeoutMs) ->
    ?LOG_DEBUG("pid ~p Waiting for connection on ~p ...", [self(), socket:sockname(ListenSocket)]),
    %% TODO add support for accept timeout
    case socket:accept(ListenSocket) of
        {ok, Connection} ->
            ?LOG_INFO("Accepted connection from ~p", [socket:peername(Connection)]),
            spawn_opt(fun() -> accept(ControllingProcess, ListenSocket, {Handler, HandlerArgs}, {RecvLen, RecvTimeoutMs}) end, [link]),
            start_loop(ControllingProcess, Connection, {Handler, HandlerArgs}, {RecvLen, RecvTimeoutMs});
        Error ->
            ?LOG_ERROR("Error accepting connection: ~p", [Error])
    end.

start_loop(ControllingProcess, Connection, {Handler, HandlerArgs}, {RecvLen, RecvTimeoutMs}) ->
    erlang:display({Handler, HandlerArgs}),
    case Handler:init(HandlerArgs) of
        {ok, HandlerState} ->
            LoopState = #loop_state{
                handler = Handler,
                handler_state = HandlerState
            },
            loop(ControllingProcess, Connection, LoopState, RecvLen, RecvTimeoutMs);
        _HandlerError ->
            try_close(Connection)
        end.

%% @private
loop(ControllingProcess, Connection, LoopState, RecvLen, RecvTimeoutMs) ->
    Handler = LoopState#loop_state.handler,
    HandlerState = LoopState#loop_state.handler_state,
    case socket:recv(Connection, RecvLen, RecvTimeoutMs) of
        {ok, Packet} ->
            ?LOG_DEBUG("Received data ~p on connection ~p", [Packet, Connection]),
            NewLoopState = process_packet(Connection, Packet, LoopState),
            loop(ControllingProcess, Connection, NewLoopState, RecvLen, RecvTimeoutMs);
        {error, closed} ->
            ?LOG_INFO("Peer closed connection ~p", [Connection]),
            _NewHandlerState = Handler:handle_tcp_closed(Connection, HandlerState),
            ok;
        {error, timeout} ->
            ?LOG_INFO("Client peer timed out on receive ~p", [Connection]),
            try_close(Connection),
            _NewHandlerState = Handler:handle_tcp_closed(Connection, HandlerState),
            ok;
        {error, timeout} ->
            ?LOG_INFO("Client peer timed out on receive ~p", [Connection]),
            try_close(Connection);
        {error, _SomethingElse} ->
            ?LOG_ERROR("Some other error occurred ~p", [Connection]),
            try_close(Connection)
    end.

process_packet(Socket, Packet, LoopState) ->
    #loop_state{handler=Handler, handler_state=HandlerState} = LoopState,
    ?LOG_DEBUG("received packet: len(~p) from ~p", [erlang:byte_size(Packet), socket:peername(Socket)]),
    case Handler:handle_receive(Socket, Packet, HandlerState) of
        {reply, ResponsePacket, ResponseState} ->
            ?LOG_DEBUG("Sending reply to endpoint ~p", [socket:peername(Socket)]),
            try_send(Socket, ResponsePacket),
            LoopState#loop_state{handler_state=ResponseState};
        {noreply, ResponseState} ->
            ?LOG_DEBUG("no reply", []),
            LoopState#loop_state{handler_state=ResponseState};
        {close, ResponsePacket} ->
            ?LOG_DEBUG("Sending reply to endpoint ~p and closing socket: ~p", [socket:peername(Socket), Socket]),
            try_send(Socket, ResponsePacket),
            % timer:sleep(500),
            try_close(Socket),
            LoopState;
        close ->
            ?LOG_DEBUG("Closing socket ~p", [Socket]),
            try_close(Socket),
            LoopState;
        SomethingElse ->
            ?LOG_ERROR("Unexpected response from handler ~p: ~p", [Handler, SomethingElse]),
            try_close(Socket),
            LoopState
    end.
