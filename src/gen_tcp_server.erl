%%
%% Copyright (c) 2022 dushin.net
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

-export([start/4, start/3, start_link/4, stop/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%
%% gen_tcp_server behavior
%%

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_receive(Socket :: term(), Packet :: binary(), State :: term()) ->
    {reply, Packet :: iolist(), NewState :: term()} | {noreply, NewState :: term()} | {close, Packet :: iolist()} | close.

-callback handle_tcp_closed(Socket :: term(), State :: term()) -> ok.

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-record(state, {
    handler,
    handler_state
}).

%%
%% API
%%

start(Port, SocketOptions, Handler, Args) ->
    gen_server:start(?MODULE, {Port, SocketOptions, Handler, Args}, []).

start(Socket, Handler, Args) ->
    gen_server:start(?MODULE, {Socket, Handler, Args}, []).

start_link(Port, SocketOptions, Handler, Args) ->
    gen_server:start_link(?MODULE, {Port, SocketOptions, Handler, Args}, []).

stop(Server) ->
    gen_server:stop(Server).

%%
%% gen_server implementation
%%

%% @hidden
init({Port, _SocketOptions, Handler, Args}) ->
    Self = self(),
    case socket:open(inet, stream) of
        {ok, Socket} ->
            case socket:bind(Socket, #{family => inet, addr => any, port => Port}) of
                ok ->
                    case socket:listen(Socket) of
                        ok ->
                            spawn(fun() -> accept(Self, Socket) end),
                            case Handler:init(Args) of
                                {ok, HandlerState} ->
                                    {ok, #state{handler = Handler, handler_state = HandlerState}};
                                HandlerError ->
                                    {stop, {handler_error, HandlerError}}
                            end;
                        ListenError ->
                            {stop, {listen_error, ListenError}}
                        end;
                BindError ->
                    {stop, {bind_error, BindError}}
            end;
        OpenError ->
            {stop, {open_error, OpenError}}
    end;
init({Socket, Handler, Args}) ->
    Self = self(),
    case Handler:init(Args) of
        {ok, HandlerState} ->
            spawn(fun() -> loop(Self, Socket) end),
            {ok, #state{handler = Handler, handler_state = HandlerState}};
        HandlerError ->
            {stop, {handler_error, HandlerError}}
    end.

%% @hidden
handle_call(_From, _Request, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({tcp_closed, Socket}, State) ->
    ?TRACE("TCP Socket closed ~p", [Socket]),
    #state{handler=Handler, handler_state=HandlerState} = State,
    NewHandlerState = Handler:handle_tcp_closed(Socket, HandlerState),
    {noreply, State#state{handler_state=NewHandlerState}};
handle_info({tcp, Socket, Packet}, State) ->
    #state{handler=Handler, handler_state=HandlerState} = State,
    ?TRACE("received packet: len(~p) from ~p", [erlang:byte_size(Packet), socket:peername(Socket)]),
    case Handler:handle_receive(Socket, Packet, HandlerState) of
        {reply, ResponsePacket, ResponseState} ->
            ?TRACE("Sending reply to endpoint ~p", [socket:peername(Socket)]),
            socket:send(Socket, ResponsePacket),
            {noreply, State#state{handler_state=ResponseState}};
        {noreply, ResponseState} ->
            ?TRACE("no reply", []),
            {noreply, State#state{handler_state=ResponseState}};
        {close, ResponsePacket} ->
            ?TRACE("Sending reply to endpoint ~p and closing socket: ~p", [socket:peername(Socket), Socket]),
            socket:send(Socket, ResponsePacket),
            % timer:sleep(500),
            socket:close(Socket),
            {noreply, State};
        close  ->
            ?TRACE("Closing socket ~p", [Socket]),
            socket:close(Socket),
            {noreply, State};
        SomethingElse ->
            ?TRACE("Unexpected response from handler ~p: ~p", [Handler, SomethingElse]),
            socket:close(Socket),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%
%% internal functions
%%

%% @private
accept(ControllingProcess, ListenSocket) ->
    ?TRACE("pid ~p Waiting for connection on ~p ...", [self(), socket:sockname(ListenSocket)]),
    case socket:accept(ListenSocket) of
        {ok, Connection} ->
            ?TRACE("Accepted connection from ~p", [socket:peername(Connection)]),
            spawn(fun() -> accept(ControllingProcess, ListenSocket) end),
            loop(ControllingProcess, Connection);
        Error ->
            ?TRACE("Error accepting connection: ~p", [Error])
    end.


%% @private
loop(ControllingProcess, Connection) ->
    case socket:recv(Connection) of
        {ok, Data} ->
            ?TRACE("Received data ~p on connection ~p", [Data, Connection]),
            ControllingProcess ! {tcp, Connection, Data},
            loop(ControllingProcess, Connection);
        {error, closed} ->
            ?TRACE("Peer closed connection ~p", [Connection]),
            ControllingProcess ! {tcp_closed, Connection},
            ok;
        {error, _SomethingElse} ->
            ?TRACE("Some other error occurred ~p", [Connection]),
            socket:close(Connection)
    end.
