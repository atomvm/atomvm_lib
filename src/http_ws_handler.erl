%%
%% Copyright (c) dushin.net
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
-module(http_ws_handler).

%% internal API only called by http_ws_handler
-export([start/3, stop/1, handle_web_socket_message/2]).
%% API used by implementors of the http_ws_handler behavior
-export([send/2]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").


-type websocket() :: term().

-export_type([websocket/0]).

%%
%% http_ws_handler behavior
%%

-callback handle_ws_init(WebSocket :: websocket(), Path :: httpd:path(), Args :: term()) ->
    {ok, State :: term()} |
    term().

-callback handle_ws_message(PayloadData :: binary(), State :: term()) ->
    {reply, Reply :: iolist(), NewState :: term()} |
    {noreply, NewState :: term()} |
    {close, Reply :: iolist(), NewState :: term()} |
    {close, NewState :: term()} |
    term().

%%
%% API
%%

%% @hidden
start(Socket, Path, Config) ->
    gen_server:start(?MODULE, {Socket, Path, Config}, []).

%% @hidden
stop(WebSocket) ->
    gen_server:stop(WebSocket).


%% @hidden
handle_web_socket_message(WebSocket, Packet) ->
    gen_server:cast(WebSocket, {message, Packet}).

send(WebSocket, Packet) ->
    case self() of
        WebSocket ->
            throw(badarg);
        _ ->
            gen_server:call(WebSocket, {send, Packet})
    end.


%%
%% gen_server implementation
%%

-record(state, {
    socket,
    handler_module,
    handler_state
}).

%% @hidden
init({Socket, Path, Config}) ->
    ?TRACE("Started WebSocket using socket ~p with Config ~p", [Socket, Config]),
    case maps:get(module, Config, undefined) of
        undefined ->
            {stop, bad_config};
        HandlerModule ->
            case HandlerModule:handle_ws_init(self(), Path, maps:get(args, Config, undefined)) of
                {ok, HandlerState} ->
                    {ok, #state{socket=Socket, handler_module=HandlerModule, handler_state=HandlerState}};
                Error ->
                    {stop, Error}
            end
    end.

%% @hidden
handle_cast({message, Packet}, State) ->
    #state{
        socket = Socket,
        handler_module = HandlerModule,
        handler_state = HandlerState
    } = State,
    ?TRACE("WebSocket received packet ~p", [Packet]),
    case parse_frame(Packet) of
        {ok, PayloadData} ->
            ?TRACE("HandlerModule ~p; PayloadData ~p", [HandlerModule, PayloadData]),
            case HandlerModule:handle_ws_message(PayloadData, HandlerState) of
                {reply, Reply, NewHandlerState} ->
                    ?TRACE("Handled WS payload.  NewHandlerState: ~p", [NewHandlerState]),
                    do_send(Socket, Reply, text),
                    {noreply, State#state{handler_state = NewHandlerState}};
                {noreply, NewHandlerState} ->
                    ?TRACE("Handled WS payload.  NewHandlerState: ~p", [NewHandlerState]),
                    {noreply, State#state{handler_state = NewHandlerState}};
                HandleModleError ->
                    ?TRACE("HandleModleError: ~p", [HandleModleError]),
                    socket:close(Socket),
                    {stop, HandleModleError, State}
            end;
        empty_payload ->
            ?TRACE("Empty payload.", []),
            {noreply, State};
        ParseFrameError ->
            ?TRACE("ParseFrameError: ~p", [ParseFrameError]),
            socket:close(Socket),
            {stop, ParseFrameError, State}
    end.

%% @hidden
handle_call({send, Packet}, _From, State) ->
    ?TRACE("Sending packet ~p", [Packet]),
    Reply = do_send(State#state.socket, Packet, text),
    {reply, Reply, State}.

%% @hidden
handle_info(_Msg, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.


%%
%% internal implementation
%%

%% @private
parse_frame(<<0,0,0,0,0,0,0,0,0,0>>) ->
    empty_payload;
parse_frame(Packet) ->
    try
        <<FinOpcode:8, MaskLen:8, Rest/binary>> = Packet,
        Fin = (FinOpcode band 16#80) bsr 7,
        Opcode = FinOpcode band 16#0F,
        Mask = (MaskLen band 16#80) bsr 7,
        PayloadLen = MaskLen band 16#7F,
        % <<Fin:1, _Reserved:3, Opcode:4, Mask:1, PayloadLen:7, Rest/binary>> = Packet,
        % ?TRACE("FinOpcode: ~p, Fin: ~p, Opcode: ~p, MaskLen: ~p, Mask: ~p, PayloadLen: ~p, Rest: ~p", [FinOpcode, Fin, Opcode, MaskLen, Mask, PayloadLen, Rest]),
        ?TRACE("Fin: ~p, Opcode: ~p, Mask: ~p, PayloadLen: ~p, Rest: ~p", [Fin, Opcode, Mask, PayloadLen, Rest]),
        case PayloadLen of
            0 ->
                {ok, <<"">>};
            126 ->
                case Mask of
                    1 ->
                        <<MediumPayloadLen:16, Rest2/binary>> = Rest,
                        <<MaskingKey:4/binary, MaskedPayload:MediumPayloadLen/binary>> = Rest2,
                        ?TRACE("MaskingKey: ~p, MaskedPayload: ~p", [MaskingKey, MaskedPayload]),
                        {ok, unmask(MaskingKey, MaskedPayload)};
                    _ ->
                        <<MediumPayloadLen:16, Rest2/binary>> = Rest,
                        {ok, <<Rest2:MediumPayloadLen/binary>>}
                end;
            127 ->
                case Mask of
                    1 ->
                        <<LargePayloadLen:64, Rest2/binary>> = Rest,
                        <<MaskingKey:4/binary, MaskedPayload:LargePayloadLen/binary>> = Rest2,
                        ?TRACE("MaskingKey: ~p, MaskedPayload: ~p", [MaskingKey, MaskedPayload]),
                        {ok, unmask(MaskingKey, MaskedPayload)};
                    _ ->
                        <<MediumPayloadLen:16, Rest2/binary>> = Rest,
                        {ok, <<Rest2:MediumPayloadLen/binary>>}
                end;
            _ ->
                case Mask of
                    1 ->
                        <<MaskingKey:4/binary, MaskedPayload:PayloadLen/binary>> = Rest,
                        ?TRACE("MaskingKey: ~p, MaskedPayload: ~p", [MaskingKey, MaskedPayload]),
                        {ok, unmask(MaskingKey, MaskedPayload)};
                    _ ->
                        {ok, Rest}
                end
        end
    catch
        _:Error ->
            ?TRACE("Error in parse_frame: ~p", [Error]),
            {error, Error}
    end.

%% @private
unmask(MaskingKey, MaskedPayload) ->
    unmask(MaskingKey, MaskedPayload, 0, []).

unmask(_MaskingKey, <<"">>, _I, Accum) ->
    % ?TRACE("unmasked Accum: ~p", [Accum]),
    list_to_binary(lists:reverse(Accum));
unmask(MaskingKey, <<H:8, T/binary>>, I, Accum) ->
    MaskingOctet = octet(MaskingKey, I rem 4),
    % ?TRACE("H: ~p, MaskingOctet: ~p", [H, MaskingOctet]),
    unmask(MaskingKey, T, I + 1, [MaskingOctet bxor H | Accum]).

%% @private
octet(<<First:8, _/binary>>, 0) ->
    First;
octet(<<_:1/binary, Second:8, _/binary>>, 1) ->
    Second;
octet(<<_:2/binary, Third:8, _/binary>>, 2) ->
    Third;
octet(<<_:3/binary, Fourth:8, _/binary>>, 3) ->
    Fourth.

%% @private
do_send(Socket, Packet, Mode) ->
    FramedPacket = frame(Packet, Mode),
    ?TRACE("Framed packet: [~s]", [atomvm_lib:to_hex(FramedPacket)]),
    socket:send(Socket, FramedPacket).

%% @private
frame(Packet, Mode) when is_list(Packet) ->
    frame(iolist_to_binary(Packet), Mode);
frame(Packet, Mode) when is_binary(Packet) ->
    Fin = 16#80,
    Opcode = case Mode of text -> 16#01; binary -> 16#02; _ -> 16#01 end,
    FinOpcode = Fin bor Opcode,
    PayloadLen = erlang:byte_size(Packet),
    case {PayloadLen =< 125, PayloadLen =< 65536} of
        {true, _} ->
            NoMask = 16#7F,
            MaskLen = NoMask band PayloadLen,
            <<FinOpcode:8, MaskLen:8, Packet/binary>>;
        {false, true} ->
            NoMask = 16#7F,
            MaskLen = NoMask band 126,
            <<FinOpcode:8, MaskLen:8, PayloadLen:16/unsigned, Packet/binary>>;
        {false, false} ->
            NoMask = 16#7F,
            MaskLen = NoMask band 127,
            <<FinOpcode:8, MaskLen:8, PayloadLen:64/unsigned, Packet/binary>>
    end.
