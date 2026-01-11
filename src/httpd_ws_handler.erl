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
-module(httpd_ws_handler).

%% internal API only called by httpd_ws_handler
-export([start/3, stop/1, handle_web_socket_message/2]).
%% API used by implementors of the httpd_ws_handler behavior
-export([send/2]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").


-type websocket() :: term().

-export_type([websocket/0]).

%%
%% httpd_ws_handler behavior
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
    handler_state,
    frame_buffer = <<>>  %% Buffer for incomplete WebSocket frames
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
        handler_state = HandlerState,
        frame_buffer = Buffer
    } = State,
    ?TRACE("WebSocket received packet ~p", [Packet]),
    
    %% Accumulate packet data into buffer
    NewBuffer = <<Buffer/binary, Packet/binary>>,
    
    case parse_frame(NewBuffer) of
        {ok, PayloadData, Remaining} ->
            ?TRACE("HandlerModule ~p; PayloadData ~p; Remaining ~p bytes", [HandlerModule, PayloadData, byte_size(Remaining)]),
            case HandlerModule:handle_ws_message(PayloadData, HandlerState) of
                {reply, Reply, NewHandlerState} ->
                    ?TRACE("Handled WS payload.  NewHandlerState: ~p", [NewHandlerState]),
                    do_send(Socket, Reply, text),
                    {noreply, State#state{handler_state = NewHandlerState, frame_buffer = Remaining}};
                {noreply, NewHandlerState} ->
                    ?TRACE("Handled WS payload.  NewHandlerState: ~p", [NewHandlerState]),
                    {noreply, State#state{handler_state = NewHandlerState, frame_buffer = Remaining}};
                HandleModleError ->
                    ?TRACE("HandleModleError: ~p", [HandleModleError]),
                    socket:close(Socket),
                    {stop, HandleModleError, State}
            end;
        incomplete ->
            ?TRACE("Incomplete frame, buffering ~p bytes", [byte_size(NewBuffer)]),
            {noreply, State#state{frame_buffer = NewBuffer}};
        empty_payload ->
            ?TRACE("Empty payload.", []),
            {noreply, State#state{frame_buffer = <<>>}};
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
parse_frame(Packet) when byte_size(Packet) < 2 ->
    incomplete;
parse_frame(Packet) ->
    try
        <<_FinOpcode:8, MaskLen:8, Rest/binary>> = Packet,
        Mask = (MaskLen band 16#80) bsr 7,
        PayloadLen = MaskLen band 16#7F,
        
        %% Calculate how many bytes we need for the complete frame
        {ActualPayloadLen, HeaderSize} = case PayloadLen of
            126 ->
                case byte_size(Rest) >= 2 of
                    true ->
                        <<Len:16, _/binary>> = Rest,
                        {Len, 2};
                    false ->
                        {need_more, 2}
                end;
            127 ->
                case byte_size(Rest) >= 8 of
                    true ->
                        <<Len:64, _/binary>> = Rest,
                        {Len, 8};
                    false ->
                        {need_more, 8}
                end;
            Len ->
                {Len, 0}
        end,
        
        case ActualPayloadLen of
            need_more ->
                incomplete;
            _ ->
                MaskSize = case Mask of 1 -> 4; _ -> 0 end,
                RequiredBytes = HeaderSize + MaskSize + ActualPayloadLen,
                
                case byte_size(Rest) >= RequiredBytes of
                    true ->
                        %% We have enough data to parse the complete frame
                        parse_complete_frame(PayloadLen, Mask, Rest);
                    false ->
                        ?TRACE("Incomplete frame: have ~p bytes, need ~p", [byte_size(Packet), 2 + RequiredBytes]),
                        incomplete
                end
        end
    catch
        _:Error ->
            ?TRACE("Error in parse_frame: ~p", [Error]),
            {error, Error}
    end.

%% @private
parse_complete_frame(PayloadLen, Mask, Rest) ->
    case PayloadLen of
        0 ->
            {ok, <<"">>, Rest};
        126 ->
            <<MediumPayloadLen:16, Rest2/binary>> = Rest,
            extract_payload(Mask, MediumPayloadLen, Rest2);
        127 ->
            <<LargePayloadLen:64, Rest2/binary>> = Rest,
            extract_payload(Mask, LargePayloadLen, Rest2);
        _ ->
            extract_payload(Mask, PayloadLen, Rest)
    end.

%% @private
extract_payload(Mask, PayloadLen, Data) ->
    case Mask of
        1 ->
            <<MaskingKey:4/binary, MaskedPayload:PayloadLen/binary, Remaining/binary>> = Data,
            ?TRACE("MaskingKey: ~p, MaskedPayload length: ~p", [MaskingKey, byte_size(MaskedPayload)]),
            {ok, unmask(MaskingKey, MaskedPayload), Remaining};
        _ ->
            <<Payload:PayloadLen/binary, Remaining/binary>> = Data,
            {ok, Payload, Remaining}
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
