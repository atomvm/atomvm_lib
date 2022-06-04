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

-module(httpd).

-export([start/2, start_link/2, stop/1]).

-behaviour(gen_tcp_server).
-export([init/1, handle_receive/3, handle_tcp_closed/2]).

-include("httpd.hrl").

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-type method() :: get | post | put | delete.
-type content_type() :: string().
-type path() :: list(binary()).
-type query_params() :: #{
    binary() := binary()
}.
-type http_request() :: #{
    method := method(),
    path := path(),
    uri := string(),
    query_params := query_params(),
    headers := #{binary() := binary()},
    body := binary(),
    socket := term()
}.
-type handler_config() :: #{
    module := module(),
    module_config := term()
}.
-type config() :: [{path(), handler_config()}].
-type portnum() :: 0..65536.

-export_type([method/0, path/0, http_request/0, query_params/0]).

%%
%% Handle an HTTP request.
%%
-callback handle_http_req(Method :: method(), PathSuffix :: path(), HttpRequest :: http_request(), HandlerConfig :: handler_config()) ->
    ok | {ok, {ContentType :: content_type(), Reply :: term()}} | {ok, Reply :: term()} |
    close | {close, {ContentType :: content_type(), Reply :: term()}} | {close, Reply :: term()} |
    not_found | bad_request | internal_server_error |
    term().

-record(state, {
    config,
    ws_socket_map = #{}
}).

%%
%% API
%%

-spec start(Port :: portnum(), Config :: config()) -> {ok, HTTPD :: pid()} | {error, Reason :: term()}.
start(Port, Config) ->
    gen_tcp_server:start(Port, [{binary, true}, {buffer, 1024}], ?MODULE, Config).

-spec start_link(Port :: portnum(), Config :: config()) -> {ok, HTTPD :: pid()} | {error, Reason :: term()}.
start_link(Port, Config) ->
    gen_tcp_server:start_link(Port, [{binary, true}, {buffer, 1024}], ?MODULE, Config).

stop(Httpd) ->
    gen_tcp_server:stop(Httpd).

%%
%% gen_tcp_server implementation
%%

%% @hidden
init(Config) ->
    {ok, #state{config = Config}}.

%% @hidden
handle_receive(Socket, Packet, State) ->
    case maps:get(Socket, State#state.ws_socket_map, undefined) of
        undefined ->
            handle_http_request(Socket, Packet, State);
        WebSocket ->
            case http_ws_handler:handle_web_socket_message(WebSocket, Packet) of
                ok ->
                    {noreply, State};
                Error ->
                    {close, create_error(?INTERNAL_SERVER_ERROR, Error, Socket)}
            end
    end.


handle_http_request(Socket, Packet, State) ->
    Config = State#state.config,
    try
        HttpRequest = parse_http_request(binary_to_list(Packet)),
        ?TRACE("HttpRequest: ~p", [HttpRequest]),
        Path = maps:get(path, HttpRequest),
        case get_handler(Path, Config) of
            {ok, PathSuffix, EntryConfig} ->
                Handler = maps:get(handler, EntryConfig),
                HandlerConfig = maps:get(handler_config, EntryConfig, #{}),
                Method = maps:get(method, HttpRequest),
                ?TRACE("Method: ~p Handler: ~p HandlerConfig: ~p", [Method, Handler, HandlerConfig]),
                case get_protocol(Method, maps:get(headers, HttpRequest)) of
                    http ->
                        case Handler:handle_http_req(Method, PathSuffix, HttpRequest#{socket => Socket}, HandlerConfig) of
                            ok ->
                                {noreply, State};
                            {ok, {ContentType, Reply}} ->
                                {reply, create_reply(?OK, ContentType, Reply, Socket), State};
                            {ok, Reply} ->
                                {reply, create_reply(?OK, "application/octet-stream", Reply, Socket), State};
                            close ->
                                {close, State};
                            {close, {ContentType, Reply}} ->
                                {close, create_reply(?OK, ContentType, Reply, Socket)};
                            {close, Reply} ->
                                {close, create_reply(?OK, "application/octet-stream", Reply, Socket)};
                            not_found ->
                                {close, create_error(?NOT_FOUND, not_found, Socket)};
                            bad_request ->
                                {close, create_error(?BAD_REQUEST, bad_request, Socket)};
                            internal_server_error ->
                                {close, create_error(?INTERNAL_SERVER_ERROR, internal_server_error, Socket)};
                            HandlerError ->
                                {close, create_error(?INTERNAL_SERVER_ERROR, HandlerError, Socket)}
                        end;
                    ws ->
                        case http_ws_handler:start(Socket, PathSuffix, HandlerConfig) of
                            {ok, WebSocket} ->
                                NewWebSocketMap = maps:put(Socket, WebSocket, State#state.ws_socket_map),
                                NewState = State#state{ws_socket_map = NewWebSocketMap},
                                ReplyToken = get_reply_token(maps:get(headers, HttpRequest)),
                                ReplyHeaders = #{"Upgrade" => "websocket", "Connection" => "Upgrade", "Sec-WebSocket-Accept" => ReplyToken},
                                {reply, create_reply(?SWITCHING_PROTOCOLS, ReplyHeaders, <<"">>, Socket), NewState};
                            Error ->
                                {close, create_error(?INTERNAL_SERVER_ERROR, {web_socket_error, Error}, Socket)}
                        end
                end;
             {error, no_handler} ->
                ?TRACE("No handler found for Path: ~p, Config: ~p", [Path, Config]),
                {close, create_error(?NOT_FOUND, no_handler, Socket)}
        end
    catch
        A:E ->
            io:format("Caught error: ~p:~p~n", [A, E]),
            {close, create_error(?BAD_REQUEST, E, Socket)}
    end.

%% @hidden
handle_tcp_closed(Socket, State) ->
    case maps:get(Socket, State#state.ws_socket_map, undefined) of
        undefined ->
            State;
        WebSocket ->
            ok = http_ws_handler:stop(WebSocket),
            NewWebSocketMap = maps:remove(Socket, State#state.ws_socket_map),
            State#state{ws_socket_map = NewWebSocketMap}
    end.

%%
%% Internal functions
%%

%% @private
get_reply_token(Headers) ->
    #{<<"Sec-WebSocket-Key">> := WebSocketKey} = Headers,
    MagicKey = <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>,
    PreImage = <<WebSocketKey/binary, MagicKey/binary>>,
    ReplyToken = base64:encode(atomvm_lib:sha1(PreImage)),
    ?TRACE("ReplyToken: ~p", [ReplyToken]),
    ReplyToken.

%% @private
parse_http_request(Packet) ->
    {Heading, HeadingRest} = parse_heading(Packet, start, [], #{}),
    {Headers, Body} = parse_header(HeadingRest, #{}),
    maps:merge(
        Heading,
        #{
            headers => Headers,
            body => Body
        }
    ).

%% @private
parse_heading([$\s|Rest], start, Tmp, Accum) ->
    parse_heading(Rest, start, Tmp, Accum);
parse_heading(Packet, start, Tmp, Accum) ->
    parse_heading(Packet, in_method, Tmp, Accum);
parse_heading([$\s|Rest], in_method, Tmp, Accum) ->
    Method = method_to_atom(string:to_upper(lists:reverse(Tmp))),
    parse_heading(Rest, wait_uri, [], Accum#{method => Method});
parse_heading([C|Rest], in_method, Tmp, Accum) ->
    parse_heading(Rest, in_method, [C|Tmp], Accum);
%% wait_uri state
parse_heading([$\s|Rest], wait_uri, Tmp, Accum) ->
    parse_heading(Rest, wait_uri, Tmp, Accum);
parse_heading(Packet, wait_uri, Tmp, Accum) ->
    parse_heading(Packet, in_uri, Tmp, Accum);
%% in_uri state
parse_heading([$\s|Rest], in_uri, Tmp, Accum) ->
    Uri = lists:reverse(Tmp),
    {Path, QueryParams} = normalize_uri(Uri),
    parse_heading(Rest, wait_version, [], Accum#{uri => Uri, path => Path, query_params => QueryParams});
parse_heading([C|Rest], in_uri, Tmp, Accum) ->
    parse_heading(Rest, in_uri, [C|Tmp], Accum);
%% wait_version state
parse_heading([$\s|Rest], wait_version, Tmp, Accum) ->
    parse_heading(Rest, wait_version, Tmp, Accum);
parse_heading(Packet, wait_version, Tmp, Accum) ->
    parse_heading(Packet, in_version, Tmp, Accum);
%% in_version state
parse_heading([$\n|Rest], in_version, _Tmp, Accum) ->
    {Accum, Rest};
parse_heading([C|Rest], in_version, Tmp, Accum) ->
    parse_heading(Rest, in_version, [C|Tmp], Accum);
%% error state
parse_heading(_Packet, _State, _Tmp, _Accum) ->
    throw(bad_heading).

%% @private
parse_header([$\r, $\n | Rest], Accum) ->
    {Accum, Rest};
parse_header([$\n | Rest], Accum) ->
    {Accum, Rest};
parse_header(Packet, Accum) ->
    {Line, Rest} = parse_line(Packet, []),
    {Key, Value} = split_header(Line),
    parse_header(Rest, Accum#{Key => Value}).

parse_line([$\r, $\n | Rest], Accum) ->
    {lists:reverse(Accum), Rest};
parse_line([$\n | Rest], Accum) ->
    {lists:reverse(Accum), Rest};
parse_line([C | Rest], Accum) ->
    parse_line(Rest, [C | Accum]);
parse_line(_Packet, _Accum) ->
    throw(bad_line).

%% @private
split_header(Header) ->
    [Key, Value] = string:split(Header, ":"),
    %% TODO to_lower the key
    {list_to_binary(string:trim(Key)), list_to_binary(string:trim(Value))}.

normalize_uri(Uri) ->
    case string:split(Uri, "?", leading) of
        [Uri] ->
            {tokenize_path(Uri), #{}};
        [Path, QueryParamString] ->
            {tokenize_path(Path), parse_query_params(QueryParamString)}
    end.

tokenize_path(Path) ->
    Components = string:split(Path, "/", all),
    [list_to_binary(C) || C <- Components, C =/= []].

%% @private
parse_query_params(QueryParamString) ->
    NVPairsStrings = string:split(QueryParamString, "&", all),
    NVPairLists = [string:split(NVPairString, "=") || NVPairString <- NVPairsStrings],
    maps:from_list([{list_to_atom(Key), Value} || [Key, Value] <- NVPairLists]).


get_handler(_Path, []) ->
    {error, no_handler};
get_handler(Path, [{PathPrefix, Config} | Rest]) ->
    case path_prefix(PathPrefix, Path) of
        {true, PathSuffix} ->
            {ok, PathSuffix, Config};
        _ ->
            get_handler(Path, Rest)
    end.

%% @private
path_prefix([], Path) ->
    {true, Path};
path_prefix([C|R1], [C|R2]) ->
    path_prefix(R1, R2);
path_prefix(_Prefix, _Path) ->
    false.

%% @private
get_protocol(get, #{<<"Upgrade">> := <<"websocket">>, <<"Connection">> := <<"Upgrade">>, <<"Sec-WebSocket-Key">> := _, <<"Sec-WebSocket-Version">> := <<"13">>} = _Headers) ->
    ws;
get_protocol(_, _) ->
    http.


%% @private
create_error(StatusCode, Error, Socket) ->
    ErrorString = io_lib:format("Error: ~p", [Error]),
    io:format("error in httpd. StatusCode=~p  Error=~p~n", [StatusCode, Error]),
    create_reply(StatusCode, "text/html", ErrorString, Socket).

%% @private
create_reply(StatusCode, ContentType, Reply, _Socket) when is_list(ContentType) orelse is_binary(ContentType) ->
    create_reply(StatusCode, #{"Content-Type" => ContentType}, Reply, _Socket);
create_reply(StatusCode, Headers, Reply, _Socket) when is_map(Headers) ->
    [
        <<"HTTP/1.1 ">>, erlang:integer_to_binary(StatusCode), <<" ">>, moniker(StatusCode),
        <<"\r\n">>,
        io_lib:format("Server: atomvm-~s\r\n", [get_version_str(erlang:system_info(atomvm_version))]),
        to_headers_list(Headers),
        <<"\r\n">>,
        Reply
    ].

%% @private
maybe_binary_to_string(Bin) when is_binary(Bin) ->
    erlang:binary_to_list(Bin);
maybe_binary_to_string(Other) ->
    Other.

%% @private
to_headers_list(Headers) ->
    [io_lib:format("~s: ~s\r\n", [maybe_binary_to_string(Key), maybe_binary_to_string(Value)]) || {Key, Value} <- maps:to_list(Headers)].


%% @private
get_version_str(Version) when is_binary(Version) ->
    binary_to_list(Version);
get_version_str(_) ->
    "unknown".

%% @private
moniker(?OK) ->
    <<"OK">>;
moniker(?INTERNAL_SERVER_ERROR) ->
    <<"INTERNAL_SERVER_ERROR">>;
moniker(?BAD_REQUEST) ->
    <<"BAD_REQUEST">>;
moniker(?NOT_FOUND) ->
    <<"NOT_FOUND">>;
moniker(?SWITCHING_PROTOCOLS) ->
    <<"Switching Protocols">>;
moniker(_) ->
    <<"undefined">>.

%% @private
method_to_atom("GET") ->
    get;
method_to_atom("PUT") ->
    put;
method_to_atom("POST") ->
    post;
method_to_atom("DELETE") ->
    delete;
method_to_atom(_) ->
    undefined.
