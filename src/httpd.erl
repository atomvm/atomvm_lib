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

-export([start/1, start_link/1, stop/1]).

%% deprecated exports
-export([start/2, start/3, start_link/2, start_link/3]).

-behaviour(gen_tcp_server).
-export([init/1, handle_receive/3, handle_tcp_closed/2]).

-include("httpd.hrl").
-include_lib("kernel/include/logger.hrl").

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
-type handlers() :: [{path(), handler_config()}].
-type octet() :: 0..255.
-type address_v4() :: {octet(), octet(), octet(), octet()}.
-type address() :: any | loopback | address_v4().
-type portnum() :: 0..65536.
-opaque httpd() :: pid().

-type config() :: #{
    address => address(),
    port => portnum(),
    handlers => handlers(),
    keepalive_timeout_ms => non_neg_integer(),
    recv_buffer_size => non_neg_integer()
}.

-export_type([
    config/0,
    method/0,
    path/0,
    http_request/0,
    query_params/0,
    httpd/0
]).

-define(DEFAULT_CONFIG, #{
    address => any,
    port => 8080,
    handlers => [],
    keepalive_timeout_ms => 17000,
    recv_buffer_size => 1024
}).

%%
%% Handle an HTTP request.
%%
-callback handle_http_req(Method :: method(), PathSuffix :: path(), HttpRequest :: http_request(), HandlerConfig :: handler_config()) ->
    ok | {ok, {ContentType :: content_type(), Reply :: term()}} | {ok, Reply :: term()} |
    close | {close, {ContentType :: content_type(), Reply :: term()}} | {close, Reply :: term()} |
    not_found | bad_request | internal_server_error |
    term().

%%
%% API
%%

-spec start_link(Config :: config()) -> {ok, HTTPD :: httpd()} | {error, Reason :: term()}.
start_link(Config) ->
    internal_start(Config, fun gen_tcp_server:start_link/1).

-spec start(Config :: config()) -> {ok, HTTPD :: httpd()} | {error, Reason :: term()}.
start(Config) ->
    internal_start(Config, fun gen_tcp_server:start/1).

%% @private
-spec internal_start(Config :: config(), StartFun :: function()) -> {ok, HTTPD :: httpd()} | {error, Reason :: term()}.
internal_start(Config, StartFun) ->
    EffectiveConfig = maps:merge(?DEFAULT_CONFIG, Config),
    GenTCPServerConfig = #{
        bind_address => #{
            family => inet,
            addr => maps:get(address, EffectiveConfig),
            port => maps:get(port, EffectiveConfig)
        },
        handler => {
            ?MODULE,
            maps:get(handlers, EffectiveConfig)
        },
        recv_timeout_ms => maps:get(keepalive_timeout_ms, EffectiveConfig),
        recv_len => maps:get(recv_buffer_size, EffectiveConfig)
    },
    ?LOG_DEBUG("Starting gen_tcp_server with config ~p~n", [GenTCPServerConfig]),
    StartFun(GenTCPServerConfig).

%% @deprecated
-spec start(Port :: portnum(), Handlers :: handlers()) -> {ok, HTTPD :: pid()} | {error, Reason :: term()}.
start(Port, Handlers) ->
    start(any, Port, Handlers).

%% @deprecated
-spec start(Address :: address(), Port :: portnum(), Handlers :: handlers()) -> {ok, HTTPD :: pid()} | {error, Reason :: term()}.
start(Address, Port, Handlers) ->
    io:format("DEPRECATION WARNING: Using deprecated ~p:start/3 function.  Use start/1 instead.~n", [?MODULE]),
    start(#{
        address => Address,
        port => Port,
        handlers => Handlers
    }).

%% @deprecated
-spec start_link(Port :: portnum(), Handlers :: handlers()) -> {ok, HTTPD :: httpd()} | {error, Reason :: term()}.
start_link(Port, Handlers) ->
    start_link(any, Port, Handlers).

%% @deprecated
-spec start_link(Address :: address(), Port :: portnum(), Handlers :: handlers()) -> {ok, HTTPD :: httpd()} | {error, Reason :: term()}.
start_link(Address, Port, Handlers) ->
    io:format("DEPRECATION WARNING: Using deprecated ~p:start_link/3 function.  Use start_link/1 instead.~n", [?MODULE]),
    start_link(#{
        bind_address => #{
            family => inet,
            addr => Address,
            port => Port
        },
        handler => {
            ?MODULE,
            Handlers
        }
    }).

stop(Httpd) ->
    gen_tcp_server:stop(Httpd).

%%
%% gen_tcp_server implementation
%%

-record(state, {
    handlers,
    gen_tcp_server,
    pending_request_map = #{},
    ws_socket_map = #{}
}).

%% @hidden
init(Handlers) ->
    {ok, #state{handlers = Handlers, gen_tcp_server = self()}}.

%% @hidden
handle_receive(Socket, Packet, State) ->
    try
        case maps:get(Socket, State#state.ws_socket_map, undefined) of
            undefined ->
                handle_http_request(Socket, Packet, State);
            WebSocket ->
                case httpd_ws_handler:handle_web_socket_message(WebSocket, Packet) of
                    ok ->
                        {noreply, State};
                    Error ->
                        {close, create_error(?INTERNAL_SERVER_ERROR, Error)}
                end
        end
    catch
        A:E:S ->
            ?LOG_ERROR("Caught error: ~p:~p:~p Packet=~p State=~p~n", [A, E, S, Packet, State]),
            {close, create_error(?BAD_REQUEST, E)}
    end.

%% @private
handle_http_request(Socket, Packet, State) ->
    case maps:get(Socket, State#state.pending_request_map, undefined) of
        undefined ->
            HttpRequest = parse_http_request(binary_to_list(Packet)),
            ?LOG_DEBUG("HttpRequest: ~p~n", [HttpRequest]),
            #{
                method := Method,
                headers := Headers
            } = HttpRequest,
            case get_protocol(Method, Headers) of
                http ->
                    case init_handler(HttpRequest, State) of
                        {ok, {Handler, HandlerState, PathSuffix, HandlerConfig}} ->
                            NewHttpRequest = HttpRequest#{
                                handler => Handler,
                                handler_state => HandlerState,
                                path_suffix => PathSuffix,
                                handler_config => HandlerConfig,
                                socket => Socket
                            },
                            handle_request_state(Socket, NewHttpRequest, State);
                        Error ->
                            {close, create_error(?INTERNAL_SERVER_ERROR, Error)}
                    end;
                ws ->
                    ?LOG_INFO("Protocol is ws", []),
                    Handlers = State#state.handlers,
                    Path = maps:get(path, HttpRequest),
                    case get_handler(Path, Handlers) of
                        {ok, PathSuffix, EntryConfig} ->
                            WsHandler = maps:get(handler, EntryConfig),
                            ?LOG_INFO("Got ws handler ~p", [WsHandler]),
                            HandlerConfig = maps:get(handler_config, EntryConfig, #{}),
                            case WsHandler:start(Socket, PathSuffix, HandlerConfig) of
                                {ok, WebSocket} ->
                                    ?LOG_INFO("Started web socket handler: ~p", [WebSocket]),
                                    NewWebSocketMap = maps:put(Socket, WebSocket, State#state.ws_socket_map),
                                    NewState = State#state{ws_socket_map = NewWebSocketMap},
                                    ReplyToken = get_reply_token(maps:get(headers, HttpRequest)),
                                    ReplyHeaders = #{"Upgrade" => "websocket", "Connection" => "Upgrade", "Sec-WebSocket-Accept" => ReplyToken},
                                    Reply = create_reply(?SWITCHING_PROTOCOLS, ReplyHeaders, <<"">>),
                                    ?LOG_INFO("Sending web socket upgrade reply: ~p", [Reply]),
                                    {reply, Reply, NewState};
                                Error ->
                                    ?LOG_ERROR("Web socket error: ~p", [Error]),
                                    {close, create_error(?INTERNAL_SERVER_ERROR, {web_socket_error, Error})}
                            end;
                        Error ->
                            Error
                    end
            end;
        PendingHttpRequest ->
            ?LOG_DEBUG("Packetlen: ~p", [erlang:byte_size(Packet)]),
            handle_request_state(Socket, PendingHttpRequest#{body := Packet}, State)
    end.

%% @private
init_handler(HttpRequest, State) ->
    Handlers = State#state.handlers,
    Path = maps:get(path, HttpRequest),
    case get_handler(Path, Handlers) of
        {ok, PathSuffix, EntryConfig} ->
            Handler = maps:get(handler, EntryConfig),
            HandlerConfig = maps:get(handler_config, EntryConfig, #{}),
            ?LOG_INFO("Initializing handler ~p with config ~p", [Handler, HandlerConfig]),
            case Handler:init_handler(PathSuffix, HandlerConfig) of
                {ok, HandlerState} ->
                    {ok, {Handler, HandlerState, PathSuffix, HandlerConfig}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @private
handle_request_state(Socket, HttpRequest, State) ->
    PendingRequestMap = State#state.pending_request_map,
    case get_request_state(HttpRequest) of
        complete ->
            ?LOG_DEBUG("Request complete.  Handling...", []),
            NewPendingRequestMap = maps:remove(Socket, PendingRequestMap),
            call_http_req_handler(Socket, HttpRequest, State#state{pending_request_map = NewPendingRequestMap});
        expect_continue ->
            Headers = maps:get(headers, HttpRequest),
            NewHeaders = maps:remove(<<"Expect">>, Headers),
            NewHttpRequest = HttpRequest#{headers := NewHeaders},
            Reply = create_reply(?CONTINUE, #{}, <<"">>),
            NewPendingRequestMap = PendingRequestMap#{Socket => NewHttpRequest},
            {reply, Reply, State#state{pending_request_map = NewPendingRequestMap}};
        wait_for_body ->
            NewPendingRequestMap = PendingRequestMap#{Socket => HttpRequest},
            call_http_req_handler(Socket, HttpRequest, State#state{pending_request_map = NewPendingRequestMap})
    end.

%% @private
get_request_state(HttpRequest) ->
    Headers = maps:get(headers, HttpRequest),
    case maps:get(<<"Expect">>, Headers, undefined) of
        <<"100-continue">> ->
            ?LOG_DEBUG("Expect: 100-continue", []),
            expect_continue;
        undefined ->
            case maps:get(<<"Content-Length">>, Headers, undefined) of
                undefined ->
                    ?LOG_DEBUG("No content length; request complete", []),
                    complete;
                ContentLenBin when is_binary(ContentLenBin) ->
                    ContentLen = binary_to_integer(ContentLenBin),
                    ?LOG_DEBUG("ContentLen: ~p", [ContentLen]),
                    Body = maps:get(body, HttpRequest, <<"">>),
                    BodyLen = erlang:byte_size(Body),
                    ?LOG_DEBUG("BodyLen: ~p", [BodyLen]),
                    case BodyLen < ContentLen of
                        true ->
                            wait_for_body;
                        false ->
                            ?LOG_INFO("Complete! BodyLen: ~p ContentLen: ~p", [BodyLen, ContentLen]),
                            complete
                    end
            end
    end.

%% @private
call_http_req_handler(Socket, HttpRequest, State) ->
    #{
        handler := Handler,
        handler_state := HandlerState
    } = HttpRequest,
    case Handler:handle_http_req(HttpRequest, HandlerState) of
        %% noreply
        {noreply, NewHandlerState} ->
            NewState = update_state(Socket, HttpRequest, NewHandlerState, State),
            {noreply, NewState};
        %% reply
        {reply, Reply, NewHandlerState} ->
            NewState = update_state(Socket, HttpRequest, NewHandlerState, State),
            {reply, create_reply(?OK, #{"Content-Type" => "application/octet-stream"}, Reply), NewState};
        {reply, ReplyHeaders, Reply, NewHandlerState} ->
            NewState = update_state(Socket, HttpRequest, NewHandlerState, State),
            {reply, create_reply(?OK, ReplyHeaders, Reply), NewState};
        %% close
        close ->
            {close, State};
        {close, Reply} ->
            {close, create_reply(?OK, #{"Content-Type" => "application/octet-stream"}, Reply)};
        {close, ReplyHeaders, Reply} ->
            {close, create_reply(?OK, ReplyHeaders, Reply)};
        %% errors
        {error, not_found} ->
            {close, create_error(?NOT_FOUND, not_found)};
        {error, bad_request} ->
            {close, create_error(?BAD_REQUEST, bad_request)};
        {error, internal_server_error} ->
            {close, create_error(?INTERNAL_SERVER_ERROR, internal_server_error)};
        HandlerError ->
            ?LOG_ERROR("Unexpected handler response ~p from handler ~p", [HandlerError, Handler]),
            {close, create_error(?INTERNAL_SERVER_ERROR, HandlerError)}
    end.

%% @private
update_state(Socket, HttpRequest, HandlerState, State) ->
    NewHttpRequest = HttpRequest#{handler_state := HandlerState},
    PendingRequestMap = State#state.pending_request_map,
    NewPendingRequestMap = PendingRequestMap#{Socket := NewHttpRequest},
    State#state{pending_request_map = NewPendingRequestMap}.


%% @hidden
handle_tcp_closed(Socket, State) ->
    case maps:get(Socket, State#state.ws_socket_map, undefined) of
        undefined ->
            State;
        WebSocket ->
            ok = httpd_ws_handler:stop(WebSocket),
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
    ReplyToken = base64:encode(crypto:hash(sha, PreImage)),
    ?LOG_DEBUG("ReplyToken: ~p", [ReplyToken]),
    ReplyToken.

%% @private
parse_http_request(Packet) ->
    ?LOG_DEBUG("parse_http_request.  Packet=~p", [Packet]),
    {Heading, HeadingRest} = parse_heading(Packet, start, [], #{}),
    {Headers, Body} = parse_header(HeadingRest, #{}),
    maps:merge(
        Heading,
        #{
            headers => Headers,
            body => erlang:list_to_binary(Body)
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
parse_heading(Packet, State, Tmp, Accum) ->
    ?LOG_ERROR("bad_heading.  Packet=~p State=~p Tmp=~p Accum=~p", [Packet, State, Tmp, Accum]),
    throw(bad_heading).

%% @private
parse_header([$\r, $\n | Rest], Accum) ->
    {Accum, Rest};
parse_header([$\n | Rest], Accum) ->
    {Accum, Rest};
parse_header([], Accum) ->
    {Accum, []};
parse_header(Packet, Accum) ->
    {Line, Rest} = parse_line(Packet, []),
    {Key, Value} = split_header(Line),
    parse_header(Rest, Accum#{Key => Value}).

parse_line([$\r, $\n | Rest], Accum) ->
    {lists:reverse(Accum), Rest};
parse_line([$\n | Rest], Accum) ->
    {lists:reverse(Accum), Rest};
parse_line([], Accum) ->
    {lists:reverse(Accum), []};
parse_line([C | Rest], Accum) ->
    parse_line(Rest, [C | Accum]);
parse_line(Packet, _Accum) ->
    ?LOG_ERROR("bad line: ->~s<-~n", [Packet]),
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
    maps:from_list([{list_to_atom(Key), url_decode(Value, [])} || [Key, Value] <- NVPairLists]).

% from https://docs.microfocus.com/OMi/10.62/Content/OMi/ExtGuide/ExtApps/URL_encoding.htm
url_decode([], Accum) ->
    lists:reverse(Accum);
url_decode([$%, $2, $0 | Rest], Accum) ->
    url_decode(Rest, [$\s | Accum]);
url_decode([$%, $3, $C | Rest], Accum) ->
    url_decode(Rest, [$< | Accum]);
url_decode([$%, $3, $E | Rest], Accum) ->
    url_decode(Rest, [$> | Accum]);
url_decode([$%, $2, $3 | Rest], Accum) ->
    url_decode(Rest, [$# | Accum]);
url_decode([$%, $2, $5 | Rest], Accum) ->
    url_decode(Rest, [$% | Accum]);
url_decode([$%, $2, $B | Rest], Accum) ->
    url_decode(Rest, [$+ | Accum]);
url_decode([$%, $7, $B | Rest], Accum) ->
    url_decode(Rest, [${ | Accum]);
url_decode([$%, $7, $D | Rest], Accum) ->
    url_decode(Rest, [$} | Accum]);
url_decode([$%, $7, $C | Rest], Accum) ->
    url_decode(Rest, [$| | Accum]);
url_decode([$%, $5, $C | Rest], Accum) ->
    url_decode(Rest, [$\\ | Accum]);
url_decode([$%, $5, $E | Rest], Accum) ->
    url_decode(Rest, [$^ | Accum]);
url_decode([$%, $7, $E | Rest], Accum) ->
    url_decode(Rest, [$~ | Accum]);
url_decode([$%, $5, $B | Rest], Accum) ->
    url_decode(Rest, [$[ | Accum]);
url_decode([$%, $5, $D | Rest], Accum) ->
    url_decode(Rest, [$] | Accum]);
url_decode([$%, $6, $0 | Rest], Accum) ->
    url_decode(Rest, [$` | Accum]);
url_decode([$%, $3, $B | Rest], Accum) ->
    url_decode(Rest, [$; | Accum]);
url_decode([$%, $2, $F | Rest], Accum) ->
    url_decode(Rest, [$/ | Accum]);
url_decode([$%, $3, $F | Rest], Accum) ->
    url_decode(Rest, [$? | Accum]);
url_decode([$%, $3, $A | Rest], Accum) ->
    url_decode(Rest, [$: | Accum]);
url_decode([$%, $4, $0 | Rest], Accum) ->
    url_decode(Rest, [$@ | Accum]);
url_decode([$%, $3, $D | Rest], Accum) ->
    url_decode(Rest, [$= | Accum]);
url_decode([$%, $2, $6 | Rest], Accum) ->
    url_decode(Rest, [$& | Accum]);
url_decode([$%, $2, $4 | Rest], Accum) ->
    url_decode(Rest, [$$ | Accum]);
url_decode([$%, $2, $1 | Rest], Accum) ->
    url_decode(Rest, [$! | Accum]);
url_decode([H | Rest], Accum) ->
    url_decode(Rest, [H | Accum]).

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
str(Str, Substring) ->
    str(Str, Substring, 1).

%% @private
str([], _, _I) ->
    0;
str([_H|T] = Str, Substring, I) ->
    case starts_with(Str, Substring) of
        true ->
            I;
        _ ->
            str(T, Substring, I + 1)
    end.

starts_with([], []) ->
    true;
starts_with([H|T1], [H|T2]) ->
    starts_with(T1, T2);
starts_with([_H1|_], [_H2|_]) ->
    false.



%% @private
get_protocol(get, #{<<"Upgrade">> := <<"websocket">>, <<"Connection">> := Upgrade, <<"Sec-WebSocket-Key">> := _, <<"Sec-WebSocket-Version">> := <<"13">>} = _Headers) ->
    case str(string:to_upper(binary_to_list(Upgrade)), "UPGRADE") of
        0 ->
            http;
        _ ->
            ws
    end;
get_protocol(_, _) ->
    http.


%% @private
create_error(StatusCode, Error) ->
    ErrorString = io_lib:format("Error: ~p", [Error]),
    ?LOG_INFO("Error in httpd. StatusCode=~p  Error=~p~n", [StatusCode, Error]),
    create_reply(StatusCode, "text/html", ErrorString).

%% @private
create_reply(StatusCode, ContentType, Reply) when is_list(ContentType) orelse is_binary(ContentType) ->
    create_reply(StatusCode, #{"Content-Type" => ContentType}, Reply);
create_reply(StatusCode, Headers, Reply) when is_map(Headers) ->
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
moniker(?CONTINUE) ->
    <<"Continue">>;
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
