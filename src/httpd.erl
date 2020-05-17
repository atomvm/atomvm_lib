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

-export([start/2]).

-include("httpd.hrl").

-record(http_data, {
    method,
    uri,
    version,
    headers = [],
    body
}).

-include("logger.hrl").

%%
%% API
%%

start(Port, Handlers) ->
    case gen_tcp:listen(Port, []) of
        {ok, ListenSocket} = Ret ->
            spawn(fun() -> accept(ListenSocket, Handlers) end),
            Ret;
        Error ->
            Error
    end.

%%
%%
%%

accept(ListenSocket, Handlers) ->
    %?LOG_INFO("Waiting to accept a connection...", []),
    case gen_tcp:accept(ListenSocket) of
        {ok, _Socket} ->
            %?LOG_INFO("Accepted connection.", []),
            spawn(fun() -> accept(ListenSocket, Handlers) end),
            loop(Handlers);
        Error ->
            ?LOG_ERROR("An error occurred acception a connection: ~p", [Error])
    end.

loop(Handlers) ->
    try
        loop(Handlers, {waiting_request_line, [], #http_data{}})
    catch
        A:B ->
            erlang:display({A, B, self()})
    end.

loop(Handlers, State) ->
    receive
        {tcp_closed, _Socket} ->
            ?LOG_INFO("Client closed the connection.", []),
            ok;
        {tcp, Socket, Packet} ->
            case parse_http(Packet, State) of
                {ok, HttpData} ->
                    try
                        Method = method_to_atom(string:to_upper(HttpData#http_data.method)),
                        [Path, QueryParams] = normalize_uri(HttpData#http_data.uri),
                        case get_handler(Path, Handlers) of
                            {ok, {Mod, PathSuffix, Opts}} ->
                                TcpRequest = [
                                    {socket, Socket}
                                ],
                                HttpRequest = [
                                    {path, Path},
                                    {query_params, QueryParams},
                                    {tcp, TcpRequest},
                                    {method, Method},
                                    {version, HttpData#http_data.version},
                                    {headers, HttpData#http_data.headers},
                                    {body, HttpData#http_data.body}
                                ],
                                case Mod:handle_http_req(Method, PathSuffix, HttpRequest, Opts) of
                                    ok ->
                                        ok;
                                    {ok, {ContentType, Reply}} ->
                                        reply_and_close(?OK, ContentType, Reply, Socket);
                                    {ok, Reply} ->
                                        reply_and_close(?OK, "application/html", Reply, Socket);
                                    not_found ->
                                        handle_error(?NOT_FOUND, not_found, Socket);
                                    bad_request ->
                                        handle_error(?BAD_REQUEST, bad_request, Socket);
                                    internal_server_error ->
                                        handle_error(?INTERNAL_SERVER_ERROR, internal_server_error, Socket);
                                    HandlerError ->
                                        handle_error(?INTERNAL_SERVER_ERROR, HandlerError, Socket)
                                end;
                            NoRouteError ->
                                handle_error(?NOT_FOUND, NoRouteError, Socket)
                        end
                    catch
                        A:E ->
                            ?LOG_ERROR("Caught error: ~p:~p", [A, E]),
                            handle_error(?INTERNAL_SERVER_ERROR, E, Socket)
                    end                ;
                {continue, NewState} ->
                    loop(Handlers, NewState);
                ParseError ->
                    handle_error(?BAD_REQUEST, ParseError, Socket)
            end
    end.

normalize_uri(Uri) ->
    case string:split(Uri, "?", leading) of
        [Uri] ->
            [tokenize_path(Uri), []];
        [Path, QueryParamString] ->
            [tokenize_path(Path), parse_query_params(QueryParamString)]
    end.

tokenize_path(Path) ->
    Components = string:split(Path, "/", all),
    lists:map(
        fun(Component) ->
            unescape(Component)
        end,
        [C || C <- Components, C =/= []]
    ).

%% @private
parse_query_params(QueryParamString) ->
    NVPairsStrings = string:split(QueryParamString, "&", all),
    NVPairLists = [string:split(NVPairString, "=") || NVPairString <- NVPairsStrings],
    [{unescape(Key), unescape(Value)} || [Key, Value] <- NVPairLists].

%% @private
get_handler(_Path, []) ->
    {error, no_handler};
get_handler(Path, [{Prefix, Mod, Opts} | T]) ->
    case path_prefix(Prefix, Path) of
        {true, PathSuffix} ->
            {ok, {Mod, PathSuffix, Opts}};
        _ ->
            get_handler(Path, T)
    end.

%% @private
path_prefix([], Path) ->
    {true, Path};
path_prefix([C|R1], [C|R2]) ->
    path_prefix(R1, R2);
path_prefix(_Prefix, _Path) ->
    false.

%% @private
handle_error(StatusCode, Error, Socket) ->
    ErrorString = io_lib:format("Error: ~p", [Error]),
    ?LOG_ERROR("error in httpd. StatusCode=~p  Error=~p", [StatusCode, Error]),
    reply_and_close(StatusCode, "text/html", ErrorString, Socket).

%% @private
reply_and_close(StatusCode, ContentType, Reply, Socket) ->
    FullReply = [
        <<"HTTP/1.1 ">>, erlang:integer_to_binary(StatusCode), <<" ">>, moniker(StatusCode),
        <<"\r\n">>,
        io_lib:format("Server: atomvm-httpd\r\n", []),
        io_lib:format("Content-Type: ~s\r\n", [ContentType]),
        %io_lib:format("Content-Length: ~p\r\n", [length(Reply)]),
        <<"\r\n">>,
        Reply,
        <<"\n">>
    ],
    %?LOG_INFO("FullReply: ~p~n", [FullReply]),
    gen_tcp:send(Socket, FullReply),
    gen_tcp:close(Socket).

%% @private
unescape(String) ->
    unescape(String, []).

%% @private
unescape([], Accum) ->
    lists:reverse(Accum);
unescape([$%, Hex1, Hex2 | Rest], Accum) ->
    Char = (hex_char_to_n(Hex1) bsl 4) bor hex_char_to_n(Hex2),
    unescape(Rest, [Char|Accum]);
unescape([Char|Rest], Accum) ->
    unescape(Rest, [Char|Accum]).

%% @private
hex_char_to_n(N) when N >= $0 andalso N =< $9 ->
    N - $0;
hex_char_to_n(N) when N >= $a andalso N =< $f ->
    (N - $a) + 10;
hex_char_to_n(N) when N >= $A andalso N =< $F ->
    (N - $A) + 10.

%% @private
parse_http([], {waiting_body, [], HttpData}) ->
    {ok, HttpData};
parse_http([], {in_body, Acc, HttpData}) ->
    {ok, HttpData#http_data{body=lists:reverse(Acc)}};
parse_http([], StateAndData) ->
    {continue, StateAndData};
parse_http([$\n | Tail], {{waiting_lf, NextState}, [], HttpData}) ->
    parse_http(Tail, {NextState, [], HttpData});
%% start
parse_http([C | Tail], {waiting_request_line, [], HttpData}) ->
    %erlang:display({waiting_request_line, []}),
    parse_http(Tail, {in_method, [C], HttpData});
%% in_method
parse_http([$\s | Tail], {in_method, Acc, HttpData}) ->
    %erlang:display(waiting_uri),
    parse_http(Tail, {waiting_uri, [], HttpData#http_data{method=lists:reverse(Acc)}});
parse_http([C | Tail], {in_method, Acc, HttpData}) ->
    parse_http(Tail, {in_method, [C | Acc], HttpData});
%% waiting_uri
parse_http([$\s | Tail], {waiting_uri, Accum, HttpData}) ->
    %erlang:display(in_uri),
    parse_http(Tail, {in_uri, Accum, HttpData});
parse_http([C | Tail], {waiting_uri, [], HttpData}) ->
    parse_http(Tail, {in_uri, [C], HttpData});
%% in_uri
parse_http([$\s | Tail], {in_uri, Acc, HttpData}) ->
    %erlang:display(waiting_http_version),
    parse_http(Tail, {waiting_http_version, [], HttpData#http_data{uri=lists:reverse(Acc)}});
parse_http([C | Tail], {in_uri, Acc, HttpData}) ->
    parse_http(Tail, {in_uri, [C | Acc], HttpData});
%% waiting_http_version
parse_http([$\s | Tail], {waiting_http_version, Accum, HttpData}) ->
    %erlang:display(in_http_version),
    parse_http(Tail, {in_http_version, Accum, HttpData});
parse_http([C | Tail], {waiting_http_version, [], HttpData}) ->
    parse_http(Tail, {in_http_version, [C], HttpData});
%% in_http_version
parse_http([$\r | Tail], {in_http_version, Acc, HttpData}) ->
    parse_http(Tail, {{waiting_lf, waiting_headers}, [], HttpData#http_data{version=lists:reverse(Acc)}});
parse_http([C | Tail], {in_http_version, Acc, HttpData}) ->
    parse_http(Tail, {in_http_version, [C | Acc], HttpData});
%% waiting_headers
parse_http([$\r | Tail], {waiting_headers, [], HttpData}) ->
    parse_http(Tail, {{waiting_lf, waiting_body}, [], HttpData});
parse_http([C | Tail], {waiting_headers, [], HttpData}) ->
    parse_http(Tail, {in_header, [C], HttpData});
%% in_header
parse_http([$\r | Tail], {in_header, Acc, #http_data{headers=Headers} = HttpData}) ->
    %erlang:display(in_header_parsing),
    Header = parse_header(lists:reverse(Acc)),
    %erlang:display({header, Header}),
    parse_http(Tail, {{waiting_lf, waiting_headers}, [], HttpData#http_data{headers=[Header|Headers]}});
parse_http([C | Tail], {in_header, Acc, HttpData}) ->
    parse_http(Tail, {in_header, [C | Acc], HttpData});
%% waiting_body
parse_http([C | Tail], {waiting_body, [], HttpData}) ->
    parse_http(Tail, {in_body, [C], HttpData});
%% in_body
parse_http([C | Tail], {in_body, Acc, HttpData}) ->
    parse_http(Tail, {in_body, [C | Acc], HttpData});
%%
parse_http([C | Tail], {_, _Acc, HttpData}) ->
    erlang:display({consume, [C]}),
    parse_http(Tail, {consume, [], HttpData});
%%
parse_http(Input, StateTuple) ->
    erlang:display({cannot_process, Input, StateTuple}).

%% @private
parse_header(Header) ->
    %erlang:display({parsing, Header}),
    [Key, Value] = string:split(Header, ":"),
    {string:trim(Key), string:trim(Value)}.

%% @private
moniker(?OK) ->
    <<"OK">>;
moniker(?INTERNAL_SERVER_ERROR) ->
    <<"INTERNAL_SERVER_ERROR">>;
moniker(?BAD_REQUEST) ->
    <<"BAD_REQUEST">>;
moniker(?NOT_FOUND) ->
    <<"NOT_FOUND">>;
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
