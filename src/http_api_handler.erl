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
-module(http_api_handler).

-behavior(http_handler).
-export([handler_module/0, handle_http_req/4]).

-type json_term() :: #{atom() := true | false | atom() | integer() | string() | binary() | list(json_term()) | json_term()}.

-callback handle_api_request(Method :: httpd:method(), PathSuffix :: httpd:path(), HttpRequest :: httpd:http_request(), Args :: term()) ->
    {ok, Reply :: json_term()} |
    {close, Reply :: json_term()} |
    bad_request | internal_server_error |
    term().

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

%% @hidden
handler_module() ->
    ?MODULE.

%% @hidden
handle_http_req(Method, Path, HttpRequest, HandlerConfig) ->
    ?TRACE("Method: ~p Path: ~p HttpRequest: ~p HandlerConfig: ~p", [Method, Path, HttpRequest, HandlerConfig]),
    Module = maps:get(module, HandlerConfig),
    Args = maps:get(args, HandlerConfig, undefined),
    ?TRACE("Module: ~p Args: ~p", [Module, Args]),
    case Module of
        Mod when is_atom(Mod) ->
            case Mod:handle_api_request(Method, Path, HttpRequest, Args) of
                {ok, Reply} ->
                    Body = json_encoder:encode(maps:to_list(Reply)),
                    {close, {"application/json", Body}};
                {close, Reply} ->
                    Body = json_encoder:encode(maps:to_list(Reply)),
                    {close, {"application/json", Body}};
                Error ->
                    Error
            end;
        Fun when is_function(Fun) ->
            case Fun(Method, Path, HttpRequest, Args) of
                {ok, Reply} ->
                    Body = json_encoder:encode(Reply),
                    {ok, {"application/json", Body}};
                Error ->
                    Error
            end;
        SomethingElse ->
            io:format("Bad Config.  Expected module or function, but got ~p~n", [SomethingElse]),
            internal_server_error
    end.
