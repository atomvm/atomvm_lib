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
-module(httpd_api_handler).

-behavior(httpd_handler).
-export([handler_module/0, init_handler/2, handle_http_req/2]).

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

-record(state, {
    path_suffix,
    handler_config
}).

%% @hidden
init_handler(PathSuffix, HandlerConfig) ->
    {ok, #state{path_suffix = PathSuffix, handler_config = HandlerConfig}}.

%% @hidden
handle_http_req(HttpRequest, State) ->
    #{
        method := Method
    } = HttpRequest,
    PathSuffix = State#state.path_suffix,
    HandlerConfig = State#state.handler_config,
    ?TRACE("Method: ~p PathSuffix: ~p HttpRequest: ~p HandlerConfig: ~p", [Method, PathSuffix, HttpRequest, HandlerConfig]),
    Module = maps:get(module, HandlerConfig),
    Args = maps:get(args, HandlerConfig, undefined),
    ?TRACE("Module: ~p Args: ~p", [Module, Args]),
    case Module of
        Mod when is_atom(Mod) ->
            case Mod:handle_api_request(Method, PathSuffix, HttpRequest, Args) of
                ok ->
                    {close, #{"Content-Type" => "text/plain"}, "ok"};
                {ok, Reply} when is_atom(Reply) ->
                    {close, #{"Content-Type" => "text/plain"}, atom_to_list(Reply)};
                {ok, Reply} when is_list(Reply) ->
                    {close, #{"Content-Type" => "text/plain"}, Reply};
                {ok, Reply} when is_map(Reply) ->
                    ?TRACE("Encoding reply ~p", [Reply]),
                    Body = json_encoder:encode(Reply),
                    ?TRACE("JSON Body: ~p", [Body]),
                    {close, #{"Content-Type" => "application/json"}, Body};
                {close, Reply} ->
                    ?TRACE("Encoding reply ~p", [Reply]),
                    Body = json_encoder:encode(Reply),
                    ?TRACE("JSON Body: ~p", [Body]),
                    {close, #{"Content-Type" => "application/json"}, Body};
                Error ->
                    Error
            end;
        Fun when is_function(Fun) ->
            case Fun(Method, PathSuffix, HttpRequest, Args) of
                {ok, Reply, State} ->
                    ?TRACE("Encoding reply ~p", [Reply]),
                    Body = json_encoder:encode(Reply),
                    {close, #{"Content-Type" => "application/json"}, Body};
                Error ->
                    Error
            end;
        SomethingElse ->
            io:format("Bad Config.  Expected module or function, but got ~p~n", [SomethingElse]),
            {error, internal_server_error}
    end.
