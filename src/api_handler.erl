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
-module(api_handler).

-export([handle_http_req/4]).

-include("logger.hrl").

handle_http_req(Method, Path, HttpRequest, {ModOrFun, Opts}) ->
    case ModOrFun of
        Mod when is_atom(Mod) ->
            case Mod:handle_api_request(Method, Path, HttpRequest, Opts) of
                {ok, Reply} ->
                    Body = json_encoder:encode(Reply),
                    {ok, {"application/json", Body}};
                Error ->
                    Error
            end;
        Fun when is_function(Fun) ->
            case Fun(Method, Path, HttpRequest, Opts) of
                {ok, Reply} ->
                    Body = json_encoder:encode(Reply),
                    {ok, {"application/json", Body}};
                Error ->
                    Error
            end;
        SomethingElse ->
            ?LOG_ERROR("Bad Argument.  Expected module or function, but got ~p", [SomethingElse]),
            internal_server_error
    end.
