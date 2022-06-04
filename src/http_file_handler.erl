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
-module(http_file_handler).

-export([handler_module/0, handle_http_req/4]).
-behavior(http_handler).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

%% @hidden
handler_module() ->
    ?MODULE.

%% @hidden
handle_http_req(get, Path, _HttpRequest, HandlerConfig) ->
    App = maps:get(app, HandlerConfig),
    FullPath = join("/", lists:reverse(Path)),
    ?TRACE("App: ~p Path: ~p FullPath: ~p", [App, Path, FullPath]),
    case atomvm:read_priv(App, FullPath) of
        undefined ->
            not_found;
        Data ->
            {close, {get_content_type(lists:reverse(Path)), Data}}
    end;
handle_http_req(_Method, _Path, _HttpRequest, _HandlerConfig) ->
    internal_server_error.

%% @private
join(Separator, Path) ->
    join(Separator, Path, []).

%% @private
join(_Separator, [], Accum) ->
    Accum;
join(Separator, [H|T], []) ->
    join(Separator, T, binary_to_list(H));
join(Separator, [H|T], Accum) ->
    join(Separator, T, binary_to_list(H) ++ Separator ++ Accum).

%% @private
get_content_type([Filename|_]) ->
    case get_suffix(Filename) of
        "html" ->
            "text/html";
        "css" ->
            "text/css";
        "js" ->
            "text/javascript";
        _ ->
            "application/octet-streeam"
    end.

get_suffix(Filename) ->
    case string:split(binary_to_list(Filename), ".") of
        [_Basename, Suffix] ->
            Suffix;
        _ ->
            undefined
    end.
