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
-module(httpd_file_handler).

-export([handler_module/0, init_handler/2, handle_http_req/2]).
-behavior(httpd_handler).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-record(state, {
    path_suffix,
    handler_config
}).

%% @hidden
handler_module() ->
    ?MODULE.

%% @hidden
init_handler(PathSuffix, HandlerConfig) ->
    {ok, #state{path_suffix = PathSuffix, handler_config = HandlerConfig}}.

%% @hidden
handle_http_req(#{method := get} = _HttpRequest, State) ->
    HandlerConfig = State#state.handler_config,
    App = maps:get(app, HandlerConfig),
    PathSuffix = State#state.path_suffix,
    FullPath = join("/", lists:reverse(PathSuffix)),
    ?TRACE("App: ~p PathSuffix: ~p FullPath: ~p", [App, PathSuffix, FullPath]),
    case atomvm:read_priv(App, FullPath) of
        undefined ->
            {error, not_found};
        Data ->
            {close, #{"Content-Type" => get_content_type(lists:reverse(PathSuffix))}, Data}
    end;
handle_http_req(_HttpRequest, _HandlerConfig) ->
    {error, internal_server_error}.

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
