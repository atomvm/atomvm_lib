%
% Copyright 2022 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(httpd_cmd_api_handler).

-behavior(httpd_api_handler).
-export([handle_api_request/4]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

%%
%% API Handler implementation
%%

handle_api_request(post, [<<"restart">>], _HttpRequest, _Args) ->
    ?TRACE("POST restart request", []),
    %% TODO parameterize
    spawn(fun() -> restart(3000) end),
    ok;

handle_api_request(Method, Path, _HttpRequest, _Args) ->
    io:format("ERROR!  Unsupported method ~p or path ~p~n", [Method, Path]),
    not_found.

%%
%% Internal Functions
%%

restart(SleepMs) ->
    timer:sleep(SleepMs),
    esp:restart().
