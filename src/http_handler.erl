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

%%
%% @doc This module defined the `handle_http_req/4' callback function.
%%
%% The `handle_http_req/4' callback function is used to implment HTTP handlers for the `httpd' module.
%% @end
%%

-module(http_handler).

%%
%%
%%
-callback handle_http_req(Method :: atom(), PathSuffix :: list(), HttpRequest :: term(), HandlerConfig :: map()) ->
    ok |
    {ok, Reply :: term()} |
    {ok, ContentType :: string(), Reply :: term()} |
    not_found |
    bad_request |
    internal_server_error |
    term().
