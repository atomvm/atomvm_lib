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
-module(avm_application).

-export([
    start/1, stop/1, get_env/2, get_env/3, set_env/3, set_env/4, ensure_all_started/1
]).


start(Application) ->
    avm_application_controller:start_application(Application).

stop(Application) ->
    avm_application_controller:stop_application(Application).

get_env(Application, Key) ->
    avm_env:get_env(Application, Key).

get_env(Application, Key, Default) ->
    avm_env:get_env(Application, Key, Default).

set_env(Application, Key, Value) ->
    avm_env:set_env(Application, Key, Value).

set_env(Application, Key, Value, Opts) ->
    avm_env:set_env(Application, Key, Value, Opts).

ensure_all_started(Application) ->
    avm_application_controller:ensure_all_started(Application).
    % avm_application_controller:start_application(Application).
