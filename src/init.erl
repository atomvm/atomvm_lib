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
-module(init).

-export([
    start/0, stop/0
]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

%% @hidden
start() ->
    ?TRACE("init:start/0", []),
    case atomvm:read_priv(?MODULE, "start.boot") of
        Bin when is_binary(Bin) ->
            ?TRACE("Found start.boot for init module", []),
            case start_boot(binary_to_term(Bin)) of
                ok ->
                    ?TRACE("init booted", []),
                    timer:sleep(infinity); %% TODO
                Error ->
                    io:format("ERROR!  Boot failure: ~p~n", [Error]),
                    {error, {boot_failure, Error}}
            end;
        undefined ->
            io:format("ERROR!  Missing boot file. (~p/priv/start.boot)~n", [?MODULE]),
            {error, missing_boot_file};
        _ ->
            io:format("ERROR!  Invalid boot file. (~p/priv/start.boot)~n", [?MODULE]),
            {error, invalid_boot_file}
    end.

%% @hidden
stop() ->
    %% TODO
    avm_application_controller:stop().

%% @private
start_boot(BootSpec) ->
    ?TRACE("start_boot(~p)", [BootSpec]),
    case BootSpec of
        {boot, _Version, Spec} when is_map(Spec) ->
            boot(Spec);
        _ ->
            {error, {invalid_boot_spec, not_map}}
    end.

%% @private
boot(Spec) ->
    ?TRACE("boot(~p)", [Spec]),
    case maps:get(applications, Spec, undefined) of
        Applications when is_list(Applications) ->
            ?TRACE("Starting applications ~p", [Applications]),
            {ok, _Pid} = avm_application_controller:start_link(),
            ?TRACE("Application controller started", []),
            try
                lists:foreach(
                    fun(Application) ->
                        ?TRACE("Ensuring that all applications are being started for ~p", [Application]),
                        case avm_application:ensure_all_started(Application) of
                            ok ->
                                io:format("Application ~p started~n", [Application]),
                                ok;
                            Error ->
                                io:format("Error!  Failed to ensure applications started for ~p~n", [Application]),
                                throw({error, {application_start_failure, Application, Error}})
                        end
                    end,
                    Applications
                )
            catch
                C:E:S ->
                io:format("Error!  An exception occurred when attempting to ensure all applications started.  Spec=~p Class=~p Exception=~p Stacktrace=~p~n", [Spec, C, E, S])
            end;
        _ ->
            io:format("Error!  Applications in spec file is not a list or does not exist! Spec=~p~n", [Spec]),
            {error, applications_not_list}
    end.
