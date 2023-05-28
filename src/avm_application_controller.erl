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
-module(avm_application_controller).

-export([start_link/0, stop/0, start_application/1, stop_application/1, ensure_all_started/1]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-record(state, {
    running = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

stop() ->
    gen_server:stop(?MODULE).

start_application(Application) ->
    gen_server:call(?MODULE, {start, Application}).

stop_application(Application) ->
    gen_server:call(?MODULE, {stop, Application}).

ensure_all_started(Application) ->
    gen_server:call(?MODULE, {ensure_all_started, Application}, 60000).

%%
%% gen_server implementation
%%

%% @hidden
init(_) ->
    {ok, _Pid} = avm_env:start_link(),
    {ok, #state{}}.

    %% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_call({start, Application}, _From, State) ->
    Running = State#state.running,
    {Reply, NewState} = case do_start(Application, Running) of
        {ok, Pid} ->
            {ok, State#state{running = Running#{Application => Pid}}};
        Error ->
            {Error, State}
    end,
    {reply, Reply, NewState};
handle_call({ensure_all_started, Application}, _From, State) ->
    Running = State#state.running,
    {Reply, NewState} = case do_ensure_all_started(Application, Running) of
        {ok, ApplicationPidMap} ->
            {ok, State#state{running = maps:merge(Running, ApplicationPidMap)}};
        Error ->
            {Error, State}
    end,
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

%% @hidden
handle_info(_Msg, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%
%% internal implementation
%%

do_start(Application, Running) ->
    ?TRACE("Loading application ~p ...", [Application]),
    case try_load_application(Application) of
        {ok, ApplicationSpec} ->
            ?TRACE("Application ~p loaded with spec ~p.", [Application, ApplicationSpec]),
            maybe_register_env(Application, proplists:get_value(env, element(3, ApplicationSpec))),
            case maps:get(Application, Running, undefined) of
                undefined ->
                    ?TRACE("Starting application ~p ...", [Application]),
                    case try_start_application(ApplicationSpec) of
                        {ok, Pid} ->
                            %% TODO monitor
                            ?TRACE("Application ~p started with Pid ~p", [Application, Pid]),
                            {ok, Pid};
                        StartError ->
                            ?TRACE("Failed to start Application ~p started with error ~p", [Application, StartError]),
                            {error, {failed_to_start_application, Application, StartError}}
                    end;
                _RunningApp ->
                    {error, {already_running, Application}}
            end;
        LoadError ->
            ?TRACE("Failed to load application spec for application ~p", [Application]),
            ModuleName = list_to_atom(atom_to_list(Application) ++ "_app"),
            case try_start(ModuleName, []) of
                {ok, _Pid} = Okay ->
                    Okay;
                StartError ->
                    {error, {failed_to_load_application, Application, LoadError, StartError}}
            end
    end.

try_load_application(Application) ->
    case atomvm:read_priv(Application, "application.bin") of
        Bin when is_binary(Bin) ->
            try
                {ok, binary_to_term(Bin)}
            catch
                _:E ->
                    {error, {invalid_application_spec, Application, E}}
            end;
        undefined ->
            {error, {no_application_spec, Application}}
    end.

try_start_application(ApplicationSpec) ->
    {application, _Application, Properties} = ApplicationSpec,
    case proplists:get_value(mod, Properties, undefined) of
        undefined ->
            {error, missing_mod_property};
        {Module, Args} when is_atom(Module) ->
            try_start(Module, Args);
        Mod ->
            {error, {invalid_mod, Mod}}
    end.

try_start(Module, Args) ->
    try
        ?TRACE("Starting module ~p with args ~p ...", [Module, Args]),
        case Module:start(normal, Args) of
            {ok, Pid} ->
                ?TRACE("Module ~p started.", [Module]),
                {ok, Pid};
            StartError ->
                io:format("ERROR! Failed to start Module ~p with Args ~p. Error=~p~n", [Module, Args, StartError]),
                %% TODO stop previously started applications
                StartError
        end
    catch
        _:E:S ->
            %% TODO stop previously started applications
            io:format("Error starting application module ~p.  Error: ~p Stacktrace: ~p~n", [Module, E, S]),
            {error, {start_error, E, S}}
    end.

maybe_register_env(_Application, undefined) ->
    ok;
maybe_register_env(_Application, []) ->
    ok;
maybe_register_env(Application, Env) when is_list(Env) ->
    avm_env:register_env(Application, maps:from_list(Env)).


do_ensure_all_started(Application, Running) ->
    case try_ensure_all_started(Application, Running, #{}) of
        ApplicationPidMap when is_map(ApplicationPidMap) ->
            {ok, ApplicationPidMap};
        Error ->
            Error
    end.

try_ensure_all_started(kernel, _Running, Accum) ->
    ?TRACE("skipping kernel.", []),
    Accum;
try_ensure_all_started(stdlib, _Running, Accum) ->
    ?TRACE("skipping stdlib.", []),
    Accum;
try_ensure_all_started(Application, Running, Accum) ->
    case try_load_application(Application) of
        {ok, ApplicationSpec} ->
            ?TRACE("Loaded application spec ~p for application ~p", [ApplicationSpec, Application]),
            DependentApplications = proplists:get_value(applications, element(3, ApplicationSpec)),
            case try_start_list(DependentApplications, Running, #{}) of
                Started when is_map(Started) ->
                    ?TRACE("Started dependents: ~p", [Started]),
                    case do_start(Application, maps:merge(Running, Started)) of
                        {ok, Pid} ->
                            Accum#{Application => Pid};
                        Error ->
                            %% TODO unwind
                            ?TRACE("Error starting application: ~p", [Application]),
                            Error
                    end;
                Error ->
                    Error
            end;
        {error, {no_application_spec, _}} ->
            ?TRACE("No application spec for application ~p", [Application]),
            ModuleName = list_to_atom(atom_to_list(Application) ++ "_app"),
            case try_start(ModuleName, []) of
                {ok, Pid} ->
                    Accum#{Application => Pid};
                Error ->
                    Error
            end
    end.

try_start_list([], _Running, Started) ->
    Started;
try_start_list([Application | Rest], Running, Started) ->
    case maps:get(Application, maps:merge(Running, Started), undefined) of
        undefined ->
            case try_ensure_all_started(Application, Running, #{}) of
                ApplicationPidMap when is_map(ApplicationPidMap) ->
                    try_start_list(Rest, Running, maps:merge(ApplicationPidMap, Started));
                Error ->
                    try_stop_started(Started),
                    Error
            end;
        Pid when is_pid(Pid) ->
            ?TRACE("Application ~p already started at pid ~p", [Application, Pid]),
            try_start_list(Rest, Running, Started)
    end.

try_stop_started(Started) ->
    maps:fold(
        fun(Application, Pid, _Accum) ->
            try
                Application:stop(Pid),
                ok
            catch
                _:_E ->
                    ok
            end
        end,
        ok,
        Started
    ).
