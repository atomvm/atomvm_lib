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
-module(avm_env).

%%
%% Workaround for the fact that we don't have ETS
%%

-export([
    get_env/2, get_env/3, set_env/3, set_env/4, register_env/2
]).

-export([start_link/0]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

%%
%% api
%%

%% @doc
%%
%% @end
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

get_env(Application, Param) when is_atom(Application) andalso is_atom(Param) ->
    ?TRACE("get_env(~p, ~p)", [Application, Param]),
    gen_server:call(?MODULE, {get_env, Application, Param}).

get_env(Application, Param, Default) when is_atom(Application) andalso is_atom(Param) ->
    ?TRACE("get_env(~p, ~p, ~p)", [Application, Param, Default]),
    gen_server:call(?MODULE, {get_env, Application, Param, Default}).

set_env(Application, Param, Value) when is_atom(Application) andalso is_atom(Param) ->
    set_env(Application, Param, Value, []).

set_env(Application, Param, Value, Opts) when is_atom(Application) andalso is_atom(Param) ->
    gen_server:call(?MODULE, {set_env, Application, Param, Value, Opts}).

%% used internally by the application_contoller
register_env(Application, Env) when is_atom(Application) andalso is_map(Env) ->
    ?TRACE("register_env(~p, ~p)", [Application, Env]),
    gen_server:call(?MODULE, {register_env, Application, Env}).


%%
%% gen_server implementation
%%

-record(state, {
    envs = #{}
}).

%% @hidden
init(_) ->
    {ok, #state{}}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_call({get_env, Application, Param}, _From, State) ->
    ReturnValue = case do_get_env(Application, Param, maps:get(Application, State#state.envs, undefined), internal_default) of
        internal_default ->
            undefined;
        Value ->
            {ok, Value}
    end,
    ?TRACE("returning ~p", [ReturnValue]),
    {reply, ReturnValue, State};
handle_call({get_env, Application, Param, Default}, _From, State) ->
    ReturnValue = case do_get_env(Application, Param, maps:get(Application, State#state.envs, undefined), internal_default) of
        internal_default ->
            Default;
        Value ->
            Value
    end,
    ?TRACE("returning ~p", [ReturnValue]),
    {reply, ReturnValue, State};
handle_call({set_env, Application, Param, Value, Opts}, _From, State) ->
    Envs = State#state.envs,
    ApplicationEnv = maps:get(Application, Envs, undefined),
    {Reply, NewApplicationEnv} = do_set_env(Application, Param, Value, Opts, ApplicationEnv),
    {reply, Reply, State#state{envs = Envs#{Application => NewApplicationEnv}}};
handle_call({register_env, Application, Env}, _From, State) ->
    {Result, NewState} = try_register_env(Application, Env, State),
    {reply, Result, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

%% @hidden
handle_info(Msg, State) ->
    io:format("Received Msg: ~p~n", [Msg]),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%
%% internal implementation
%%

do_get_env(Application, Param, undefined, Default) ->
    get_persistent_env(Application, Param, Default);
do_get_env(Application, Param, ApplicationEnv, Default) ->
    case get_persistent_env(Application, Param, undefined) of
        undefined ->
            ?TRACE("No persistent env for application ~p with param ~p.  Searching for param in ApplicationEnv ~p", [Application, Param, ApplicationEnv]),
            maps:get(Param, ApplicationEnv, Default);
        NVSEnv ->
            maps:get(Param, maps:merge(ApplicationEnv, NVSEnv), Default)
    end.

get_persistent_env(Application, Param, Default) ->
    case atomvm:platform() of
        esp32 ->
            get_nvs_env(Application, Param, Default);
        _ ->
            Default
    end.


get_nvs_env(Application, Param, Default) ->
    ?TRACE("Getting NVS entry.  Application=~p Param=~p Default=~p", [Application, Param, Default]),
    case esp:nvs_get_binary(application, Application) of
        undefined ->
            ?TRACE("No NVS entry for application ~p.  Returning default=~p", [Application, Default]),
            Default;
        Binary ->
            try
                Term = binary_to_term(Binary),
                case is_map(Term) of
                    true ->
                        ?TRACE("NVS entry for application ~p param ~p is a map: Term=~p", [Application, Param, Term]),
                        case maps:get(Param, Term, undefined) of
                            undefined ->
                                Default;
                            Value ->
                                Value
                        end;
                    _ ->
                        {error, application_env_not_a_map}
                end
            catch
                _:Error ->
                    {error, {application_binary_not_a_term, Error}}
            end
    end.

do_set_env(Application, Param, Value, Opts, ApplicationEnv) ->
    case proplists:get_value(persistent, Opts, false) of
        true ->
            set_persistent_env(Application, Param, Value);
        false ->
            ok
    end,
    case ApplicationEnv of
        undefined ->
            {ok, #{Param => Value}};
        _ ->
            {ok, ApplicationEnv#{Param => Value}}
    end.

set_persistent_env(Application, Param, Value) ->
    case atomvm:platform() of
        esp32 ->
            ?TRACE("Setting persistent env for esp32.  Application=~p Param=~p Value=~p", [Application, Param, Value]),
            set_nvs_env(Application, Param, Value);
        Platform ->
            io:format("WARNING!  Requested to persist env but persistent storage not supported on platform ~p~n", [Platform]),
            ok
    end.


set_nvs_env(Application, Param, Value) ->
    case esp:nvs_get_binary(application, Application) of
        undefined ->
            Term = #{Param => Value},
            ?TRACE("Writing NVS for new entry.  Application=~p Term=~p", [Application, Term]),
            Binary = term_to_binary(Term),
            ok = esp:nvs_set_binary(application, Application, Binary);
        Binary ->
            try
                Term = binary_to_term(Binary),
                case is_map(Term) of
                    true ->
                        NewTerm = Term#{Param => Value},
                        ?TRACE("Writing NVS for existing entry.  Application=~p Term=~p", [Application, Term]),
                        NewBinary = term_to_binary(NewTerm),
                        ok = esp:nvs_set_binary(application, Application, NewBinary);
                    _ ->
                        io:format("Error!  Application env is not a map!  Term=~p", [Term]),
                        {error, application_env_not_a_map}
                end
            catch
                _:Error ->
                    io:format("Error converting binary to term!  Binary=~p Error=~p", [Binary, Error]),
                    {error, {application_binary_not_a_term, Error}}
            end
    end.

try_register_env(Application, Env, State) ->
    try
        NewState = do_register_env(Application, Env, State),
        {ok, NewState}
    catch
        _:E ->
            {{error, E}, State}
    end.

do_register_env(Application, Env, State) ->
    Envs = State#state.envs,
    NewEnv = case Env of
        Map when is_map(Env) ->
            Map;
        List when is_list(List) ->
            map:from_list(List)
    end,
    NewEnvs = case maps:get(Application, Envs, undefined) of
        undefined ->
            ?TRACE("Putting env ~p under application ~p into Envs ~p", [NewEnv, Application, Envs]),
            maps:put(Application, NewEnv, Envs);
        ApplicationEnv ->
            NewApplicationEnv = maps:merge(ApplicationEnv, NewEnv),
            ?TRACE("Putting merged env ~p under application ~p into Envs ~p", [NewApplicationEnv, Application, Envs]),
            maps:put(Application, NewApplicationEnv, Envs)
    end,
    State#state{envs = NewEnvs}.
