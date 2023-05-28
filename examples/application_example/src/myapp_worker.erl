%%
%% Copyright (c) 2022 dushin.net
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
-module(myapp_worker).

-export([start_link/0]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

%%
%% api
%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%
%% gen_server implementation
%%

-record(state, {
    counter = 0
}).

%% @hidden
init([]) ->
    send_tick(),
    {ok, #state{}}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

%% @hidden
handle_info(tick, #state{counter = 5} = State) ->
    io:format("boom!~n"),
    exit(boom),
    {noreply, State};
handle_info(tick, State) ->
    io:format("tick~n"),
    send_tick(),
    {noreply, State#state{counter = State#state.counter + 1}};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%
%% internal implementation
%%

%% @private
send_tick() ->
    erlang:send_after(1000, self(), tick).
