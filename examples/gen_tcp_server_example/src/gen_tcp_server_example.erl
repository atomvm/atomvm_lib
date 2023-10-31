%%
%% Copyright (c) 2021 dushin.net
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
-module(gen_tcp_server_example).

-export([start/0]).

-behavior(gen_tcp_server).
-export([init/1, handle_receive/3, handle_tcp_closed/2]).

start() ->
    ok = maybe_start_network(atomvm:platform()),

    {ok, Pid} = gen_tcp_server:start(1342, #{{socket, reuseaddr} => true, {socket, linger} => #{onoff => true, linger => 0}}, ?MODULE, []),
    io:format("Started gen_tcp_server ~p on port ~p~n", [Pid, 1342]),
    timer:sleep(infinity).

init(_) ->
    {ok, undefined}.

handle_receive(Socket, <<"sleep\n">>, State) ->
    io:format("Received sleep from ~p.  sleeping ...~n", [Socket]),
    timer:sleep(5000),
    io:format("Done with sleep.  replying with ok packet~n"),
    {reply, <<"ok\n">>, State};
handle_receive(Socket, Packet, State) ->
    io:format("Received ~p from ~p.  Echoing back...~n", [Packet, Socket]),
    {reply, Packet, State}.

handle_tcp_closed(Socket, _State) ->
    io:format("Socekt closed: ~p~n", [Socket]),
    ok.

%%
%% Internal functions
%%

%% @private
maybe_start_network(esp32) ->
    Config = maps:get(sta, config:get()),
    case network:wait_for_sta(Config, 30000) of
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~p Netmask: ~p Gateway: ~p~n",
                [Address, Netmask, Gateway]
            ),
            ok;
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error]),
            Error
    end;
maybe_start_network(_Platform) ->
    ok.
