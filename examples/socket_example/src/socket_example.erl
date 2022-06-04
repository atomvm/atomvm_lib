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
-module(socket_example).

-export([start/0]).

-export([init/1, handle_receive/3, handle_tcp_closed/1]).

start() ->
    ok = maybe_start_network(atomvm:platform()),

    {ok, Pid} = gen_tcp_server:start(1342, #{}, ?MODULE, []),
    io:format("Started gen_tcp_server ~p on port ~p~n", [Pid, 1342]),
    timer:sleep(infinity).

init(_) ->
    {ok, undefined}.

handle_receive(Socket, Packet, State) ->
    io:format("Received ~p from ~p.  Echoing back...~n", [Packet, Socket]),
    {reply, [<<"fred">>, 32, Packet], State}.

handle_tcp_closed(State) ->
    State.

%     {ok, Socket} = socket:open(inet, stream),

%     ok = socket:bind(Socket, #{family => inet, addr => any, port => 1342}),

%     ok = socket:listen(Socket),

%     erlang:display({sockname, socket:sockname(Socket)}),

%     io:format("Waiting for connection ...~n"),
%     {ok, Connection} = socket:accept(Socket),
%     erlang:display({connection, Connection}),

%     erlang:display({sockname, socket:sockname(Connection)}),
%     erlang:display({peername, socket:peername(Connection)}),

%     loop(5, Connection),

%     ok = socket:close(Connection),
%     ok = socket:close(Socket),

%     io:format("Going to sleep forever...~n"),
%     timer:sleep(infinity).



% loop(0, _Connection) ->
%     ok;
% loop(I, Connection) ->
%     io:format("Waiting to receive [~p] ...~n", [I]),
%     case socket:recv(Connection) of
%         {ok, Data} ->
%             erlang:display({recv, Data}),
%             {ok, _Rest} = socket:send(Connection, Data),
%             loop(I - 1, Connection);
%         Error ->
%             erlang:display(Error)
%     end.

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
