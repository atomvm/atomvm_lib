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

-module(gen_tcp_server_test).

-export([start/0]).

-behaviour(gen_tcp_server).
-export([init/1, handle_receive/3, handle_tcp_closed/2]).

-include_lib("kernel/include/logger.hrl").

start() ->

    {ok, _} = logger_manager:start_link(#{
        log_level => info
    }),

    ok = maybe_start_network(atomvm:platform()),

    case gen_tcp_server:start(#{
            bind_address => #{
                family => inet,
                addr => any,
                port => 44444
            },
            handler => {
                ?MODULE,
                undefined
            },
            recv_timeout_ms => 30000,
            recv_len => 0
        }) of
        {ok, _Pid} ->
            ?LOG_INFO("gen_tcp_server_test started."),
            timer:sleep(infinity);
        Error ->
            ?LOG_INFO("An error occurred starting gen_tcp_server_test: ~p~n", [Error])
    end.

%%
%% gen_tcp_server implementation
%%

-record(state, {
}).

%% @hidden
init(undefined) ->
    {ok, #state{}}.

%% @hidden
handle_receive(Socket, Packet, State) ->
    ?LOG_INFO("Received packet ~p on socket ~p in state ~p", [Packet, Socket, State]),
    {noreply, State}.


%% @hidden
handle_tcp_closed(Socket, State) ->
    ?LOG_INFO("TCP Closed on socket ~p in state ~p", [Socket, State]),
    State.


%%
%% internal implementation
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
