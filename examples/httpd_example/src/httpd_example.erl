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

-module(httpd_example).

-behavior(http_api_handler).
-export([start/0, handle_api_request/4, handle_ws_init/3, handle_ws_message/2]).

start() ->
    ok = maybe_start_network(atomvm:platform()),

    Config = [
        {[<<"api">>], #{
            handler => http_api_handler,
            handler_config => #{
                module => ?MODULE
            }
        }},
        {[<<"ws">>], #{
            handler => http_ws_handler,
            handler_config => #{
                module => ?MODULE
            }
        }},
        {[], #{
            handler => http_file_handler,
            handler_config => #{
                app => ?MODULE
            }
        }}
    ],

    io:format("Starting httpd on port 8080 ...~n", []),
    case httpd:start(8080, Config) of
        {ok, _Pid} ->
            io:format("httpd started.~n", []),
            timer:sleep(infinity);
        Error ->
            io:format("An error occurred: ~p~n", [Error])
    end.

%%
%% API Handler implementation
%%

handle_api_request(get, [<<"system_info">>], HttpRequest, _Args) ->
    Socket = maps:get(socket, HttpRequest),
    {ok, #{addr := Host, port := Port}} = socket:peername(Socket),
    io:format("GET system_info request from ~p:~p~n", [Host, Port]),
    {ok, #{
        platform => atomvm:platform(),
        word_size => erlang:system_info(wordsize),
        system_architecture => erlang:system_info(system_architecture),
        atomvm_version => erlang:system_info(atomvm_version),
        esp32_chip_info => get_esp32_chip_info(),
        esp_idf_version => list_to_binary(erlang:system_info(esp_idf_version))
    }};
handle_api_request(get, [<<"memory">>], HttpRequest, _Args) ->
    Socket = maps:get(socket, HttpRequest),
    {ok, #{addr := Host, port := Port}} = socket:peername(Socket),
    io:format("GET memory request from ~p:~p~n", [Host, Port]),
    {ok, get_memory_data()};
handle_api_request(Method, Path, _HttpRequest, _Args) ->
    io:format("ERROR!  Unsupported method ~p or path ~p~n", [Method, Path]),
    not_found.

get_memory_data() ->
    #{
        atom_count => erlang:system_info(atom_count),
        process_count => erlang:system_info(process_count),
        port_count => erlang:system_info(port_count),
        esp32_free_heap_size => erlang:system_info(esp32_free_heap_size),
        esp32_largest_free_block => erlang:system_info(esp32_largest_free_block),
        esp32_minimum_free_size => erlang:system_info(esp32_minimum_free_size)
    }.

%%
%% WebSocket handlers
%%

handle_ws_init(WebSocket, _Path, _Args) ->
    io:format("Initializing websocket.~n"),
    spawn(fun() -> update_loop(WebSocket, get_memory_data()) end),
    {ok, undefined}.

handle_ws_message(<<"ping">>, State) ->
    {reply, <<"pong">>, State};
handle_ws_message(Message, State) ->
    io:format("Received message from web socket.  Message: ~p~n", [Message]),
    {noreply, State}.

update_loop(WebSocket, LastMemoryData) ->
    timer:sleep(5000),
    erlang:garbage_collect(),
    LatestMemoryData = get_memory_data(),
    NewMemoryData = get_difference(LastMemoryData, LatestMemoryData),
    Binary = json_encoder:encode(NewMemoryData),
    io:format("Sending message to client ~p ...~n", [Binary]),
    http_ws_handler:send(WebSocket, Binary),
    update_loop(WebSocket, LatestMemoryData).

%%
%% Internal functions
%%

get_difference(Map1, Map2) ->
    maps:fold(
        fun(Key, Value, Accum) ->
            case maps:get(Key, Map2, undefined) of
                undefined ->
                    [{Key, Value} | Accum];
                Value ->
                    Accum;
                NewValue ->
                    [{Key, NewValue} | Accum]
            end
        end,
        [],
        Map1
    ).

get_esp32_chip_info() ->
    case erlang:system_info(esp32_chip_info) of
        undefined ->
            undefined;
        %% TODO remove old API
        {esp32, Features, Cores, Revision} ->
            [{features, Features}, {cores, Cores}, {revision, Revision}, {model, undefined}];
        Info when is_map(Info) ->
            maps:to_list(Info);
        _ ->
            unknown
    end.

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
