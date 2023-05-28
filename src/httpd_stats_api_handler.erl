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

-module(httpd_stats_api_handler).

-behavior(httpd_api_handler).
-export([handle_api_request/4]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

%%
%% API Handler implementation
%%

handle_api_request(get, [<<"system">>], _HttpRequest, _Args) ->
    ?TRACE("GET system request", []),
    {ok, get_system_info()};

handle_api_request(get, [<<"memory">>], _HttpRequest, _Args) ->
    ?TRACE("GET memory request", []),
    {ok, get_memory()};

handle_api_request(Method, Path, _HttpRequest, _Args) ->
    io:format("ERROR!  Unsupported method ~p or path ~p~n", [Method, Path]),
    not_found.

%%
%% Internal Functions
%%

get_system_info() ->
    #{
        platform => atomvm:platform(),
        word_size => erlang:system_info(wordsize),
        system_architecture => erlang:system_info(system_architecture),
        atomvm_version => erlang:system_info(atomvm_version),
        esp32_chip_info => get_esp32_chip_info(),
        esp_idf_version => list_to_binary(erlang:system_info(esp_idf_version))
    }.

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

get_memory() ->
    #{
        atom_count => erlang:system_info(atom_count),
        process_count => erlang:system_info(process_count),
        port_count => erlang:system_info(port_count),
        esp32_free_heap_size => erlang:system_info(esp32_free_heap_size),
        esp32_largest_free_block => erlang:system_info(esp32_largest_free_block),
        esp32_minimum_free_size => erlang:system_info(esp32_minimum_free_size)
    }.
