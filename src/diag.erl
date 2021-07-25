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
-module(diag).

-export([print_all_proc_info/0, print_proc_info/0, print_proc_info/1]).


print_all_proc_info() ->
    Procs = erlang:processes(),
    io:format("num_procs: ~p~n", [length(Procs)]),
    [print_proc_info(Proc) || Proc <- Procs],
    io:format("esp32_free_heap_size: ~p~n", [erlang:system_info(esp32_free_heap_size)]),
    io:format("refc_binary_info: ~p~n", [erlang:system_info(refc_binary_info)]),
    ok.

print_proc_info() ->
    print_proc_info(self()).

print_proc_info(Proc) ->
    io:format("pid: ~p~n", [Proc]),
    io:format("    heap_size: ~p~n", [erlang:process_info(Proc, heap_size)]),
    io:format("    stack_size: ~p~n", [erlang:process_info(Proc, stack_size)]),
    io:format("    message_queue_len: ~p~n", [erlang:process_info(Proc, message_queue_len)]),
    io:format("    memory: ~p~n", [erlang:process_info(Proc, memory)]),
    ok.
