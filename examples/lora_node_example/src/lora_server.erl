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
-module(lora_server).

-export([start/0]).

start() ->
    LoraNodeConfig = #{
        lora => config:lora_config(sx127x),
        call_handler => fun handle_call/2
    },
    {ok, _LoraNode} = lora_node:start(robert, LoraNodeConfig),

    io:format("Lora server started.  Waiting to receive requests...~n"),
    loop_forever().

handle_call({hello, I} = _Message, Context) ->
    From = maps:get(from, Context),
    io:format("Received {hello, ~p} from ~p...~n", [I, From]),
    {hello, From};
handle_call(_Message, _Context) ->
    buzz_off.

loop_forever() ->
    receive
        halt -> ok
    after 100000 ->
        loop_forever()
    end.
