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
-module(lora_client).

-export([start/0]).

start() ->
    LoraNodeConfig = #{
        lora => config:lora_config()
    },
    {ok, LoraNode} = lora_node:start(joe, LoraNodeConfig),
    io:format("Lora started.  Sending hello to robert...~n"),
    loop(LoraNode, 0).

loop(LoraNode, I) ->
    try
        io:format("Calling robert with message {hello, ~p} ... ", [I]),
        case lora_node:call(LoraNode, robert, {hello, I}) of
            {hello, Who} ->
                io:format("Received hello back from ~p~n", [Who]);
            Error ->
                io:format("Error sendingto robert: ~p~n", [Error])
        end
    after
        ok = timer:sleep(1000)
    end,
    loop(LoraNode, I + 1).
