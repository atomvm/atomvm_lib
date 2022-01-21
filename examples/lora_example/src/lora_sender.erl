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
-module(lora_sender).

-export([start/0]).

start() ->
    LoraConfig = config:lora_config(sx126x),
    {ok, Lora} = lora:start(LoraConfig),
    io:format("Lora started.  Sending messages...~n"),
    loop(Lora, 0).

loop(Lora, I) ->
    Payload = [<<"AtomVM ">>, integer_to_list(I)],
    try
        case lora:broadcast(Lora, Payload) of
            ok ->
                io:format("Sent ~p~n", [Payload]);
            Error ->
                io:format("Error sending: ~p~n", [Error])
        end
    catch
        exit:timeout ->
            io:format("Timed out broadcasting ~p~n", [Payload])
    end,
    timer:sleep(10000),
    loop(Lora, I + 1).
