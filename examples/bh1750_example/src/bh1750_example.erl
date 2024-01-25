%%
%% Copyright (c) 2020 dushin.net
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
-module(bh1750_example).

-export([start/0]).

start() ->
    {ok, I2CBus} = i2c_bus:start(
        #{
            sda => 21,
            scl => 22
        }
    ),
    Mode = continuous, %% change to continuous to receive continuous readings
    {ok, BH} = bh1750:start(I2CBus, [{mode, Mode}]),
    case Mode of
        one_time ->
            one_time_loop(BH);
        continuous ->
            continuous_loop(BH)
    end.

one_time_loop(BH) ->
    case bh1750:take_reading(BH) of
        {ok, Reading} ->
            io:format("Luminosity: ~slx\n", [Reading]);
        ErrorT ->
            io:format("Error taking reading temperature: ~p~n", [ErrorT])
    end,
    timer:sleep(1000),
    one_time_loop(BH).

continuous_loop(BH) ->
    receive
        Reading ->
            io:format("Luminosity: ~p\n", [Reading])
    end,
    continuous_loop(BH).
