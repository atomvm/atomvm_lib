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
-module(sht3x_example).

-export([start/0]).

start() ->
    {ok, I2CBus} = i2c_bus:start(#{
        sda => 21,
        scl => 22
    }),
    {ok, SHT} = sht3x:start_link(I2CBus),
    loop(SHT).

loop(SHT) ->
    case sht3x:take_reading(SHT) of
        {ok, {Temperature, Humidity}} ->
            io:format("Temperature: ~p, Humidity: ~p~n", [Temperature, Humidity]);
        Error ->
            io:format("Error taking reading: ~p~n", [Error])
    end,
    timer:sleep(5000),
    loop(SHT).
