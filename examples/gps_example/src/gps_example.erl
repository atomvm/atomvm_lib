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
-module(gps_example).

-export([start/0]).

start() ->
    Config = #{
        uart_port => uart_1,
        rx_pin => 2,
        gps_reading_filter => [datetime, latitude, longitude, altitude, speed, valid],
        gps_reading_handler => fun handle_gps_reading/2
    },
    {ok, _GPS} = gps:start(Config),
    io:format("GPS started.~n"),

    loop_forever().

loop_forever() ->
    receive
        halt -> halt
    end.

handle_gps_reading(_GPS, GPSReading) ->
    io:format("GPSReading: ~p~n", [GPSReading]).
