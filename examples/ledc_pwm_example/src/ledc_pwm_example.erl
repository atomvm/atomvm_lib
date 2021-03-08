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
-module (ledc_pwm_example).

-export([start/0]).

start() ->
    ok = ledc_pwm:start(),

    Freq = 5000,
    {ok, Timer} = ledc_pwm:create_timer(Freq),
    io:format("Timer: ~p~n", [Timer]),

    {ok, Channel1} = ledc_pwm:create_channel(Timer, 18),
    {ok, Channel2} = ledc_pwm:create_channel(Timer, 19),
    io:format("Channel1: ~p~n", [Channel1]),
    io:format("Channel2: ~p~n", [Channel2]),

    spawn(fun() -> fade_loop(Channel1) end),
    spawn(fun() -> step_loop(Channel2, 0, 10) end),
    loop_forever().

fade_loop(Channel) ->
    FadeupMs = 1500,
    ok = ledc_pwm:fade(Channel, 100, FadeupMs),
    timer:sleep(FadeupMs),

    FadeDownMs = 3000,
    ok = ledc_pwm:fade(Channel, 0, FadeDownMs),
    timer:sleep(FadeDownMs),

    fade_loop(Channel).

step_loop(Channel, Dutyp, _Incr) when Dutyp < 0 ->
    step_loop(Channel, 0, 10);
step_loop(Channel, Dutyp, _Incr) when Dutyp > 100 ->
    step_loop(Channel, 100, -10);
step_loop(Channel, Dutyp, Incr) ->
    ok = ledc_pwm:set_dutyp(Channel, Dutyp),
    timer:sleep(1000),
    step_loop(Channel, Dutyp + Incr, Incr).

loop_forever() ->
    timer:sleep(10000),
    loop_forever().
