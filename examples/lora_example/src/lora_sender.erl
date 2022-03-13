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
    SPIConfig = [
        {bus_config, [
            {miso_io_num, 15},
            {mosi_io_num, 13},
            {sclk_io_num, 14}
        ]},
        {device_config, [
            {spi_clock_hz, 1000000},
            {spi_mode, 0},
            {spi_cs_io_num, 18},
            {address_len_bits, 8}
        ]}
    ],
    SPI = spi:open(SPIConfig),
    LoraConfig = #{
        spi => SPI,
        frequency => freq_915mhz,
        bandwidth => bw_125khz
        % , spreading_factor => 10
        % , tx_power => 15
    },
    {ok, Lora} = lora:start(LoraConfig),
    io:format("Lora started.  Sending messages...~n"),
    loop(Lora, 0).

loop(Lora, I) ->
    Payload = [<<"AtomVM ">>, integer_to_list(I)],
    case lora:broadcast(Lora, Payload) of
        ok ->
            io:format("Sent ~p~n", [Payload]),
            timer:sleep(10000);
        Error ->
            io:format("Error sending: ~p~n", [Error])
    end,
    loop(Lora, I + 1).
