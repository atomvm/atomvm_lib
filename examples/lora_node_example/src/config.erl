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
-module(config).

-export([spi_config/0, lora_config/0]).

spi_config() -> [
    {bus_config, [
        {miso_io_num, 12},
        {mosi_io_num, 13},
        {sclk_io_num, 14}
    ]},
    {device_config, [
        {spi_clock_hz, 1000000},
        {spi_mode, 0},
        {spi_cs_io_num, 18},
        {address_len_bits, 8}
    ]}
].

lora_config() -> #{
    spi => spi_config(),
    dio_0 => 26
}.
