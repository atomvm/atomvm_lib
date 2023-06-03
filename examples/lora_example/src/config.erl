%%
%% Copyright (c) 2022 dushin.net
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

-export([lora_config/1]).

-define(DEVICE_NAME, my_device).

-spec lora_config(Device :: sx127x | sx126x) -> map().
lora_config(Device) -> #{
    spi => spi:open(spi_config(Device)),
    device_name => ?DEVICE_NAME,
    device => Device,
    irq => 26,
    busy => 22,
    reset => 14
}.

%% @private
spi_config(Device) -> #{
    bus_config => #{
        miso_io_num => 19,
        mosi_io_num => 27,
        sclk_io_num => 5
    },
    device_config => #{
        ?DEVICE_NAME => #{
            address_len_bits => case Device of sx127x -> 8; _ -> 0 end,
            spi_clock_hz => 1000000,
            mode => 0,
            spi_cs_io_num => 18
        }
    }
}.
