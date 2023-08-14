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
-module(lora).

%%%
%%% @doc
%%% An SPI driver for the LoRa (SX127X) chipset.
%%%
%%% This module can be used to send and receive messages using LoRa modulation.
%%% Currently, this module only supports point-to-point communications.  This
%%% module does not support LoRaWAN.
%%%
%%% References
%%% SemTech SX127x data sheet: https://semtech.my.salesforce.com/sfc/p/#E0000000JelG/a/2R0000001Rbr/6EfVZUorrpoKFfvaF_Fkpgp5kzjiNyiAbqcpqh9qSjE
%%% Python implementation: https://github.com/lemariva/uPyLoRaWAN
%%%
%%% @end

-export([start/1, stop/1, broadcast/2, sleep/1]).
%% debugging
-export([dump_registers/1]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-type lora() :: any().
-type message() :: iodata().

-type frequency() :: freq_169mhz | freq_433mhz | freq_868mhz | freq_915mhz | non_neg_integer().
-type bandwidth() :: bw_7_8khz | bw_10_4khz | bw_15_6khz | bw_20_8khz | bw_31_25khz | bw_41_7khz | bw_62_5khz
                        | bw_125khz | bw_250khz | bw_500khz.
-type tx_power() :: 2..17.
-type spreading_factor() :: sf_5 | sf_6 | sf_7 | sf_8 | sf_9 | sf_10 | sf_11 | sf_12 | 5..12.
-type ldro() :: on | off.
-type preamble_length() :: 6..65535.
-type lna_gain() :: lna_1 | lna_2 | lna_3 | lna_4 | lna_5 | lna_6 | auto.

-type coding_rate() :: cr_4_5 | cr_4_6 | cr_4_7 | cr_4_8.
-type header_mode() :: implicit | explicit.
-type device() :: sx127x | sx126x.

-type config() :: #{
    device => device(),
    device_name => atom(),
    frequency => frequency(),
    bandwidth => bandwidth(),
    tx_power => tx_power(),
    spreading_factor => spreading_factor(),
    preamble_length => preamble_length(),
    lna_gain => lna_gain(),
    coding_rate => coding_rate(),
    ldro => ldro(),
    header_mode => header_mode(),
    sync_word => non_neg_integer(),
    enable_crc => boolean(),
    invert_iq => boolean(),
    binary => boolean()
}.

-define(DEFAULT_CONFIG, #{
    device => sx127x,
    frequency => freq_915mhz,
    bandwidth => bw_125khz,
    tx_power => 2,
    spreading_factor => 7,
    preamble_length => 8,
    lna_gain => auto,
    coding_rate => cr_4_8,
    header_mode => explicit,
    sync_word => 16#12,
    enable_crc => true,
    invert_iq => false,
    binary => true
}).


%%%
%%% Public API
%%%

-spec start(config()) -> {ok, lora()} | {error, Reason::term()}.
start(Config) ->
    ?TRACE("Config: ~p", [Config]),
    SPI = get_or_load_spi(maps:get(spi, Config)),
    NewConfig = verify_config(maps:merge(?DEFAULT_CONFIG, Config#{spi => SPI})),
    ?TRACE("NewConfig: ~p", [NewConfig]),
    Module = get_module(NewConfig),
    case Module:start(NewConfig) of
        {ok, Impl} ->
            {ok, {Module, Impl}};
        E -> E
    end.

-spec stop(Lora::lora()) -> ok.
stop({Module, Impl}) ->
    Module:stop(Impl).

-spec broadcast(Lora::lora(), Message::message()) -> {ok, Length::non_neg_integer()} | {error, Reason::term()}.
broadcast({Module, Impl}, Message) ->
    Data = erlang:iolist_to_binary(Message),
    Module:broadcast(Impl, Data).

-spec sleep(Lora::lora()) -> ok.
sleep({Module, Impl}) ->
    Module:sleep(Impl).

%% @hidden
dump_registers({_Module, Impl}) ->
    ?TRACE("Calling dump_registers", []),
    gen_server:call(Impl, dump_registers).

%%%
%%% internal functions
%%%

%% @private
verify_config(Config) ->
    %% TODO
    Config.

%% @private
get_module(Config) ->
    %% Note.  In order to get r
    Device = maps:get(device, Config),
    case Device of
        sx127x ->
            lora_sx127x;
        sx126x ->
            lora_sx126x;
        Unknown ->
            throw({unsupported_device, Unknown})
    end.

%% @private
get_or_load_spi(SPI) when is_pid(SPI) ->
    SPI;
get_or_load_spi(SPIConfig) when is_map(SPIConfig) ->
    get_or_load_spi(maps:to_list(SPIConfig));
get_or_load_spi(SPIConfig) when is_list(SPIConfig) ->
    spi:open(SPIConfig).
