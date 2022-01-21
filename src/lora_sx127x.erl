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
-module(lora_sx127x).

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
%%% AtomVM example program: https://github.com/bettio/AtomVM/blob/master/examples/erlang/esp32/sx127x.erl
%%%
%%% @end

%% Internal Lora provider API
-export([start/1, start_link/1, stop/1, broadcast/2, sleep/1]).

%% gen_server
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-behavior(gen_server).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-define (REG_FIFO, 16#00).
-define (REG_OP_MODE, 16#01).
-define (REG_FRF_MSB, 16#06).
-define (REG_FRF_MID, 16#07).
-define (REG_FRF_LSB, 16#08).
-define (REG_PA_CONFIG, 16#09).
-define (REG_LR_OCP, 16#0B).
-define (REG_LNA, 16#0C).
-define (REG_FIFO_ADDR_PTR, 16#0D).
-define (REG_FIFO_TX_BASE_ADDR, 16#0E).
-define (REG_FIFO_RX_BASE_ADDR, 16#0F).
-define (REG_FIFO_RX_CURRENT_ADDR, 16#10).
-define (REG_IRQ_FLAGS, 16#12).
-define (REG_RX_NB_BYTES, 16#13).
-define (REG_PKT_RSSI_VALUE, 16#1A).
-define (REG_PKT_SNR_VALUE, 16#1B).
-define (REG_MODEM_CONFIG_1, 16#1D).
-define (REG_MODEM_CONFIG_2, 16#1E).
-define (REG_PREAMBLE_MSB, 16#20).
-define (REG_PREAMBLE_LSB, 16#21).
-define (REG_PAYLOAD_LENGTH, 16#22).
-define (REG_MODEM_CONFIG_3, 16#26).
-define (REG_RSSI_WIDEBAND, 16#2C).
-define (REG_DETECTION_OPTIMIZE, 16#31).
-define (REG_DETECTION_THRESHOLD, 16#37).
-define (REG_SYNC_WORD, 16#39).
-define (REG_DIO_MAPPING_1, 16#40).
-define (REG_VERSION, 16#42).
-define (REG_PADAC, 16#4D).

-define (REG_INVERTIQ, 16#33).
-define (RFLR_INVERTIQ_RX_MASK, 16#BF).
-define (RFLR_INVERTIQ_RX_OFF, 16#00).
-define (RFLR_INVERTIQ_RX_ON, 16#40).
-define (RFLR_INVERTIQ_TX_MASK, 16#FE).
-define (RFLR_INVERTIQ_TX_OFF, 16#01).
-define (RFLR_INVERTIQ_TX_ON, 16#00).

-define (REG_INVERTIQ2, 16#3B).
-define (RFLR_INVERTIQ2_ON, 16#19).
-define (RFLR_INVERTIQ2_OFF, 16#1D).

-define (MODE_LONG_RANGE_MODE, 16#80).
-define (MODE_SLEEP, 16#00).
-define (MODE_STDBY, 16#01).
-define (MODE_TX, 16#03).
-define (MODE_RX_CONTINUOUS, 16#05).
-define (MODE_RX_SINGLE, 16#06).

-define (AUTO_AGC_FLAG, 16#04).

-define (IRQ_TX_DONE_MASK, 16#08).
-define (IRQ_PAYLOAD_CRC_ERROR_MASK, 16#20).
-define (IRQ_RX_DONE_MASK, 16#40).



%%%
%%% gen_server implementation
%%%

-record(state, {
    spi,
    config,
    irq
}).

%% @hidden
start(Config) ->
    gen_server:start(?MODULE, Config, []).

%% @hidden
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @hidden
stop(Lora) ->
    gen_server:stop(Lora).

%% @hidden
broadcast(Lora, Message) ->
    gen_server:call(Lora, {broadcast, Message}).

%% @hidden
sleep(Lora) ->
    gen_server:call(Lora, sleep).

%% @hidden
init(Config) ->
    ?TRACE("init(~p)", [Config]),
    timer:sleep(424),
    SPI = {maps:get(spi, Config), maps:get(device_name, Config)},
    case verify_version(SPI) of
        ok ->
            case init_lora(SPI, Config) of
                ok ->
                    % io:format("Registers: ~p~n", [do_dump_registers(SPI)]),
                    set_mode(SPI, recv),
                    State = #state{
                        spi = SPI,
                        config = Config,
                        irq = get_irq(Config)
                    },
                    {ok, State};
                LoraError ->
                    {stop, LoraError}
            end;
        VersionError ->
            {stop, VersionError}
    end.

%% @hidden
handle_cast(Message, State) ->
    io:format("Unhandled cast.  Message: ~p~n", [Message]),
    {noreply, State}.

%% @hidden
handle_call({broadcast, Message}, _From, State) ->
    Reply = do_broadcast(State#state.spi, Message),
    set_mode(State#state.spi, recv),
    {reply, Reply, State};
handle_call(dump_registers, _From, State) ->
    {reply, do_dump_registers(State#state.spi), State};
handle_call(sleep, _From, State) ->
    do_sleep(State#state.spi),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    io:format("lora Unhandled call.  Request: ~p~n", [Request]),
    {reply, error, State}.

%% @hidden
handle_info({gpio_interrupt, Pin}, #state{irq = Pin} = State) ->
    do_receive(State),
    {noreply, State};
handle_info(Message, State) ->
    io:format("lora Unhandled info.  Message: ~p~n", [Message]),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%%
%%% internal functions
%%%

%% @private
verify_version(SPI) ->
    ?TRACE("verify_version", []),
    case read_register(SPI, ?REG_VERSION) of
        {ok, 16#12} ->
            ok;
        {ok, UnexpectedVersion} ->
            {error, {unexpected_version, UnexpectedVersion}};
        Error ->
            Error
    end.

%% @private
init_lora(SPI, Config) ->
    ok = set_mode(SPI, sleep),

    ok = set_frequency(SPI, maps:get(frequency, Config)),
    ok = set_signal_bandwidth(SPI, maps:get(bandwidth, Config)),
    ok = set_lna_boost(SPI),
    ok = set_automatic_gain_control(SPI),
    ok = set_tx_power(SPI, maps:get(tx_power, Config)),
    ok = set_header_mode(SPI, maps:get(header_mode, Config)),
    ok = set_spreading_factor(SPI, maps:get(spreading_factor, Config)),
    ok = set_coding_rate(SPI, maps:get(coding_rate, Config)),
    ok = set_preamble_length(SPI, maps:get(preamble_length, Config)),
    ok = set_sync_word(SPI, maps:get(sync_word, Config)),
    ok = set_enable_crc(SPI, maps:get(enable_crc, Config)),
    ok = set_invert_iq(SPI, maps:get(invert_iq, Config)),
    ok = set_base_addr(SPI),

    ok = set_mode(SPI, standby),

    GPIO = gpio:open(),
    ok = maybe_set_irq(SPI, GPIO, get_irq(Config)),
    ok = maybe_reset(GPIO, maps:get(reset, Config, undefined)).

%% @private
get_irq(Config) ->
    case {maps:get(dio_0, Config, undefined), maps:get(irq, Config, undefined)} of
        {undefined, undefined} ->
            undefined;
        {undefined, IRQ} ->
            IRQ;
        {DPIO_0, undefined} ->
            io:format("WARNING: dpio_0 deprecated.  Use irq instead.~n"),
            DPIO_0;
        {_DPIO_0, IRQ} ->
            io:format("WARNING: dpio_0 deprecated and IRQ defined.  Using IRQ value.~n"),
            IRQ
    end.

%% @private
set_mode(SPI, sleep) ->
    set_mode(SPI, ?MODE_SLEEP);
set_mode(SPI, standby) ->
    set_mode(SPI, ?MODE_STDBY);
set_mode(SPI, recv) ->
    set_mode(SPI, ?MODE_RX_CONTINUOUS);
set_mode(SPI, Mode) ->
    ?TRACE("set_mode ~p", [Mode]),
    ok = write_register(SPI, ?REG_OP_MODE, ?MODE_LONG_RANGE_MODE bor Mode),
    ok.

%% @private
get_mode(SPI) ->
    ?TRACE("get_mode", []),
    {ok, Mode} = read_register(SPI, ?REG_OP_MODE),
    Mode.

%% @private
set_frequency(SPI, freq_169mhz) ->
    set_frequency_internal(SPI, 2768896);
set_frequency(SPI, freq_433mhz) ->
    set_frequency_internal(SPI, 7094272);
set_frequency(SPI, freq_868mhz) ->
    set_frequency_internal(SPI, 14221312);
set_frequency(SPI, freq_915mhz) ->
    set_frequency_internal(SPI, 14991360);
set_frequency(SPI, Freq) when is_integer(Freq) ->
    %% caution: requires AtomVM fix for parsing external terms > 0x0FFFFFFF
    {F, _} = rational:simplify(
        rational:reduce(
            rational:multiply(
                Freq,
                {256,15625} %% 32Mhz/2^19 or rational:reduce(rational:divide(1 bsl 19, 32000000))
            )
        )
    ),
    set_frequency_internal(SPI, F).

%% @private
set_frequency_internal(SPI, F) when is_integer(F) ->
    ?TRACE("set_frequency_internal ~p", [F]),
    ok = write_register(SPI, ?REG_FRF_MSB, ((F bsr 16) band 16#FF)),
    ok = write_register(SPI, ?REG_FRF_MID, ((F bsr 8) band 16#FF)),
    ok = write_register(SPI, ?REG_FRF_LSB, F band 16#FF),
    ok.

%% @private
set_signal_bandwidth(SPI, bw_7_8khz) ->
    set_signal_bandwidth(SPI, 0);
set_signal_bandwidth(SPI, bw_10_4khz) ->
    set_signal_bandwidth(SPI, 1);
set_signal_bandwidth(SPI, bw_15_6khz) ->
    set_signal_bandwidth(SPI, 2);
set_signal_bandwidth(SPI, bw_20_8khz) ->
    set_signal_bandwidth(SPI, 3);
set_signal_bandwidth(SPI, bw_31_25khz) ->
    set_signal_bandwidth(SPI, 4);
set_signal_bandwidth(SPI, bw_41_7khz) ->
    set_signal_bandwidth(SPI, 5);
set_signal_bandwidth(SPI, bw_62_5khz) ->
    set_signal_bandwidth(SPI, 6);
set_signal_bandwidth(SPI, bw_125khz) ->
    set_signal_bandwidth(SPI, 7);
set_signal_bandwidth(SPI, bw_250khz) ->
    set_signal_bandwidth(SPI, 8);
set_signal_bandwidth(SPI, bw_500khz) ->
    set_signal_bandwidth(SPI, 9);
set_signal_bandwidth(SPI, I) ->
    ?TRACE("set_signal_bandwidth ~p", [I]),
    {ok, ModemConfig1} = read_register(SPI, ?REG_MODEM_CONFIG_1),
    ok = write_register(SPI, ?REG_MODEM_CONFIG_1, (ModemConfig1 band 16#0F) bor (I bsl 4)),
    ok.

%% @private
set_lna_boost(SPI) ->
    ?TRACE("set_lna_boost", []),
    {ok, LNA} = read_register(SPI, ?REG_LNA),
    ok = write_register(SPI, ?REG_LNA, LNA bor 16#03),
    ok.

%% @private
set_automatic_gain_control(SPI) ->
    ?TRACE("set_automatic_gain_control", []),
    ok = write_register(SPI, ?REG_MODEM_CONFIG_3, ?AUTO_AGC_FLAG),
    ok.

%% @private
set_tx_power(SPI, Level) when 2 =< Level andalso Level =< 17 ->
    ?TRACE("set_tx_power ~p", [Level]),
    ok = write_register(SPI, ?REG_PADAC, 16#87),
    ok = write_register(SPI, ?REG_PA_CONFIG, 16#80 bor (Level - 2)),
    ok;
set_tx_power(_SPI, Level) ->
    {error, {tx_lower, Level}}.

%% @private
set_header_mode(SPI, implicit) ->
    ?TRACE("set_header_mode implicit", []),
    {ok, ModemConfig1} = read_register(SPI, ?REG_MODEM_CONFIG_1),
    ok = write_register(SPI, ?REG_MODEM_CONFIG_1, ModemConfig1 bor 16#01),
    ok;
set_header_mode(SPI, explicit) ->
    ?TRACE("set_header_mode explicit", []),
    {ok, ModemConfig1} = read_register(SPI, ?REG_MODEM_CONFIG_1),
    ok = write_register(SPI, ?REG_MODEM_CONFIG_1, ModemConfig1 band 16#FE),
    ok.

%% @private
set_spreading_factor(SPI, SF) ->
    ?TRACE("set_spreading_factor ~p", [SF]),
    ok = write_register(SPI, ?REG_DETECTION_OPTIMIZE, 16#c3),
    ok = write_register(SPI, ?REG_DETECTION_THRESHOLD, 16#0a),
    {ok, ModemConfig2} = read_register(SPI, ?REG_MODEM_CONFIG_2),
    ok = write_register(SPI, ?REG_MODEM_CONFIG_2, (ModemConfig2 band 16#0f) bor ((SF bsl 4) band 16#f0)),
    ok.

%% @private
set_coding_rate(SPI, cr_4_5) ->
    set_coding_rate(SPI, 5);
set_coding_rate(SPI, cr_4_6) ->
    set_coding_rate(SPI, 6);
set_coding_rate(SPI, cr_4_7) ->
    set_coding_rate(SPI, 7);
set_coding_rate(SPI, cr_4_8) ->
    set_coding_rate(SPI, 8);
set_coding_rate(SPI, Denominator) ->
    ?TRACE("set_coding_rate ~p", [Denominator]),
    Cr = Denominator - 4,
    {ok, ModemConfig1} = read_register(SPI, ?REG_MODEM_CONFIG_1),
    ok = write_register(
        SPI,
        ?REG_MODEM_CONFIG_1,
        (ModemConfig1 band 16#F1) bor (Cr bsl 1)
    ),
    ok.

%% @private
set_preamble_length(SPI, Length) ->
    ?TRACE("set_preamble_length ~p", [Length]),
    ok = write_register(SPI, ?REG_PREAMBLE_MSB,  (Length bsr 8) band 16#FF),
    ok = write_register(SPI, ?REG_PREAMBLE_LSB,  (Length bsr 0) band 16#FF),
    ok.

%% @private
set_sync_word(SPI, Word) ->
    ?TRACE("set_sync_word", []),
    ok = write_register(SPI, ?REG_SYNC_WORD, Word),
    ok.

%% @private
set_enable_crc(SPI, true) ->
    ?TRACE("set_enable_crc ~p", [true]),
    {ok, ModemConfig2} = read_register(SPI, ?REG_MODEM_CONFIG_2),
    ok = write_register(SPI, ?REG_MODEM_CONFIG_2, ModemConfig2 bor 16#04),
    ok;
set_enable_crc(SPI, false) ->
    ?TRACE("set_enable_crc ~p", [false]),
    {ok, ModemConfig2} = read_register(SPI, ?REG_MODEM_CONFIG_2),
    ok = write_register(SPI, ?REG_MODEM_CONFIG_2, ModemConfig2 band 16#FB),
    ok.

%% @private
set_invert_iq(SPI, true) ->
    ?TRACE("set_invert_iq ~p", [true]),
    {ok, InvertIQ} = read_register(SPI, ?REG_INVERTIQ),
    Value = (InvertIQ band ?RFLR_INVERTIQ_TX_MASK band ?RFLR_INVERTIQ_RX_MASK)
            bor ?RFLR_INVERTIQ_RX_ON bor ?RFLR_INVERTIQ_TX_ON,
    ok = write_register(SPI, ?REG_INVERTIQ, Value),
    ok = write_register(SPI, ?REG_INVERTIQ2, ?RFLR_INVERTIQ2_ON),
    ok;
set_invert_iq(SPI, false) ->
    ?TRACE("set_invert_iq ~p", [false]),
    {ok, InvertIQ} = read_register(SPI, ?REG_INVERTIQ),
    Value = (InvertIQ band ?RFLR_INVERTIQ_TX_MASK band ?RFLR_INVERTIQ_RX_MASK)
            bor ?RFLR_INVERTIQ_RX_OFF bor ?RFLR_INVERTIQ_TX_OFF,
    ok = write_register(SPI, ?REG_INVERTIQ, Value),
    ok = write_register(SPI, ?REG_INVERTIQ2, ?RFLR_INVERTIQ2_OFF),
    ok.

%% @private
set_base_addr(SPI) ->
    ?TRACE("set_base_addr", []),
    ok = write_register(SPI, ?REG_FIFO_TX_BASE_ADDR, 0),
    ok = write_register(SPI, ?REG_FIFO_RX_BASE_ADDR, 0),
    ok.

%% @private
maybe_set_irq(_SPI, _GPIO, undefined) ->
    ok;
maybe_set_irq(SPI, GPIO, Pin) ->
    ?TRACE("maybe_set_irq ~p", [Pin]),
    ok = write_register(SPI, ?REG_DIO_MAPPING_1, 16#00),
    gpio:set_int(GPIO, Pin, rising),
    ok.

%% @private
maybe_reset(_GPIO, undefined) ->
    ok;
maybe_reset(GPIO, Pin) ->
    ?TRACE("maybe_reset ~p", [Pin]),
    gpio:set_direction(GPIO, Pin, output),
    gpio:set_level(GPIO, Pin, 0),
    timer:sleep(20),
    gpio:set_level(GPIO, Pin, 1),
    timer:sleep(50),
    ok.

%% @private
get_rssi(SPI, Frequency) ->
    {ok, RSSI} = read_register(SPI, ?REG_PKT_RSSI_VALUE),
    Sub = case Frequency of
        freq_868mhz -> 157;
        freq_915mhz -> 157;
        _ -> 164
    end,
    RSSI - Sub.

%% @private
get_snr(SPI) ->
    {ok, SNR} = read_register(SPI, ?REG_PKT_SNR_VALUE),
    SNR bsr 2.


%%%
%%% send
%%%

do_broadcast(SPI, Data) ->
    %%
    %% prepare
    %%
    ?TRACE("preparing transmit...", []),
    set_mode(SPI, standby),
    set_header_mode(SPI, explicit),
    ok = write_register(SPI, ?REG_FIFO_ADDR_PTR, 0),
    ok = write_register(SPI, ?REG_PAYLOAD_LENGTH, 0),
    %%
    %% write data to FIFO in Lora chip
    %%
    ?TRACE("writing data to FIFO: ~p", [Data]),
    {ok, CurrentLength} = read_register(SPI, 16#22),
    Len = write_packet_data(SPI, Data),
    ok = write_register(SPI, ?REG_PAYLOAD_LENGTH, CurrentLength + Len),
    %%
    %% transmit and wait for signal
    %%
    ?TRACE("transmitting", []),
    ok = write_register(SPI, ?REG_OP_MODE, ?MODE_LONG_RANGE_MODE bor ?MODE_TX),
    wait_flags(SPI, ?REG_IRQ_FLAGS, ?IRQ_TX_DONE_MASK),
    ok = write_register(SPI, ?REG_IRQ_FLAGS, ?IRQ_TX_DONE_MASK),
    %%
    %% drop back into receive mode
    %%
    set_mode(SPI, recv),
    ?TRACE("done", []),
    ok.

%% @private
write_packet_data(SPI, L) ->
    write_packet_data(SPI, L, 0).

%% @private
write_packet_data(_SPI, [], Len) ->
    Len;
write_packet_data(_SPI, <<"">>, Len) ->
    Len;
write_packet_data(SPI, L, Len) ->
    %% Workaround for AtomVM bug: Can't pattern match on Lists/Binaries without
    %% a badarg exception being thrown.  Use if/then/else instead
    if  is_list(L) ->
            [H|T] = L,
            if  is_integer(H) ->
                    write_register(SPI, ?REG_FIFO, H),
                    write_packet_data(SPI, T, Len + 1);
                true ->
                    K = write_packet_data(SPI, H),
                    K + write_packet_data(SPI, T)
            end;
        is_binary(L) ->
            <<H:8/unsigned, T/binary>> = L,
            ok = write_register(SPI, ?REG_FIFO, H),
            write_packet_data(SPI, T, Len + 1);
        true ->
            throw({unsupported_payload, L})
    end.

%% @private
wait_flags(SPI, Register, Mask) ->
    wait_flags(SPI, Register, Mask, 0).

%% @private
wait_flags(SPI, Register, Mask, 0) ->
    {ok, Flags} = read_register(SPI, Register),
    wait_flags(SPI, Register, Mask, Flags band Mask);
wait_flags(_SPI, _Register, _Mask, _NotZero) ->
    ok.

%%%
%%% receive
%%%

%% @private
do_receive(State) ->
    ?TRACE("do_receive: State=~p", [State]),
    SPI = State#state.spi,
    {ok, IRQFlags} = read_register(SPI, ?REG_IRQ_FLAGS),
    ok = write_register(SPI, ?REG_IRQ_FLAGS, IRQFlags),

    RxDone = (IRQFlags band ?IRQ_RX_DONE_MASK) =/= 0,
    CrcError = (IRQFlags band ?IRQ_PAYLOAD_CRC_ERROR_MASK) =/= 0,
    case {RxDone, CrcError} of
        {true, false} ->
            {ok, PacketLength} = read_register(SPI, ?REG_RX_NB_BYTES),
            {ok, CurrentAddr} = read_register(SPI, ?REG_FIFO_RX_CURRENT_ADDR),

            ok = write_register(SPI, ?REG_FIFO_ADDR_PTR, CurrentAddr),
            Data = read_packet_data(SPI, PacketLength),
            Frequency = maps:get(frequency, State#state.config),
            QoS = #{
                rssi => get_rssi(SPI, Frequency),
                snr => get_snr(SPI)
            },
            ?TRACE("Received data (len=~p); Qos: ~p", [length(Data), QoS]),

            ok = write_register(SPI, ?REG_FIFO_ADDR_PTR, 0),
            %%
            %% Notify handler
            %%
            Lora = {?MODULE, self()},
            case maps:get(receive_handler, State#state.config, undefined) of
                undefined ->
                    ok;
                Handler ->
                    ReplyData = case maps:get(binary, State#state.config, true) of
                        true ->
                            list_to_binary(Data);
                        _ ->
                            Data
                    end,
                    % erlang:garbage_collect(),
                    if
                        is_pid(Handler) ->
                            Handler ! {lora_receive, Lora, ReplyData, QoS};
                        is_function(Handler) ->
                            spawn(fun() -> Handler(Lora, ReplyData, QoS) end);
                        true ->
                            ok
                    end
            end;
        {_, true} ->
            ?TRACE("CRC error", []);
        {_, _} ->
            ?TRACE("Unexpected IRQFlags: ~p", [IRQFlags])
    end.

%% @private
read_packet_data(_SPI, 0) ->
    [];
read_packet_data(SPI, Len) ->
    {ok, Datum} = read_register(SPI, ?REG_FIFO),
    [Datum | read_packet_data(SPI, Len - 1)].

%% @private
read_register({SPI, DeviceName}, Address) ->
    spi:read_at(SPI, DeviceName, Address bor 16#80, 8).

%% @private
write_register({SPI, DeviceName}, Address, Data) ->
    {ok, _} = spi:write_at(SPI, DeviceName, Address bor 16#80, 8, Data),
    ok.


%%%
%%% debugging
%%%

get_registers() ->
    [
        {reg_op_mode, 16#01, fun to_hex/1},
        {reg_fr_msb, 16#06, fun to_hex/1},
        {reg_fr_mid, 16#07, fun to_hex/1},
        {reg_fr_lsb, 16#08, fun to_hex/1},
        {reg_pa_config, 16#09, fun to_hex/1},
        {reg_pa_ramp, 16#0A, fun to_hex/1},
        {reg_pa_ocp, 16#0B, fun to_hex/1},
        {reg_lna, 16#0C, fun to_hex/1},
        {reg_fifo_addr_ptr, 16#0D, fun to_hex/1},
        {reg_fifo_tx_base_addr, 16#0E, fun to_hex/1},
        {reg_fifo_rx_base_addr, 16#0F, fun to_hex/1},
        {reg_fifo_rx_current_addr, 16#10, fun to_hex/1},
        {reg_irq_flags_mask, 16#11, fun to_hex/1},
        {reg_irq_flags, 16#12, fun to_hex/1},
        {reg_rx_nb_bytes, 16#13, fun to_hex/1},
        {reg_header_cnt_value_msb, 16#14, fun to_hex/1},
        {reg_header_cnt_value_lsb, 16#15, fun to_hex/1},
        {reg_packet_cnt_value_msb, 16#16, fun to_hex/1},
        {reg_packet_cnt_value_lsb, 16#17, fun to_hex/1},
        {reg_modem_stat, 16#18, fun to_hex/1},
        {reg_pkt_snr_value, 16#19, fun to_hex/1},
        {reg_pkr_rssi_value, 16#1A, fun to_hex/1},
        {reg_rssi_value, 16#1B, fun to_hex/1},
        {reg_hop_channel, 16#1C, fun to_hex/1},
        {reg_modem_config_1, 16#1D, fun to_hex/1},
        {reg_modem_config_2, 16#1E, fun to_hex/1},
        {reg_symb_timeout_lsb, 16#1F, fun to_hex/1},
        {reg_preamble_msb, 16#20, fun to_hex/1},
        {reg_preamble_lsb, 16#21, fun to_hex/1},
        {reg_payload_length, 16#22, fun to_hex/1},
        {reg_max_payload_length, 16#23, fun to_hex/1},
        {reg_hop_period, 16#24, fun to_hex/1},
        {reg_fifo_rx_byte_addr, 16#25, fun to_hex/1},
        {reg_modem_config_3, 16#26, fun to_hex/1},
        {reg_fei_msb, 16#28, fun to_hex/1},
        {reg_fei_mid, 16#29, fun to_hex/1},
        {reg_fei_lsb, 16#2A, fun to_hex/1},
        {reg_rssi_wideband, 16#2C, fun to_hex/1},
        {reg_detect_optimize, 16#31, fun to_hex/1},
        {reg_invert_iq, 16#33, fun to_hex/1},
        {reg_detection_threshold, 16#37, fun to_hex/1},
        {reg_sync_word, 16#39, fun to_hex/1}
    ].

do_dump_registers(SPI) ->
    ?TRACE("do_dump_registers", []),
    Mode = get_mode(SPI),
    Registers = get_registers(),
    try
        set_mode(SPI, sleep),
        [
            begin
                {ok, Value} = read_register(SPI, Address),
                {Name, Parse(Value)}
            end
            || {Name, Address, Parse} <- Registers
        ]
    after
        set_mode(SPI, Mode)
    end.

do_sleep(SPI) ->
    ?TRACE("do_sleep", []),
    set_mode(SPI, sleep).

to_hex(Value) ->
    "0x" ++ codec:encode(Value, hex).


% parse_op_mode(Value) ->
%         #{
%             long_range_mode => (Value band 16#80) bsr 7
%         }.
