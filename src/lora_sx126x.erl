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
-module(lora_sx126x).

%%%
%%% @doc
%%% An SPI driver for the LoRa (SX126X) chipset.
%%%
%%% This module can be used to send and receive messages using LoRa modulation.
%%% Currently, this module only supports point-to-point communications.  This
%%% module does not support LoRaWAN.
%%%
%%% References
%%% SemTech SX126x data sheet: https://semtech.my.salesforce.com/sfc/p/#E0000000JelG/a/2R0000001Rbr/6EfVZUorrpoKFfvaF_Fkpgp5kzjiNyiAbqcpqh9qSjE
%%% SemTech reference implementation: https://github.com/Lora-net/sx126x_driver
%%% Python implementation (for interoperability testing): https://github.com/ehong-tl/micropySX126X
%%%
%%% @end

%% Internal Lora provider API
-export([start/1, start_link/1, stop/1, broadcast/2, sleep/1]).

%% gen_statem
-export([init/1, waiting_to_receive/3, waiting_tx_done/3, terminate/3]).

% -behavior(gen_statem).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").


%%
%% Lora Provider API
%%

%% @hidden
start(Config) ->
    gen_statem:start(?MODULE, Config, []).

%% @hidden
start_link(Config) ->
    gen_statem:start_link(?MODULE, Config, []).

%% @hidden
stop(Lora) ->
    gen_statem:stop(Lora).

%% @hidden
broadcast(Lora, Message) ->
    gen_statem:call(Lora, {broadcast, Message}).

%% @hidden
sleep(Lora) ->
    gen_statem:call(Lora, sleep).

%%%
%%% gen_statem implementation
%%%

-record(state, {
    spi,
    config,
    irq,
    busy_pin,
    pending
}).

%% @hidden
init(Config) ->
    ?TRACE("init(~p)", [Config]),
    SPI = {maps:get(spi, Config), maps:get(device_name, Config)},

    ok = maybe_reset(maps:get(reset, Config, undefined)),

    BusyPin = maps:get(busy, Config, undefined),
    gpio:set_pin_mode(BusyPin, input),
    case maybe_wait_until_not_busy(BusyPin, 1000) of
        ok ->
            case init_lora(SPI, Config) of
                ok ->

                    GPIO = gpio:start(),
                    ok = maybe_set_interrupt(GPIO, rising, maps:get(irq, Config, undefined)),

                    set_recv_mode(SPI, Config),

                    State = #state{
                        spi = SPI,
                        config = Config,
                        irq = maps:get(irq, Config, undefined),
                        busy_pin = BusyPin
                    },
                    {ok, waiting_to_receive, State};
                LoraError ->
                    {stop, LoraError}
            end;
        Error ->
            {stop, Error}
    end.

%%
%% gen_statem state machine functions
%%

%%
%% waiting_to_receive state
%%  We are waiting to receive messages
%%  * If we get a request to broadcast a message, we broadcast the message
%%    and go into the waiting_tx_done state, to wait for the TX_DONE IRQ
%%  * Any unknown calls are rejected with an error
%%  * Any unknown messages are silently discarded
%%

%% @hidden
waiting_to_receive(info, {gpio_interrupt, Pin}, #state{irq = Pin} = State) ->
    ?TRACE("gpio_interrupt IRQ Pin=~p Level=~p in waiting_to_receive state", [Pin, gpio:digital_read(Pin)]),
    do_receive(State),
    {next_state, waiting_to_receive, State};
waiting_to_receive({call, From}, {broadcast, Message}, State) ->
    case maybe_wait_until_not_busy(State#state.busy_pin, 10) of
        ok ->
            ok = do_broadcast(State#state.spi, State#state.config, Message),
            {next_state, waiting_tx_done, State#state{pending=From}, [{state_timeout, 9000, {error, tx_timeout}}]};
        Error ->
            {next_state, waiting_to_receive, State#state{pending=From}, [{reply, From, Error}]}
    end;
waiting_to_receive({call, From}, sleep, State) ->
    do_sleep(State#state.spi),
    {next_state, sleep, State, [{reply, From, ok}]};
waiting_to_receive({call, From}, _Request, State) ->
    ?TRACE("Unhandled call in waiting_to_receive state.  Request: ~p", [Request]),
    {next_state, waiting_tx_done, State, [{reply, From, {error, unknown_request}}]};
waiting_to_receive(_EventType, _Request, State) ->
    ?TRACE("Unhandled message in waiting_to_receive state.  EventType: ~p Request: ~p", [EventType, Request]),
    {next_state, waiting_to_receive, State}.

%%
%% waiting_tx_done state
%%  We are waiting for a signal that transmission of a message has completed
%%  * If we get the expected interrupt on the IRQ pin, then reply ok to caller
%%    and go back into the waiting_to_receive state
%%  * If we timeout waiting, then reply to caller with a timeout error
%%  * Any calls are rejected with an error
%%  * Any unknown messages are silently discarded
%%

waiting_tx_done(info, {gpio_interrupt, Pin}, #state{irq = Pin} = State) ->
    ?TRACE("gpio_interrupt IRQ Pin=~p Level=~p in waiting_tx_done state", [Pin, gpio:digital_read(Pin)]),
    case maybe_wait_until_not_busy(State#state.busy_pin, 1000) of
        ok ->
            % ok;
            set_recv_mode(State#state.spi, State#state.config);
        _E ->
            io:format("Error!  Unable to get into receive mode!~n")
    end,
    NewState = State#state{pending=undefined},
    {next_state, waiting_to_receive, NewState, [{reply, State#state.pending, ok}]};
waiting_tx_done(state_timeout, ErrorMessage, State) ->
    ?TRACE("Timed out waiting for tx_done IRQ.  Error message: ~p", [ErrorMessage]),
    % init_lora(State#state.spi, State#state.config),
    set_recv_mode(State#state.spi, State#state.config),
    NewState = State#state{pending=undefined},
    ?TRACE("going back into receive state.  Will reply with ErrorMessage=~p", [ErrorMessage]),
    {next_state, waiting_to_receive, NewState, [{reply, State#state.pending, ErrorMessage}]};
waiting_tx_done({call, From}, _Request, State) ->
    ?TRACE("Illegal call in waiting_tx_done state.  Request: ~p", [Request]),
    {next_state, waiting_tx_done, State, [{reply, From, {error, busy_waiting_tx_done}}]};
waiting_tx_done(_EventType, _Request, State) ->
    ?TRACE("Unhandled message in waiting_tx_done state.  EventType: ~p Request: ~p", [EventType, Request]),
    {next_state, waiting_tx_done, State}.

%% @hidden
terminate(_Reason, _CurrentState, _State) ->
    ok.

%%%
%%% internal functions
%%%

%% @private
init_lora(SPI, Config) ->

    ok = sx126x_cmd:set_standby_xosc(SPI),
    ok = sx126x_cmd:set_buffer_base_address(SPI,
        maps:get(tx_base_address, Config, 16#00),
        maps:get(rx_base_address, Config, 16#00)
    ),

    sx126x_cmd:clear_device_errors(SPI),
    ok = sx126x_cmd:set_lora_packet_type(SPI),
    ?TRACE("packet type: ~p", [sx126x_cmd:get_packet_type(SPI)]),

    ok = sx126x_cmd:set_rx_tx_fallback_mode(SPI, rc),
    % ok = sx126x_cmd:set_cad_params(SPI),
    ok = sx126x_cmd:clear_irq_status(SPI),
    ok = sx126x_cmd:clear_irq_params(SPI),
    ok = sx126x_cmd:calibration_all(SPI),

    %% For some reason this appears to be needed
    %% on the NiceRF SX126x in order to send messages
    ok = sx126x_cmd:set_dio3_as_tcxoc_ctl(SPI),

    ok = sx126x_cmd:set_modulation_params(SPI,
        maps:get(spreading_factor, Config, sf_7),
        maps:get(bandwidth, Config, bw_125khz),
        maps:get(coding_rate, Config, cr_4_8),
        maps:get(ldro, Config, off)
    ),

    ok = sx126x_cmd:set_sync_word(SPI,
        maps:get(sync_word, Config, 16#12)
    ),

    % ok = sx126x_cmd:set_dio2_as_rf_switch_ctl(SPI, enable),
    % ok = sx126x_cmd:set_regulator_mode(SPI),

    ok = sx126x_cmd:set_packet_params(SPI,
        maps:get(preamble_length, Config, 8),
        maps:get(header_mode, Config, explicit),
        maps:get(max_payload_length, Config, 16#FF),
        maps:get(enable_crc, Config, true),
        maps:get(invert_iq, Config, false)
    ),

    ok = sx126x_cmd:set_frequency(SPI, maps:get(frequency, Config, freq_915mhz)),

    ok = sx126x_cmd:set_pa_config(SPI, sx1262),
    ok = sx126x_cmd:set_tx_params(SPI, maps:get(tx_power, Config, 2)),

    ?TRACE("device_errors: ~p", [sx126x_cmd:get_device_errors(SPI)]),

    ok.

%% @private
set_recv_mode(SPI, Config) ->
    ?TRACE("Setting mode to recv", []),
    sx126x_cmd:set_standby_xosc(SPI),

    %% This seems to be required in order to receive
    %% using the NiceRF SX1262 module (?)
    ok = sx126x_cmd:calibrate_image(SPI),

    ok = sx126x_cmd:set_rx_irq(SPI),

    ok = sx126x_cmd:set_buffer_base_address(SPI,
        maps:get(tx_base_address, Config, 16#00),
        maps:get(rx_base_address, Config, 16#00)
    ),

    ok = sx126x_cmd:clear_irq_status(SPI),

    ok = sx126x_cmd:set_packet_params(SPI,
        maps:get(preamble_length, Config, 8),
        maps:get(header_mode, Config, explicit),
        maps:get(max_payload_length, Config, 16#FF),
        maps:get(enable_crc, Config, true),
        maps:get(invert_iq, Config, false)
    ),

    ok = sx126x_cmd:set_rx(SPI),
    ok.

%% @private
maybe_set_interrupt(_GPIO, _Trigger, undefined) ->
    ok;
maybe_set_interrupt(GPIO, Trigger, Pin) ->
    ?TRACE("maybe_set_interrupt on pin ~p for trigger ~p", [Pin, Trigger]),
    gpio:set_pin_pull(Pin, down),
    gpio:set_int(GPIO, Pin, Trigger).

%% @private
maybe_reset(undefined) ->
    ?TRACE("Reset pin not set.  Skipping...", []),
    ok;
maybe_reset(ResetPin) ->
    ?TRACE("Resetting on pin ~p ...", [ResetPin]),
    ok = gpio:set_pin_mode(ResetPin, output),
    ok = gpio:digital_write(ResetPin, high),
    timer:sleep(1),
    ok = gpio:digital_write(ResetPin, low),
    timer:sleep(1),
    ok = gpio:digital_write(ResetPin, high),
    ok.

%% @private
maybe_wait_until_not_busy(_BusyPin, 0) -> {error, timeout_busy};
maybe_wait_until_not_busy(undefined, _I) -> ok;
maybe_wait_until_not_busy(BusyPin, I) ->
    case gpio:digital_read(BusyPin) of
        low ->
            ?TRACE("Pin ~p is not busy.", [BusyPin]),
            ok;
        _ ->
            ?TRACE("Pin ~p still busy.  Waiting some more.", [BusyPin]),
            % timer:sleep(10),
            maybe_wait_until_not_busy(BusyPin, I - 1)
    end.

%%%
%%% send
%%%

-define(MAX_PACKET_LEN, 255).

do_broadcast(SPI, Config, Data) ->
    Len = erlang:byte_size(Data),
    case Len > ?MAX_PACKET_LEN of
        true ->
            {error, payload_too_large};
        _ ->
            % init_lora(SPI, Config),
            %%
            %% prepare
            %%
            ?TRACE("preparing transmit...", []),

            ok = sx126x_cmd:set_standby_xosc(SPI),

            ok = sx126x_cmd:set_packet_params(SPI,
                maps:get(preamble_length, Config, 8),
                maps:get(header_mode, Config, explicit),
                Len,
                maps:get(enable_crc, Config, true),
                maps:get(invert_iq, Config, false)
            ),

            ok = sx126x_cmd:clear_irq_status(SPI),
            ok = sx126x_cmd:set_tx_irq(SPI),

            ok = sx126x_cmd:set_buffer_base_address(SPI, 0, 0),

            ?TRACE("writing data to FIFO (len=~p): ~p", [byte_size(Data), Data]),
            _Response = sx126x_cmd:write_buffer(SPI, Data),

            %% fix sensitivity
            % write_command(SPI, 16#1D, <<16#8,16#89>>),
            % write_command(SPI, 16#0D, <<16#8,16#89,16#4>>),

            ?TRACE("Populated buffer and setting TX mode to transmit...", []),
            ok = sx126x_cmd:set_tx(SPI),

            ok
    end.

%%%
%%% receive
%%%

%% @private
do_receive(State) ->
    SPI = State#state.spi,
    Config = State#state.config,

    ?TRACE("Receiving message", []),

    ok = sx126x_cmd:set_standby_xosc(SPI),

    IRQStatus = sx126x_cmd:get_irq_status(SPI),
    ?TRACE("IRQStatus: ~p", [IRQStatus]),

    try
        case lists:member(crc_err, IRQStatus) of
            true ->
                io:format("CRC error on receive!  Ignoring message.~n"),
                {error, crc_err};
            false ->
                {PayloadLengthRx, RxStartBufferPointer} = sx126x_cmd:get_rx_buffer_status(SPI),
                ?TRACE("PayloadLengthRx: ~p RxStartBufferPointer: ~p", [PayloadLengthRx, RxStartBufferPointer]),

                Payload = sx126x_cmd:read_buffer(SPI, RxStartBufferPointer, PayloadLengthRx),
                ?TRACE("Payload: ~p", [Payload]),

                %%
                %% Notify handler
                %%
                Lora = {?MODULE, self()},
                case maps:get(receive_handler, Config, undefined) of
                    undefined ->
                        ?TRACE("No receive handler configured for received message.  Ignoring message.", []),
                        ok;
                    Handler ->
                        ReplyData = case maps:get(binary, Config, true) of
                            true ->
                                Payload;
                            _ ->
                                binary_to_list(Payload)
                        end,
                        QoS = get_qos(SPI),
                        if
                            is_pid(Handler) ->
                                Handler ! {lora_receive, Lora, ReplyData, QoS};
                            is_function(Handler) ->
                                spawn(fun() -> Handler(Lora, ReplyData, QoS) end);
                            true ->
                                {error, unsupported_receive_handler}
                        end
                end
        end
    after
        set_recv_mode(SPI, State#state.config)
    end.

%% @private
get_qos(SPI) ->
    {Rssi, Snr, _SignalRssi} = sx126x_cmd:get_packet_status(SPI),
    #{
        rssi => Rssi,
        snr => Snr
    }.


%% @private
do_sleep(SPI) ->
    ?TRACE("do_sleep", []),
    sx126x_cmd:set_sleep(SPI).
