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
%%-----------------------------------------------------------------------------
%% @doc ADC support.
%%
%% Use this module to take ADC readings on pins 32-39 on an ESP32 device.
%% @end
%%-----------------------------------------------------------------------------
-module(adc).

-export([
    start/1, start/2, stop/1, read/1, read/2
]).
-export([config_width/1, config_channel_attenuation/2, take_reading/4]). %% internal nif APIs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-type adc() :: term().
-type adc_pin() :: 32..39.
-type options() :: [option()].
-type bit_width() :: bit_9 | bit_10 | bit_11 | bit_12.
-type attenuation() :: db_0 | db_2_5 | db_6 | db_11.
-type option() :: {bit_width, bit_width()} | {attenuation, attenuation()}.

-type read_options() :: [read_option()].
-type read_option() :: raw | voltage | {samples, pos_integer()}.

-type raw_value() :: 0..4095 | undefined.
-type voltage_reading() :: 0..3300 | undefined.
-type reading() :: {raw_value(), voltage_reading()}.

-define(DEFAULT_OPTIONS, [{bit_width, bit_12}, {attenuation, db_0}]).
-define(DEFAULT_SAMPLES, 64).
-define(DEFAULT_READ_OPTIONS, [raw, voltage, {samples, ?DEFAULT_SAMPLES}]).

-record(state, {
    pin :: adc_pin(),
    bit_width :: bit_width(),
    attenuation :: attenuation()
}).


%%-----------------------------------------------------------------------------
%% @param   Pin     pin from which to read ADC
%% @returns ok | {error, Reason}
%% @equiv   start(Pin, [{bit_width, bit_12}, {attenuation, db_0}])
%% @doc     Start an ADC.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Pin::adc_pin()) -> {ok, adc()} | {error, Reason::term()}.
start(Pin) ->
    start(Pin, ?DEFAULT_OPTIONS).

%%-----------------------------------------------------------------------------
%% @param   Pin         pin from which to read ADC
%% @param   Options     extra options
%% @returns ok | {error, Reason}
%% @doc     Start a ADC.
%%
%% Readings will be taken from the specified pin.  If the pin in not in the
%% range of 32..39, a badarg exception will be raised.
%%
%% Options may specify the bit width and attenuation.
%%
%% Note.  The bit width is applied globally for all ADC pins.  Use this
%% option with caution, as setting it may have side effects if reading
%% from multiple pins.
%%
%% Use the returned reference in subsequent ADC operations.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Pin::adc_pin(), Options::options()) -> {ok, adc()} | {error, Reason::term()}.
start(Pin, Options) ->
    gen_server:start(?MODULE, [Pin, Options], []).

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Stop the specified ADC.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(ADC::adc()) -> ok.
stop(ADC) ->
    gen_server:call(ADC, stop).

%%-----------------------------------------------------------------------------
%% @param   Pin         pin from which to read ADC
%% @returns {ok, {RawValue, MilliVoltage}} | {error, Reason}
%% @equiv   read(ADC, [raw, voltage, {samples, 64}])
%% @doc     Take a reading from the pin associated with this ADC.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec read(ADC::adc()) -> {ok, reading()} | {error, Reason::term()}.
read(ADC) ->
    read(ADC, ?DEFAULT_READ_OPTIONS).

%%-----------------------------------------------------------------------------
%% @param   Pin         pin from which to read ADC
%% @param   ReadOptions extra options
%% @returns {ok, {RawValue, MilliVoltage}} | {error, Reason}
%% @doc     Take a reading from the pin associated with this ADC.
%%
%% The Options parameter may be used to specify the behavior of the read
%% operation.
%%
%% If the ReadOptions contains the atom `raw', then the raw value will be returned
%% in the first element of the returned tuple.  Otherwise, this element will be the
%% atom `undefined'.
%%
%% If the ReadOptions contains the atom `voltage', then the millivoltage value will be returned
%% in the second element of the returned tuple.  Otherwise, this element will be the
%% atom `undefined'.
%%
%% You may specify the number of samples to be taken and averaged over using the tuple
%% `{samples, Samples::pos_integer()}'.
%% @end
%%-----------------------------------------------------------------------------
-spec read(ADC::adc(), ReadOptions::read_options()) -> {ok, reading()} | {error, Reason::term()}.
read(ADC, ReadOptions) ->
    gen_server:call(ADC, {read, ReadOptions}).


%%
%% gen_server API
%%

%% @hidden
init([Pin, Options]) ->
    BitWidth = proplists:get_value(bit_width, Options, bit_12),
    case adc:config_width(BitWidth) of
        ok -> ok;
        {error, R1} ->
            throw({config_width, R1})
    end,
    Attenuation = proplists:get_value(attenuation, Options, db_0),
    case adc:config_channel_attenuation(Pin, Attenuation) of
        ok -> ok;
        {error, R2} ->
            throw({config_channel_attenuation, R2})
    end,
    {ok, #state{
        pin=Pin, bit_width=BitWidth, attenuation=Attenuation
    }}.

%% @hidden
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({read, ReadOptions}, _From, State) ->
    Reading = adc:take_reading(State#state.pin, ReadOptions, State#state.bit_width, State#state.attenuation),
    {reply, {ok, Reading}, State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal operations
%%

%% @hidden
config_width(_BitWidth) ->
    throw(nif_error).

%% @hidden
config_channel_attenuation(_Pin, _Attenuation) ->
    throw(nif_error).

%% @hidden
take_reading(_Pin, _ReadOptions, _BitWidth, _Attenuation) ->
    throw(nif_error).
