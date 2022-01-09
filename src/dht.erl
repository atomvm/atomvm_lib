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
%% @doc DHT11 and DHT22 support.
%%
%%      Use this module to read temperature and humidity readings from
%%      a DHT11 or DHT22 sensor.
%% @end
%%-----------------------------------------------------------------------------
-module(dht).

-export([
    start/1, stop/1,
    take_reading/1
]).
-export([read/1]). %% internal nif APIs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-behaviour(gen_server).

-type pin() :: non_neg_integer().
-type device() :: dht_11 | dht_22.
-type config() :: #{
    pin => pin(),
    device => device()
}.
-type dht() :: pid().
-type temp() :: non_neg_integer().
-type temp_fractional() :: non_neg_integer().
-type hum() :: non_neg_integer().
-type hum_fractional() :: non_neg_integer().
-type measurement() :: {temp(), temp_fractional(), hum(), hum_fractional()}.

-define(DEFAULT_CONFIG, #{device => dht_11}).
-define(MIN_TIME_BETWEEN_MEASUREMENTS_DHT11_MS, 1000).
-define(MIN_TIME_BETWEEN_MEASUREMENTS_DHT22_MS, 2000).

-record(state, {
    pin :: pin(),
    device :: device(),
    last_measurement
}).

%%-----------------------------------------------------------------------------
%% @param   Config     DHT configuration
%% @returns {ok, DHT} | {error, Reason}
%% @doc     Start a DHT.
%%
%%
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config::config()) -> {ok, dht()} | {error, Reason::term()}.
start(Config) when is_map(Config) ->
    gen_server:start(?MODULE, validate_config(add_defaults(Config)), []).

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Stop the specified DHT.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(DHT::dht()) -> ok.
stop(DHT) ->
    gen_server:stop(DHT).

%%-----------------------------------------------------------------------------
%% @param   Pin         pin from which to read DHT
%% @param   Options     extra options expressed as a
%% @returns ok | {error, Reason}
%% @doc     Take a reading.
%%
%%          This function will return a measurement expressed as a 4-tuple
%%          of elements, including:
%%          <ol>
%%              <li>temperature whole number part</li>
%%              <li>temperature fractional part</li>
%%              <li>humidity whole number part</li>
%%              <li>humidity fractional part</li>
%%          </ol>
%%          Temperature is measured in degrees celcius.  Relative humidity
%%          is expressed as a percentage.
%%
%%          Measurements cannot be taken more frequently than once every
%%          second, for DHT11 devices, and every 2 seconds, for DHT22 devices.
%%          This operation will block until enough time has elapsed before
%%          taking the next measurement.
%% @end
%%-----------------------------------------------------------------------------
-spec take_reading(DHT::dht()) -> {ok, measurement()} | {error, Reason::term()}.
take_reading(DHT) ->
    gen_server:call(DHT, take_reading).

%%
%% Nif implementation
%%

%% @hidden
-spec read(Pin::pin()) -> {ok, binary()} | {error, Reason::term()}.
read(_Pin) ->
    throw(nif_error).


%%
%% gen_server API
%%

%% @hidden
init(#{pin := Pin, device := Device} = _Config) ->
    {ok, #state{pin=Pin, device=Device}}.

%% @hidden
handle_call(take_reading, _From, State) ->
    ?TRACE("Taking a reading.", []),
    {LastMeasurmentTime, Response} = do_measure(State#state.pin, State#state.device, State#state.last_measurement),
    {reply, Response, State#state{last_measurement=LastMeasurmentTime}};
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

%% @private
do_measure(Pin, Device, LastMeasurement) ->
    maybe_sleep(Device, LastMeasurement),
    {erlang:timestamp(), do_measure(Pin, Device)}.

%% @private
do_measure(Pin, Device) ->
    case dht:read(Pin) of
        {ok, Measurement} ->
            <<A:8, B:8, C:8, D:8, Parity:8>> = Measurement,
            case (A + B + C + D) rem 256 of
                Parity ->
                    {ok, get_measurement({A, B, C, D}, Device)};
                _ ->
                    {error, {checksum_error, {A, B, C, D}, Parity}}
            end;
        Error ->
            Error
    end.

%% @private
get_measurement({A, B, C, D}, dht_11) ->
    ?TRACE("DHT11 measurement: ~p", [{A, B, C, D}]),
    {C, D, A, B};
get_measurement({A, B, C, D}, dht_22) ->
    ?TRACE("DHT22 measurement: ~p", [{A, B, C, D}]),
    H = (A bsl 8) bor B,
    F = case C band 16#80 of 0 -> 1; 16#80 -> -1 end,
    T = ((C band 16#7F) bsl 8) bor D,
    {{F * (T div 10), T rem 10}, {H div 10, H rem 10}}.

%% @private
maybe_sleep(_Device, undefined) ->
    ok;
maybe_sleep(Device, LastMeasurement) ->
    TimeSinceLastMeasurementMs = timestamp_util:delta_ms(erlang:timestamp(), LastMeasurement),
    MinTimeBetweenMeasurements = case Device of
        dht_11 -> ?MIN_TIME_BETWEEN_MEASUREMENTS_DHT11_MS;
        dht_22 -> ?MIN_TIME_BETWEEN_MEASUREMENTS_DHT22_MS
    end,
    case TimeSinceLastMeasurementMs < MinTimeBetweenMeasurements of
        true ->
            SleepMs = MinTimeBetweenMeasurements - TimeSinceLastMeasurementMs,
            ?TRACE("Sleeping ~pms", [SleepMs]),
            timer:sleep(SleepMs);
        _ -> ok
    end.

%% @private
add_defaults(Config) ->
    maps:merge(?DEFAULT_CONFIG, Config).

%% @private
validate_config(Config) when is_map(Config) ->
    validate_pin(maps:get(pin, Config)),
    validate_device(maps:get(device, Config)),
    Config;
validate_config(_) ->
    throw(badarg).

%% @private
validate_pin(Pin) when is_integer(Pin) ->
    ok;
validate_pin(_) ->
    throw(badarg).

%% @private
validate_device(dht_11) ->
    ok;
validate_device(dht_22) ->
    ok;
validate_device(_) ->
    throw(badarg).
