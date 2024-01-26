%%
%% Copyright (c) dushin.net
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
-module(sht3x).

%%-----------------------------------------------------------------------------
%% @doc An AtomVM I2C driver for the Sensirion SHT3x series of digital temperature
%% and humidity sensors.
%%
%% The Sensirion SHT3[0|1|5] is a small sensor that can read temperature and humidity.
%% The chipset supports the I2C interfaces, with varying levels of accuracy.
%% This driver uses the AtomVM I2C interface for communicating with the SHT31.
%% This means you can take temperature and humidity readings using two GPIO
%% pins on your ESP32.
%%
%% Developers interact with this driver by starting an instance, specifying pins for
%% the I2C data and clock pins.  Starting an instance of the driver yeilds a reference
%% that can be used in subsequent calls.
%%
%% The primary operation in this module is the take_reading/1 function, which takes
%% a reference to a SHT31 driver, and returns a reading expressed as a tuple containing
%% the temperature (in degrees celcius) and relative humidity (as a percentage).
%%
%% Note.  The SHT31 sensor is a fairly dynamic sensor and can be used for
%% many different applications (e.g., weather collection, gaming, drones, etc).
%% The primary use-case for this driver is weather collection, which is assumed
%% to be a low frequency operation.  Some of the SHT31 applications may require
%% additional support in this driver, which would be relatively straightforward
%% to support in future versions.
%%
%% Further information about the Sensirion SHT3x can be found in the reference
%% documentation:
%% https://www.sensirion.com/fileadmin/user_upload/customers/sensirion/Dokumente/2_Humidity_Sensors/Datasheets/Sensirion_Humidity_Sensors_SHT3x_Datasheet_digital.pdf
%%
%% @end
%%-----------------------------------------------------------------------------

-behaviour(gen_server).

-export([start/1, start/2, start_link/1, start_link/2, stop/1, take_reading/1, soft_reset/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-type repeatability() :: high | medium | low.
-type clock_stretching() :: enabled | disabled.
-type options() :: #{
    repeatability => repeatability(),
    clock_stretching => clock_stretching()
}.
-type sht() :: pid().

-type fractional() :: 0..99.
-type temp_reading() :: {integer(), fractional()}.
-type humidity_reading() :: {integer(), fractional()}.
-type reading() :: {temp_reading(), humidity_reading()}.

-define(SHT31_BASE_ADDR, 16#44).

-define(DEFAULT_OPTIONS, #{
    repeatability => high,
    clock_stretching => disabled
}).

-record(state, {
    i2c_bus,
    options
}).


%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @returns {ok, SHT} on success, or {error, Reason}, on failure
%% @equiv   start(SDAPin, SCLPin, [])
%% @doc     Start the SHT31 driver.
%% @end
%%-----------------------------------------------------------------------------
-spec start(I2CBus::i2c_bus:i2c_bus()) -> {ok, SHT::sht()} | {error, Reason::term()}.
start(I2CBus) ->
    start(I2CBus, maps:new()).

%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @param   Options additional driver options
%% @returns {ok, SHT} on success, or {error, Reason}, on failure
%% @doc     Start the SHT31 driver.
%%
%% This operation will start the SHT driver.  Use the returned reference
%% in subsequent operations, such as for taking a reading.
%%
%% The Options parameter may be used to fine-tune behavior of the sensor,
%% but the default values should be sufficient for weather-station based
%% scenarios.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec start(I2CBus::i2c_bus:i2c_bus(), Options::options()) -> {ok, SHT::sht()} | {error, Reason::term()}.
start(I2CBus, Options) ->
    gen_server:start(?MODULE, {I2CBus, maps:merge(?DEFAULT_OPTIONS, Options)}, []).


%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @returns {ok, SHT} on success, or {error, Reason}, on failure
%% @equiv   start(SDAPin, SCLPin, [])
%% @doc     Start the SHT31 driver.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(I2CBus::i2c_bus:i2c_bus()) -> {ok, SHT::sht()} | {error, Reason::term()}.
start_link(I2CBus) ->
    start_link(I2CBus, maps:new()).

%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @param   Options additional driver options
%% @returns {ok, SHT} on success, or {error, Reason}, on failure
%% @doc     Start the SHT31 driver.
%%
%% This operation will start the SHT driver.  Use the returned reference
%% in subsequent operations, such as for taking a reading.
%%
%% The Options parameter may be used to fine-tune behavior of the sensor,
%% but the default values should be sufficient for weather-station based
%% scenarios.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(I2CBus::i2c_bus:i2c_bus(), Options::options()) -> {ok, SHT::sht()} | {error, Reason::term()}.
start_link(I2CBus, Options) ->
    gen_server:start_link(?MODULE, {I2CBus, maps:merge(?DEFAULT_OPTIONS, Options)}, []).

%%-----------------------------------------------------------------------------
%% @param       SHT a reference to the SHT instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc Stop the SHT31 driver.
%%
%% Note. This function is not well tested and its use may result in a memory leak.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(SHT::sht()) -> ok | {error, Reason::term()}.
stop(SHT) ->
    gen_server:stop(SHT).

%%-----------------------------------------------------------------------------
%% @param       SHT a reference to the SHT instance created via start
%% @returns     {ok, Reading} if successful; {error, Reason}, otherwise
%% @doc Take a reading from the sensor.
%%
%% This function will take a reading from the attached SHT31 sensor.
%%
%% The return value is a 2-ary tuple containing the temperature
%% and humidity readings from the sensor.
%%
%% Temperature is a measurement expressed in degrees celsius.
%%
%% Humidity is a percentage in the range [0.0 .. 100.0]
%%
%% Both values are represented by floating point numbers.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec take_reading(SHT::sht()) -> {ok, Reading::reading()} | {error, Reason::term()}.
take_reading(SHT) ->
    gen_server:call(SHT, take_reading).

%%-----------------------------------------------------------------------------
%% @param       SHT a reference to the SHT instance created via start
%% @returns     ok
%% @doc         Perform a soft reset of the SHT31 sensor.
%%
%%              A soft reset will set all of the registers in the device
%%              to values in section 5.3 of the reference documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec soft_reset(SHT::sht()) -> ok.
soft_reset(SHT) ->
    gen_server:call(SHT, soft_reset).

%%
%% gen_server API
%%

%% @hidden
init({I2CBus, Options}) ->
    ?TRACE("Initializing sht3x instance ~p with I2CBus ~p and Options ~p", [self(), I2CBus, Options]),
    {ok, #state{
        i2c_bus = I2CBus,
        options = Options
    }}.

%% @hidden
handle_call(take_reading, _From, State) ->
    ?TRACE("Taking reading ...", []),
    Reply = do_take_reading(State),
    {reply, Reply, State};
handle_call(soft_reset, _From, State) ->
    %% TODO fix
    %% write_byte(State#state.i2c_bus, ?SHT31_REGISTER_SOFT_RESET, 16#01)
    {reply, {error, unimplemented}, State};
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
%% Internal functions
%%

%% @private
do_take_reading(State) ->
    #state{
        i2c_bus = I2CBus
    } = State,
    %%
    %% Tell the sensor to take a temp and humidity reading
    %%
    {MSB, LSB} = create_measurement_command(State#state.options),
    ?TRACE("measurement_command: ~p", [{MSB, LSB}]),
    ok = i2c_bus:write_bytes(I2CBus, ?SHT31_BASE_ADDR, <<MSB:8, LSB:8>>),
    timer:sleep(20),
    %%
    %% Read the data in memory.
    %%
    case read_bytes(I2CBus, 6) of
        error ->
            ?TRACE("Bad reading!", []),
            {error, bad_reading};
        {ok, Bytes} ->
            {ok, to_reading(Bytes)}
    end.

to_reading(Bytes) ->
    ?TRACE("to_reading: ~p", [Bytes]),
    <<
        TempReading:16, _TempChecksum:8,
        HumidityReading:16, _Humidityksum:8
    >> = Bytes,
    ?TRACE("TempReading: ~p", [TempReading]),
    ?TRACE("HumidityReading: ~p", [HumidityReading]),
    %% TODO compute/varify checksum

    %%
    %% Normalize into {integer, fractional} values.
    %%
    Reading = {
        compute_temp(TempReading),
        compute_humidity(HumidityReading)
    },
    ?TRACE("Reading: ~p", [Reading]),
    Reading.

%% @private
create_measurement_command(Options) ->
    #{
        repeatability := Repeatability,
        clock_stretching := ClockStretching
    } = Options,
    {get_msb(ClockStretching), get_lsb(ClockStretching, Repeatability)}.

%% @private
get_msb(enabled) ->
    16#2C;
get_msb(disabled) ->
    16#24.

%% @private
get_lsb(enabled, high) ->
    16#06;
get_lsb(enabled, medium) ->
    16#0D;
get_lsb(enabled, low) ->
    16#10;
get_lsb(disabled, high) ->
    16#0;
get_lsb(disabled, medium) ->
    16#0B;
get_lsb(disabled, low) ->
    16#16.

%% @private
read_bytes(I2CBus, Len) ->
    ?TRACE("Reading bytes off I2CBus ~p Len ~p ...", [I2CBus, Len]),
    i2c_bus:read_bytes(I2CBus, ?SHT31_BASE_ADDR, Len).

%% @private
compute_temp(TempReading) ->
    -45 + 175 * (TempReading / 65535).

%% @private
compute_humidity(HumidityReading) ->
    100 * (HumidityReading / 65535).
