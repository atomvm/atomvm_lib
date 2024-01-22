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

%%-----------------------------------------------------------------------------
%% @doc An AtomVM I2C driver for the Bosch-Sensortec BME280.
%%
%% The BME280 is a small sensor that can read temperature, humidity, and atmospheric pressure.
%% The chipset supports I2C and SPI interfaces.  This driver uses the AtomVM I2C
%% interface for communicating with the BME280.  This means you can take temperature,
%% barometric pressure, and humidity readings using two GPIO pins on your ESP32.
%%
%% Developers interact with this driver by starting an instance, specifying pins for
%% the I2C data and clock pins.  Starting an instance of the driver yeilds a reference
%% that can be used in subsequent calls.
%%
%% The primary operation in this module is the take_reading/1 function, which takes
%% a reference to a BME280 driver, and returns a reading expressed as a tuple containing
%% the temperature (in degrees celcius), atomspheric pressure (in hectopascals) and
%% relative humidity (as a percentage).
%%
%% Functions for reading the BME280 chip ide and version, as well as doing a soft
%% reset of the device, are also supported.
%%
%% Note.  The BME280 sensor is a fairly dynamic sensor and can be used for
%% many different applications (e.g., weather collection, gaming, drones, etc).
%% The primary use-case for this driver is weather collection, which is assumed
%% to be a low frequency operation.  Some of the BME280 applications may require
%% additional support in this driver, which would be relatively straightforward
%% to support in future versions.
%%
%% Further information about the Bosch Sensortec BME280 can be found in the reference
%% documentation:
%% https://www.bosch-sensortec.com/media/boschsensortec/downloads/datasheets/bst-bme280-ds002.pdf
%%
%% @end
%%-----------------------------------------------------------------------------
-module(bme280).

-behaviour(gen_server).

-export([
    start/1,
    start/2,
    start_link/1,
    start_link/2,
    stop/1,
    take_reading/1,
    chip_id/1,
    version/1,
    soft_reset/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-type over_sampling() :: ignore | x1 | x2 | x4 | x8 | x16.
-type address() :: 16#00..16#ff.
-type mode() :: sleep | forced | normal.
-type option() ::
            {temp_oversampling, over_sampling()} |
            {address, address()} |
            {pressure_oversampling, over_sampling()} |
            {humidity_oversampling, over_sampling()} |
            {mode, mode()}.
-type options() :: [option()].
-type bme() :: pid().

-type fractional() :: 0..99.
-type temp_reading() :: {integer(), fractional()}.
-type pressure_reading() :: {integer(), fractional()}.
-type humidity_reading() :: {integer(), fractional()}.
-type reading() :: {temp_reading(), pressure_reading(), humidity_reading()}.

-define(BME280_BASE_ADDR, 16#76).
-define(BME280_REGISTER_CHIPID, 16#D0).
-define(BME280_REGISTER_VERSION, 16#D1).
-define(BME280_REGISTER_SOFT_RESET, 16#E0).
-define(BME280_REGISTER_CTL_HUM, 16#F2).
-define(BME280_REGISTER_CTL_MEAS, 16#F4).
-define(DEFAULT_OVERSAMPLING, x4).
-define(DEFAULT_MODE, forced).

-record(state, {
    i2c_bus,
    calibration_data,
    address,
    temp_oversampling,
    pressure_oversampling,
    humidity_oversampling,
    mode
}).

-record(calibration, {
    dig_T1, dig_T2, dig_T3,
    dig_P1, dig_P2, dig_P3, dig_P4, dig_P5, dig_P6, dig_P7, dig_P8, dig_P9,
    dig_H1, dig_H2, dig_H3, dig_H4, dig_H5, dig_H6
}).

%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @returns {ok, BME} on success, or {error, Reason}, on failure
%% @equiv   start(SDAPin, SCLPin, [])
%% @doc     Start the BME280 driver.
%% @end
%%-----------------------------------------------------------------------------
-spec start(I2CBus::i2c_bus:i2c_bus()) -> {ok, BME::bme()} | {error, Reason::term()}.
start(I2CBus) ->
    start(I2CBus, []).

%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @param   Options additional driver options
%% @returns {ok, BME} on success, or {error, Reason}, on failure
%% @doc     Start the BME280 driver.
%%
%% This operation will start the BME driver.  Use the returned reference
%% in subsequent operations, such as for taking a reading.
%%
%% The Options parameter may be used to fine-tune behavior of the sensor,
%% but the default values should be sufficient for weather-station based
%% scenarios.
%%
%% Notes:  The default oversampling rates for temperature, pressure, and humidity
%% is `x4'.  A sampling rate of `ignore' is not tested.
%%
%% The default `mode' is `forced'.  Other modes are not tested.
%% @end
%%-----------------------------------------------------------------------------
-spec start(I2CBus::i2c_bus:i2c_bus(), Options::options()) -> {ok, BME::bme()} | {error, Reason::term()}.
start(I2CBus, Options) ->
    gen_server:start(?MODULE, {I2CBus, Options}, []).

%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @returns {ok, BME} on success, or {error, Reason}, on failure
%% @doc     Start and link the BME280 driver.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(I2CBus::i2c_bus:i2c_bus()) -> {ok, BME::bme()} | {error, Reason::term()}.
start_link(I2CBus) ->
    start_link(I2CBus, []).

%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @param   Options additional driver options
%% @returns {ok, BME} on success, or {error, Reason}, on failure
%% @doc     Start and link the BME280 driver.
%%
%% This operation will start and link the BME driver.  Use the
%% returned reference in subsequent operations, such as for taking a
%% reading.
%%
%% The Options parameter may be used to fine-tune behavior of the sensor,
%% but the default values should be sufficient for weather-station based
%% scenarios.
%%
%% Notes:  The default oversampling rates for temperature, pressure, and humidity
%% is `x4'.  A sampling rate of `ignore' is not tested.
%%
%% The default `mode' is `forced'.  Other modes are not tested.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(I2CBus::i2c_bus:i2c_bus(), Options::options()) -> {ok, BME::bme()} | {error, Reason::term()}.
start_link(I2CBus, Options) ->
    gen_server:start_link(?MODULE, {I2CBus, Options}, []).

%%-----------------------------------------------------------------------------
%% @param       BME a reference to the BME instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc Stop the BME280 driver.
%%
%% Note. This function is not well tested and its use may result in a memory leak.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(BME::bme()) -> ok | {error, Reason::term()}.
stop(BME) ->
    gen_server:stop(BME).

%%-----------------------------------------------------------------------------
%% @param       BME a reference to the BME instance created via start
%% @returns     {ok, Reading} if successful; {error, Reason}, otherwise
%% @doc Take a reading from the sensor.
%%
%% This function will take a reading from the attached BME280 sensor.
%%
%% The return value is a 3-ary tuple containing the temperature, pressure,
%% and humidty readings from the sensor.  Each element of the tuple is a
%% pair, containing the value in integral and fractional parts.
%%
%% Temperature is expressed in degrees celsius, pressure is expressed in hectopascals,
%% and humidity is expressed as relative humidity.
%% @end
%%-----------------------------------------------------------------------------
-spec take_reading(BME::bme()) -> {ok, Reading::reading()} | {error, Reason::term()}.
take_reading(BME) ->
    gen_server:call(BME, take_reading).

%%-----------------------------------------------------------------------------
%% @param       BME a reference to the BME instance created via start
%% @returns     the chip id of the BME280 sensor
%% @doc Return the chip id of the BME280 sensor
%% @end
%%-----------------------------------------------------------------------------
-spec chip_id(BME::bme()) -> integer().
chip_id(BME) ->
    gen_server:call(BME, chip_id).

%%-----------------------------------------------------------------------------
%% @param       BME a reference to the BME instance created via start
%% @returns     the version of the BME280 sensor
%% @doc Return the version of the BME280 sensor
%% @end
%%-----------------------------------------------------------------------------
-spec version(BME::bme()) -> integer().
version(BME) ->
    gen_server:call(BME, version).

%%-----------------------------------------------------------------------------
%% @param       BME a reference to the BME instance created via start
%% @returns     ok
%% @doc         Perform a soft reset of the BME280 sensor.
%%
%%              A soft reset will set all of the registers in the device
%%              to values in section 5.3 of the reference documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec soft_reset(BME::bme()) -> ok.
soft_reset(BME) ->
    gen_server:call(BME, soft_reset).

%%
%% gen_server API
%%

%% @hidden
init({I2CBus, Options}) ->
    Address = proplists:get_value(address, Options, ?BME280_BASE_ADDR),
    Calibration = read_calibration_data(I2CBus, Address),
    ?TRACE("Calibration data: ~p~n", [Calibration]),
    {ok, #state{
        i2c_bus = I2CBus,
        calibration_data = Calibration,
        address = Address,
        temp_oversampling = normalize_oversampling(proplists:get_value(temp_oversampling, Options, ?DEFAULT_OVERSAMPLING)),
        pressure_oversampling = normalize_oversampling(proplists:get_value(pressure_oversampling, Options, ?DEFAULT_OVERSAMPLING)),
        humidity_oversampling = normalize_oversampling(proplists:get_value(humidity_oversampling, Options, ?DEFAULT_OVERSAMPLING)),
        mode = normalize_mode(proplists:get_value(mode, Options, ?DEFAULT_MODE))
    }}.

%% @private
normalize_oversampling(OverSampling) ->
    case OverSampling of
        ignore -> 16#00;
        x1 -> 16#01;
        x2 -> 16#02;
        x4 -> 16#03;
        x8 -> 16#04;
        _ ->  16#05
    end.

%% @private
normalize_mode(Mode) ->
    case Mode of
        sleep -> 16#0;
        forced -> 16#1;
        normal -> 16#3;
        _ -> 16#1
    end.

%% @hidden
handle_call(take_reading, _From, State) ->
    Reading = do_take_reading(State),
    {reply, {ok, Reading}, State};
handle_call(chip_id, _From, State) ->
    {reply, read_byte(State#state.i2c_bus, ?BME280_REGISTER_CHIPID, State#state.address), State};
handle_call(version, _From, State) ->
    {reply, read_byte(State#state.i2c_bus, ?BME280_REGISTER_VERSION, State#state.address), State};
handle_call(soft_reset, _From, State) ->
    {reply, write_byte(State#state.i2c_bus, ?BME280_REGISTER_SOFT_RESET, 16#01, State#state.address), State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(normal, State) ->
    ?TRACE("terminate(normal, ~p)", [State]),
    do_sleep(State);
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%
%% Understanding this code probably requires an understanding of the above reference documentation.
%% Where possible, we reference sections of the document, to help explain the code.  Section 5.3
%% is particularly helpful for understanding the memory layout in the device.
%%
%% The BME280 provides temperature, pressure, and humidity data in blocks of memory on the sensor,
%% which can be read through I2C commands.  The raw metrics stored on the sensor must then be
%% calibrated, using calibration data that is burned into the sensor, but which is also read at
%% initialization time into the state of this gen_server.
%%
%% All data stored in the sensor are stored in little endian format, some signed, some unsigned,
%% some 16 bit, some 8 bit.  The calibration data is not all contiguous, unfortunately, and
%% two of the values (for humidity) is not aligned on byte boundaries.  So extraction is ... interesting.
%% It would be easier if we supported bit syntax on unaligned boundaries in AtomVM.  Some day...
%%

% See section 4.2.2 for the layout of calibration data in the sensor.
read_calibration_data(I2CBus, Address) ->
    ?TRACE("Reading calibration data off ~p...", [I2CBus]),
    Bytes1 = read_bytes(I2CBus, 16#88, 25, Address),
    <<
        T1:16/little,        T2:16/signed-little, T3:16/signed-little,
        P1:16/little,        P2:16/signed-little, P3:16/signed-little,
        P4:16/signed-little, P5:16/signed-little, P6:16/signed-little,
        P7:16/signed-little, P8:16/signed-little, P9:16/signed-little,
        H1:8
    >> = Bytes1,
    Bytes2 = read_bytes(I2CBus, 16#E1, 7, Address),
    <<H2:16/signed-little, H3:8, E4:8/signed, E5:8, E6:8/signed, H6:8/signed>> = Bytes2,

    H4 = (E4 bsl 4) bor (E5 band 16#0F),
    H5 = (E6 bsl 4) bor ((E5 band 16#F0) bsr 4),

    Calibration = #calibration {
        dig_T1 = T1, dig_T2 = T2, dig_T3 = T3,
        dig_P1 = P1, dig_P2 = P2, dig_P3 = P3,
        dig_P4 = P4, dig_P5 = P5, dig_P6 = P6,
        dig_P7 = P7, dig_P8 = P8, dig_P9 = P9,
        dig_H1 = H1, dig_H2 = H2, dig_H3 = H3,
        dig_H4 = H4, dig_H5 = H5, dig_H6 = H6
    },
    ?TRACE("Calibration data: ~p", [Calibration]),
    Calibration.

%% @private
read_bytes(I2CBus, Register, Len, Address) ->
    {ok, Bytes} = i2c_bus:read_bytes(I2CBus, Address, Register, Len),
    ?TRACE("Read bytes ~p off I2CBus ~p, Register ~p, Len ~p..., Address ~p", [Bytes, I2CBus, Register, Len, Address]),
    Bytes.

%% @private
read_byte(I2CBus, Register, Address) ->
    Bytes = read_bytes(I2CBus, Register, 1, Address),
    ?TRACE("Read bytes ~p from register ~p in address ~p~n", [Bytes, Register, Address]),
    <<Value:8>> = Bytes,
    Value.

%% @private
write_byte(I2CBus, Register, Byte, Address) ->
    Value = <<Byte:8>>,
    ?TRACE("Writing byte ~p to register ~p in address ~p~n", [Byte, Register, Address]),
    i2c_bus:write_bytes(I2CBus, Address, Register, Value).

%% @private
do_take_reading(State) ->
    #state{
        i2c_bus = I2CBus,
        address = Address,
        temp_oversampling = TempOverSampling,
        pressure_oversampling = PressureOverSampling,
        humidity_oversampling = HumidityOverSampling,
        mode = Mode
    } = State,
    %%
    %% Tell the sensor to take a temp, pressure, and humidity reading
    %% with specified oversampling.  Per the spec, we need to write to
    %% the HUM and then MEAS registers.  The mode should almost always be force.
    %%
    ok = write_byte(I2CBus, ?BME280_REGISTER_CTL_HUM, HumidityOverSampling, Address),
    Meas = (TempOverSampling bsl 5) bor (PressureOverSampling bsl 2) bor Mode,
    ok = write_byte(I2CBus, ?BME280_REGISTER_CTL_MEAS, Meas, Address),
    %%
    %% Wait the max time for the sensor to take the reading.
    %% See Section 9.2 of the spec for expected timing measurements.
    %%
    SleepTimeUs = get_max_timing_us(
        TempOverSampling,
        PressureOverSampling,
        HumidityOverSampling
    ),
    timer:sleep(SleepTimeUs div 1000 + case SleepTimeUs rem 1000 of 0 -> 0; _ -> 1 end),
    %%
    %% Read the data in memory.  The BME280 reference documentation recommends
    %% reading all values in a single block.
    %%
    Bytes = read_bytes(I2CBus, 16#F7, 8, Address),
    <<
        Press_MSB:8, Press_LSB:8, Press_XLSB:8,
        Temp_MSB:8,  Temp_LSB:8,  Temp_XLSB:8,
        Hum_MSB:8,   Hum_LSB:8
    >> = Bytes,
    Cal = State#state.calibration_data,
    %%
    %% Calculate and Calibrate temperature, pressure, and humidity readings
    %%
    RawTemp = ((Temp_MSB bsl 16) bor (Temp_LSB bsl 8) bor Temp_XLSB) bsr 4,
    {T_fine, Temperature} = calibrate_temp(Cal, RawTemp),
    RawPressure = ((Press_MSB bsl 16) bor (Press_LSB bsl 8) bor Press_XLSB) bsr 4,
    Pressure = calibrate_pressure(Cal, T_fine, RawPressure),
    RawHumidity = (Hum_MSB bsl 8) bor Hum_LSB,
    Humidity = calibrate_humidity(Cal, T_fine, RawHumidity),
    %%
    %% Normalize into {integer, fractional} values.
    %%
    Reading = {
        normalize_reading(Temperature, TempOverSampling, 100),
        normalize_reading(Pressure, PressureOverSampling, 100),
        normalize_reading(Humidity, HumidityOverSampling, 1024)
    },
    ?TRACE("Reading: ~p", [Reading]),
    Reading.

%% @private
do_sleep(State) ->
    #state{
        i2c_bus = I2CBus,
        address = Address
    } = State,
    ?TRACE("Setting BME device to sleep ...", []),
    ok = write_byte(I2CBus, ?BME280_REGISTER_CTL_HUM, 16#FF, Address),
    ok = write_byte(I2CBus, ?BME280_REGISTER_CTL_MEAS, 16#FF, Address).

%% @private
normalize_reading(R, O, D) ->
    case O of
        0 -> undefined;
        _ ->
            R / D
    end.

%% See Section 9.1 for max measurement time
%% @private
get_max_timing_us(TempOversampling, PressureOversampling, HumidityOversampling) ->
    TempSleepTimeUs = 1250 + 2300 * (1 bsl TempOversampling),
    PressureSleepTimeUs = 2300 * (1 bsl PressureOversampling) + 575,
    HumiditySleepTimeUs = 2300 * (1 bsl HumidityOversampling) + 575,
    TempSleepTimeUs + PressureSleepTimeUs + HumiditySleepTimeUs.

%% Annotated with the recommended algorithm.  See Section 8.2 (32-bit version)
%% @private
calibrate_temp(Cal, Adc_T) ->
    Dig_T1 = Cal#calibration.dig_T1,
    Dig_T2 = Cal#calibration.dig_T2,
    Dig_T3 = Cal#calibration.dig_T3,
    %% var1 = ((((adc_T>>3) – ((BME280_S32_t)dig_T1<<1))) * ((BME280_S32_t)dig_T2)) >> 11;
    %% var1 = ((((adc_T >> 3) – (dig_T1 << 1))) * dig_T2) >> 11;
    Var1 = (((Adc_T bsr 3) - (Dig_T1 bsl 1)) * Dig_T2) bsr 11,
    % var2 = (((((adc_T>>4) – ((BME280_S32_t)dig_T1)) * ((adc_T>>4) – ((BME280_S32_t)dig_T1)))>> 12) * ((BME280_S32_t)dig_T3)) >> 14;
    % var2 = (((((adc_T >> 4) – dig_T1) * ((adc_T >> 4) – dig_T1))  >> 12) * dig_T3) >> 14;
    Var2 = (((((Adc_T bsr 4) - Dig_T1) * ((Adc_T bsr 4) - Dig_T1)) bsr 12) * Dig_T3) bsr 14,
    % t_fine = var1 + var2;
    T_fine = Var1 + Var2,
    % T =(t_fine * 5 +1 28) >> 8;
    Temperature = (T_fine * 5 + 128) bsr 8,
    {T_fine, Temperature}.

%% Annotated with the recommended algorithm.  See Section 8.2 (32-bit version)
%% @private
calibrate_pressure(Cal, T_fine, Adc_P) ->
    Dig_P1 = Cal#calibration.dig_P1,
    Dig_P2 = Cal#calibration.dig_P2,
    Dig_P3 = Cal#calibration.dig_P3,
    Dig_P4 = Cal#calibration.dig_P4,
    Dig_P5 = Cal#calibration.dig_P5,
    Dig_P6 = Cal#calibration.dig_P6,
    Dig_P7 = Cal#calibration.dig_P7,
    Dig_P8 = Cal#calibration.dig_P8,
    Dig_P9 = Cal#calibration.dig_P9,
    % limit 16#0FFFFFFF
    % var1 = (((BME280_S32_t)t_fine)>>1) – (BME280_S32_t)64000;
    % var1 = (t_fine >> 1) – 64000;
    Var1 = (T_fine bsr 1) - 64000,
    % var2 = (((var1>>2) * (var1>>2)) >> 11 ) * ((BME280_S32_t)dig_P6);
    % var2 = (((var1 >> 2) * (var1 >> 2)) >> 11 ) * dig_P6;
    Var2 = (((Var1 bsr 2) * (Var1 bsr 2)) bsr 11) * Dig_P6,
    % var2 = var2 + ((var1*((BME280_S32_t)dig_P5))<<1);
    % var2 = var2 + ((var1 * dig_P5) << 1);
    Var3 = Var2 + ((Var1 * (Dig_P5)) bsl 1),
    % var2 = (var2>>2)+(((BME280_S32_t)dig_P4)<<16);
    % var2 = (var2 >> 2) + (dig_P4 << 16);
    Var4 = (Var3 bsr 2) + ((Dig_P4 ) bsl 16),
    % Var4 = (Var3 bsr 2) + ((Dig_P4 ) * 65536),
    % var1 = (((dig_P3 * (((var1>>2) * (var1>>2)) >> 13 )) >> 3) + ((((BME280_S32_t)dig_P2) * var1)>>1))>>18;
    % var1 = (((dig_P3 * (((var1 >> 2) * (var1 >> 2)) >> 13 )) >> 3) + ((dig_P2 * var1) >> 1)) >> 18;
    Var5 = (((Dig_P3 * (((Var1 bsr 2) * (Var1 bsr 2)) bsr 13)) bsr 3) + ((Dig_P2 * Var1) bsr 1)) bsr 18,
    % var1 =((((32768+var1))*((BME280_S32_t)dig_P1))>>15);
    % var1 = ((32768 + var1) * dig_P1) >> 15;
    Var6 = ((32768 + Var5) * Dig_P1) bsr 15,
    case Var6 of
        0 ->
            0;
        _ ->
            % p = (((BME280_U32_t)(((BME280_S32_t)1048576)-adc_P)-(var2>>12)))*3125;
            % p = ((1048576 - adc_P) - (var2 >> 12)) * 3125;
            %P = ((1048576 - Adc_P) - (Var4 bsr 12)) * 3125,
            P = ((1048576 - Adc_P) - (Var4 div 4096)) * 3125,
            P1 = case P < 16#80000000 of
                true ->
                    % p = (p << 1) / ((BME280_U32_t)var1);
                    (P bsl 1) div Var6;
                _ ->
                    % p = (p / (BME280_U32_t)var1) * 2;
                    (P div Var6) bsl 1
            end,
            % var1 = (((BME280_S32_t)dig_P9) * ((BME280_S32_t)(((p>>3) * (p>>3))>>13)))>>12;
            % var1 = (dig_P9 * (((p >> 3) * (p >> 3)) >> 13)) >> 12;
            Var7 = (Dig_P9 * (((P1 bsr 3) * (P1 bsr 3)) bsr 13)) bsr 12,
            % var2 = (((BME280_S32_t)(p>>2)) * ((BME280_S32_t)dig_P8))>>13;
            % var2 = ((p >> 2) * dig_P8) >> 13;
            Var8 = ((P1 bsr 2) * Dig_P8) bsr 13,
            % p = (BME280_U32_t)((BME280_S32_t)p + ((var1 + var2 + dig_P7) >> 4));
            % p = p + ((var1 + var2 + dig_P7) >> 4));
            R = P1 + ((Var7 + Var8 + Dig_P7) bsr 4),
            R
    end.

%% Annotated with the recommended algorithm.  See Section 4.2.3
%% @private
calibrate_humidity(Cal, T_fine, Adc_H) ->
    Dig_H1 = Cal#calibration.dig_H1,
    Dig_H2 = Cal#calibration.dig_H2,
    Dig_H3 = Cal#calibration.dig_H3,
    Dig_H4 = Cal#calibration.dig_H4,
    Dig_H5 = Cal#calibration.dig_H5,
    Dig_H6 = Cal#calibration.dig_H6,
    % % v_x1_u32r = (t_fine – ((BME280_S32_t)76800));
    V_x1_u32r = T_fine - 76800,
    % %v_x1_u32r = (((((adc_H << 14) – (((BME280_S32_t)dig_H4) << 20) – (((BME280_S32_t)dig_H5) * v_x1_u32r)) + ((BME280_S32_t)16384)) >> 15) * (((((((v_x1_u32r * ((BME280_S32_t)dig_H6)) >> 10) * (((v_x1_u32r * ((BME280_S32_t)dig_H3)) >> 11) + ((BME280_S32_t)32768))) >> 10) + ((BME280_S32_t)2097152)) * ((BME280_S32_t)dig_H2) + 8192) >> 14));
    % %v_x1_u32r = (((((adc_H << 14) – (dig_H4 << 20) – (dig_H5 * v_x1_u32r)) + 16384) >> 15) * (((((((v_x1_u32r * dig_H6) >> 10) * (((v_x1_u32r * dig_H3) >> 11) + 32768)) >> 10) + 2097152) * dig_H2 + 8192) >> 14));
    V_x2_u32r =    (((((Adc_H bsl 14) - (Dig_H4 bsl 20) - (Dig_H5 * V_x1_u32r)) + 16384) bsr 15) * (((((((V_x1_u32r * Dig_H6) bsr 10) * (((V_x1_u32r * Dig_H3) bsr 11) + 32768)) bsr 10) + 2097152) * Dig_H2 + 8192) bsr 14)),
    % % v_x1_u32r = (v_x1_u32r – (((((v_x1_u32r >> 15) * (v_x1_u32r >> 15)) >> 7) * ((BME280_S32_t)dig_H1)) >> 4));
    % % v_x1_u32r = (v_x1_u32r – (((((v_x1_u32r >> 15) * (v_x1_u32r >> 15)) >> 7) * dig_H1) >> 4));
    V_x3_u32r =     (V_x2_u32r - (((((V_x2_u32r bsr 15) * (V_x2_u32r bsr 15)) bsr 7) * Dig_H1) bsr 4)),
    % %v_x1_u32r = (v_x1_u32r < 0 ? 0 : v_x1_u32r);
    V_x4_u32r = case V_x3_u32r < 0 of true -> 0; _ -> V_x3_u32r end,
    % %v_x1_u32r = (v_x1_u32r > 419430400 ? 419430400 : v_x1_u32r); } return (BME280_U32_t)(v_x1_u32r>>12);
    % %v_x1_u32r = (v_x1_u32r > 419430400 ? 419430400 : v_x1_u32r); } return (v_x1_u32r >> 12);
    V_x5_u32r = case V_x4_u32r > 419430400 of true -> 419430400; _ -> V_x4_u32r end,
    % return (BME280_U32_t)(v_x1_u32r>>12);
    V_x5_u32r bsr 12.
