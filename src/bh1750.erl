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
%% @doc An AtomVM I2C driver for the BH1750.
%%
%% The BH1750 is a small sensor that can read
%%
%% Further information about the Bosch Sensortec BH1750 can be found in the reference
%% documentation:
%% https://www.bosch-sensortec.com/media/boschsensortec/downloads/datasheets/bst-bme280-ds002.pdf
%%
%% @end
%%-----------------------------------------------------------------------------
-module(bh1750).

-behaviour(gen_server).

-export([start/2, start/3, stop/1, take_reading/1, reset/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type pin() :: non_neg_integer().
-type freq_hz() :: non_neg_integer().
-type resolution() :: low | high | high2.
-type mode() :: one_time | continuous.
-type option() :: {freq_hz, freq_hz()} |
            {resolution, resolution()} |
            {mode, mode()} |
            {owner, pid()}.
-type options() :: [option()].
-type bh() :: pid().

-type fractional() :: {integer(), integer()}.
-type reading() :: {integer(), fractional()}.

-define(BH1750_BASE_ADDR_L, 16#23).
-define(BH1750_BASE_ADDR_H, 16#5C).

-define(BH1750_POWER_DOWN, 16#00).
-define(BH1750_POWER_ON, 16#01).
-define(BH1750_RESET, 16#07).
-define(BH1750_CONTINUOUS_H_RES, 16#10).
-define(BH1750_CONTINUOUS_H_RES2, 16#11).
-define(BH1750_CONTINUOUS_L_RES, 16#13).
-define(BH1750_ONE_TIME_H_RES, 16#20).
-define(BH1750_ONE_TIME_H_RES2, 16#21).
-define(BH1750_ONE_TIME_L_RES, 16#23).

-define(DEFAULT_MODE, one_time).
-define(DEFAULT_RESOLUTION, high).
-define(DEFAULT_UPDATE_INTERVAL_MS, 1000).
-define(DEFAULT_MTREG, 69).

-record(state, {
    port,
    addr,
    mode,
    owner,
    resolution,
    mtreg,
    update_interval_ms,
    timer_ref
}).

%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @returns {ok, BH} on success, or {error, Reason}, on failure
%% @equiv   start(SDAPin, SCLPin, [])
%% @doc     Start the BH1750 driver.
%% @end
%%-----------------------------------------------------------------------------
-spec start(SDAPin::pin(), SCLPin::pin()) -> {ok, BH::bh()} | {error, Reason::term()}.
start(SDAPin, SCLPin) ->
    start(SDAPin, SCLPin, []).

%%-----------------------------------------------------------------------------
%% @param   SDAPin pin number for I2C SDA channel
%% @param   SCLPin pin number for the I2C SCL channel
%% @param   Options additional driver options
%% @returns {ok, BH} on success, or {error, Reason}, on failure
%% @doc     Start the BH1750 driver.
%%
%% This operation will start the BH driver.  Use the returned reference
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
-spec start(SDAPin::pin(), SCLPin::pin(), Options::options()) -> {ok, BH::bh()} | {error, Reason::term()}.
start(SDAPin, SCLPin, Options) ->
    case gen_server:start(?MODULE, [SDAPin, SCLPin, maybe_add_self(Options)], []) of
        {ok, Pid} = R ->
            maybe_start_continuous(Pid, Options),
            R;
        E -> E
    end.

%% @private
maybe_add_self(Options) ->
    case proplists:get_value(mode, Options) of
        continuous ->
            case proplists:get_value(owner, Options) of
                undefined ->
                    io:format("Adding ~p to options ~p~n", [self(), Options]),
                    [{owner, self()} | Options];
                _ -> Options
            end;
        _ ->
            Options
    end.

%% @private
%% WORKAROUND until gen_server's init callback is called in spawned proc
maybe_start_continuous(Pid, Options) ->
    case proplists:get_value(mode, Options) of
        continuous ->
            gen_server:cast(Pid, start_continuous);
        _ -> ok
    end.

%%-----------------------------------------------------------------------------
%% @param       BH a reference to the BH instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc Stop the BH1750 driver.
%%
%% Note. This function is not well tested and its use may result in a memory leak.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(BH::bh()) -> ok | {error, Reason::term()}.
stop(BH) ->
    gen_server:stop(BH).

%%-----------------------------------------------------------------------------
%% @param       BH a reference to the BH instance created via start
%% @returns     {ok, Reading} if successful; {error, Reason}, otherwise
%% @doc Take a reading from the sensor.
%%
%% This function will take a reading from the attached BH1750 sensor.
%%
%% The return value is a 3-ary tuple containing the temperature, pressure,
%% and humidty readings from the sensor.  Each element of the tuple is a
%% pair, containing the value in integral and fractional parts.
%%
%% Temperature is expressed in degrees celsius, pressure is expressed in hectopascals,
%% and humidity is expressed as relative humidity.
%% @end
%%-----------------------------------------------------------------------------
-spec take_reading(BH::bh()) -> {ok, Reading::reading()} | {error, Reason::term()}.
take_reading(BH) ->
    gen_server:call(BH, take_reading).


%%-----------------------------------------------------------------------------
%% @param       BH a reference to the BH instance created via start
%% @returns     ok
%% @doc         Perform a soft reset of the BH1750 sensor.
%%
%%              A soft reset will set all of the registers in the device
%%              to values in section 5.3 of the reference documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec reset(BH::bh()) -> ok.
reset(BH) ->
    gen_server:call(BH, reset).

%%
%% gen_server API
%%

%% @hidden
init([SDAPin, SCLPin, Options]) ->
    FreqHz = proplists:get_value(freq_hz, Options, 400000),
    Port = open_port({spawn, "i2c"}, [{scl_io_num, SCLPin}, {sda_io_num, SDAPin}, {i2c_clock_hz, FreqHz}]),
    Mode = normalize_mode(proplists:get_value(mode, Options, ?DEFAULT_MODE)),
    UpdateIntervalMs = proplists:get_value(update_interval_ms, Options, ?DEFAULT_UPDATE_INTERVAL_MS),
    State = #state{
        port = Port,
        addr = normalize_addr(proplists:get_value(addr, Options, addr_l)),
        mode = Mode,
        update_interval_ms = UpdateIntervalMs,
        owner = proplists:get_value(owner, Options),
        resolution = normalize_resolution(proplists:get_value(resolution, Options, ?DEFAULT_RESOLUTION)),
        mtreg = normalize_mtreg(proplists:get_value(mtreg, Options, ?DEFAULT_MTREG))
    },
    do_set_sensitivity(Port, State#state.addr, State#state.mtreg),
    {ok, State}.

%% private
normalize_addr(Addr) ->
    case Addr of
        addr_l -> ?BH1750_BASE_ADDR_L;
        addr_h -> ?BH1750_BASE_ADDR_H;
        _ ->  throw(badarg)
    end.

%% private
normalize_mode(Res) ->
    case Res of
        one_time -> one_time;
        continuous -> continuous;
        _ ->  throw(badarg)
    end.

%% private
normalize_resolution(Res) ->
    case Res of
        high -> high;
        high2 -> high2;
        low -> low;
        _ ->  throw(badarg)
    end.

%% private
normalize_mtreg(MtReg) ->
    case 30 < MtReg andalso MtReg < 255 of
        true -> MtReg;
        _ ->  throw(badarg)
    end.

%% @hidden
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(take_reading, _From, State) ->
    Reply = case State#state.mode of
        one_time ->
            {ok, do_take_reading(State)};
        _ ->
            {error, badstate}
    end,
    {reply, Reply, State};
handle_call(reset, _From, State) ->
    {reply, todo, State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_cast(start_continuous, State) ->
    do_start_continuous_reading(State),
    {noreply, start_tick(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
start_tick(State) ->
    Me = self(),
    TimerRef = spawn(fun() -> start_timer(State#state.update_interval_ms, Me, continuous_reading) end),
    State#state{timer_ref = TimerRef}.

%% @private
start_timer(Ms, Pid, Msg) ->
    timer:sleep(Ms),
    Pid ! Msg.

%% @hidden
handle_info(continuous_reading, State) ->
    {noreply, continuous_reading(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
continuous_reading(State) ->
    Reading = do_continuous_reading(State),
    State#state.owner ! Reading,
    start_tick(State).

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
        port = Port,
        addr = Address,
        resolution = Resolution,
        mode = Mode,
        mtreg = MtReg
    } = State,
    ok = send_command(Port, Address, get_command(one_time, Resolution)),
    timer:sleep(get_sleep_ms(Resolution, MtReg)),
    Bin = i2c:read_bytes(Port, Address, 2),
    to_reading(Bin, Mode, MtReg).

%% @private
to_reading(Bin, Mode, MtReg) ->
    <<Reading:16/big-unsigned>> = Bin,
    X = multiply({1, {2, 10}}, {69, MtReg}),
    Y = case Mode of
        high2 ->
            multiply(X, 2);
        _ -> X
    end,
    divide(Reading, Y).

%% @private
multiply(A, B) ->
    rational:simplify(rational:reduce(rational:multiply(A, B))).

%% @private
divide(A, B) ->
    rational:to_decimal(rational:reduce(rational:divide(A, B)), 2).

%% @private
do_start_continuous_reading(State) ->
    #state{
        port = Port,
        addr = Address,
        resolution = Resolution,
        mtreg = MtReg
    } = State,
    ok = send_command(Port, Address, get_command(continuous, Resolution)),
    timer:sleep(get_sleep_ms(Resolution, MtReg)).

do_continuous_reading(State) ->
    #state{
        port = Port,
        addr = Address,
        mode = Mode,
        mtreg = MtReg
    } = State,
    Bin = i2c:read_bytes(Port, Address, 2),
    to_reading(Bin, Mode, MtReg).

do_set_sensitivity(Port, Address, MtReg) ->
    send_command(Port, Address, ?BH1750_POWER_ON),
    High = (MtReg band 16#F8) bor 16#04,
    send_command(Port, Address, High),
    Low = 16#60 bor (MtReg band 16#1F),
    send_command(Port, Address, Low),
    send_command(Port, Address, ?BH1750_POWER_DOWN).

%% @private
send_command(Port, Address, Command) ->
    ok = i2c:begin_transmission(Port, Address),
    ok = i2c:write_byte(Port, Command),
    ok = i2c:end_transmission(Port).

%% @private
get_command(one_time, high) ->
    ?BH1750_ONE_TIME_H_RES;
get_command(one_time, high2) ->
    ?BH1750_ONE_TIME_H_RES2;
get_command(one_time, low) ->
    ?BH1750_ONE_TIME_L_RES;
get_command(continuous, high) ->
    ?BH1750_CONTINUOUS_H_RES;
get_command(continuous, high2) ->
    ?BH1750_CONTINUOUS_H_RES2;
get_command(continuous, low) ->
    ?BH1750_CONTINUOUS_L_RES.

%% @private
get_sleep_ms(Resolution, MtReg) ->
    Base = get_sleep_ms(Resolution),
    Sleep = multiply(Base, divide(MtReg, 69)),
    round_sleep(Sleep).

%% @private
get_sleep_ms(high) -> 120;
get_sleep_ms(high2) -> 120;
get_sleep_ms(low) -> 16.

%% @private
round_sleep({I, {_N, _D} = F}) ->
    I + round_sleep(F);
round_sleep({0, _D}) ->
    0;
round_sleep({N, D}) ->
    case N > (D bsr 1) of
        true -> 1;
        _ -> 0
    end;
round_sleep(I) when is_integer(I) -> I.
