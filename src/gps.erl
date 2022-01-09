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
%%-----------------------------------------------------------------------------
%% @doc GPS device integration for AtomVM.
%%
%% This module can be used to integrate GPS devices into your AtomVM applications.
%%
%% The current implementation assumes a UART connection between the ESP32 and
%% GPS sensor, and that the GPS sensor supports the NMEA protocol.
%%
%% @end
%%-----------------------------------------------------------------------------
-module(gps).

-export([
    start/1, stop/1, latest_reading/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behavior(gen_server).

% -define(TRACE(A, B), io:format(A, B)).
-define(TRACE(A, B), ok).
-define(DEFAULT_CONFIG, #{
    uart_port => uart_1,
    baud_rate => 9600,
    data_bits => data_bits_8,
    parity => disable,
    stop_bits => stop_bits_1,
    event_queue_size => 16
}).

-record(state, {
    port,
    config,
    latest_reading
}).


-type gps() :: pid().
-type uart_port() :: uart_0 | uart_1 | uart_2.
-type baud_rate() :: non_neg_integer().
-type data_bits() :: data_bits_5 | data_bits_6 | data_bits_7 | data_bits_8.
-type stop_bits() :: stop_bits_1 | stop_bits_1_5 | stop_bits_2.
-type parity() :: disable | odd | even.
-type config() :: #{
    uart_port => uart_port(),
    rx_pin => non_neg_integer(),
    baud_rate => baud_rate(),
    data_bits => data_bits(),
    stop_bits => stop_bits(),
    parity => parity(),
    event_queue_size => non_neg_integer(),
    gps_reading_filter => [
        datetime | latitude | longitude | altitude | speed
        | sats_in_use | fix | fix_mode | valid | sats_in_view
    ],
    gps_reading_handler => fun((gps_reading()) -> any())
}.

-type year() :: non_neg_integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type date() :: {year(), month(), day()}.

-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type time() :: {hour(), minute(), second()}.

-type fp() :: {integer(), non_neg_integer()}.
-type geo_coordinate() :: fp().

-type fix() :: invalid | gps | dgps.
-type fix_mode() :: invalid | mode_2d | mode_3d.
-type satellite() :: #{
    num => non_neg_integer(),
    elevation => non_neg_integer(),
    azimuth => non_neg_integer(),
    snr => non_neg_integer()
}.

-type gps_reading() :: #{
    datetime => {date(), time()},
    latitude => geo_coordinate(),
    longitude => geo_coordinate(),
    altitude => fp(),
    speed => fp(),
    sats_in_use => [non_neg_integer()],
    fix => fix(),
    fix_mode => fix_mode(),
    valid => boolean(),
    sats_in_view => [satellite()]
}.

%%-----------------------------------------------------------------------------
%% @param   Config      configuration
%% @returns ok | {error, Reason}
%% @doc     Start a GPS instance.
%%
%% This function will start an instance of an GPS process.  This process will
%% deliver GPS readings to a function, if supplied in the `Config' structure.
%%
%% Use the returned reference in subsequent GPS operations.
%%
%% This function takes a configuration map as a parameter.  This map configures
%% the client for connectivity to a GPS device, in addition to configuration
%% parameters that govern the behavior of the client.
%%
%% The following options are supported (or required):
%%
%% <table style="width:100%">
%%   <tr>
%%      <th>Key</th> <th>Type</th> <th>Default Value</th> <th>Required</th> <th>Description</th>
%%   </tr>
%%   <tr>
%%      <td>`uart_port'</td>
%%      <td>`uart_0 | uart_1 | uart_2'</td>
%%      <td>`uart_1'</td>
%%      <td>yes</td>
%%      <td>UART hardware port to use</td>
%%   </tr>
%%   <tr>
%%      <td>`rx_pin'</td>
%%      <td>`non_neg_integer()'</td>
%%      <td></td>
%%      <td>no</td>
%%      <td>The ESP32 pin connected to the data port on the GPS device.  If unspecified, the
%%          default rx pin for the hardware device will be used:
%%          <ul>
%%              <li>`uart_0': GPIO3</li>
%%              <li>`uart_1': GPIO9</li>
%%              <li>`uart_2': GPIO16</li>
%%          </ul>
%%      </td>
%%   </tr>
%%   <tr>
%%      <td>`baud_rate'</td>
%%      <td>`non_neg_integer()'</td>
%%      <td>9600</td>
%%      <td>no</td>
%%      <td>UART BAUD rate</td>
%%   </tr>
%%   <tr>
%%      <td>`data_bits'</td>
%%      <td>`data_bits_5 | data_bits_6 | data_bits_7 | data_bits_8'</td>
%%      <td>`data_bits_8'</td>
%%      <td>no</td>
%%      <td>UART data bits</td>
%%   </tr>
%%   <tr>
%%      <td>`stop_bits'</td>
%%      <td>`stop_bits_1 | stop_bits_1_5 | stop_bits_2'</td>
%%      <td>`stop_bits_1'</td>
%%      <td>no</td>
%%      <td>UART stop bits</td>
%%   </tr>
%%   <tr>
%%      <td>`parity'</td>
%%      <td>`disable | odd | even'</td>
%%      <td>`disable'</td>
%%      <td>no</td>
%%      <td>UART parity</td>
%%   </tr>
%%   <tr>
%%      <td>`event_queue_size'</td>
%%      <td>`non_neg_integer()'</td>
%%      <td>16</td>
%%      <td>no</td>
%%      <td>UART event queue size</td>
%%   </tr>
%%   <tr>
%%      <td>`event_queue_size'</td>
%%      <td>`non_neg_integer()'</td>
%%      <td>16</td>
%%      <td>no</td>
%%      <td>UART event queue size</td>
%%   </tr>
%%   <tr>
%%      <td>`gps_reading_filter'</td>
%%      <td>`[atom()]'</td>
%%      <td></td>
%%      <td>no</td>
%%      <td>A list of keys from the GPS reading map to include when sending GPS readings
%%          to the application.  By not setting this key in configuration, all available
%%          keys will be sent in a GPS reading.
%%      </td>
%%   </tr>
%% </table>
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config::config()) -> {ok, gps()} | {error, Reason::term()}.
start(Config) ->
    gen_server:start(?MODULE, validate_config(maps:merge(?DEFAULT_CONFIG, Config)), []).

%%-----------------------------------------------------------------------------
%% @param   GPS    the GPS instance created via `start/1'
%% @returns ok
%% @doc     Stop the specified GPS.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(GPS::gps()) -> ok.
stop(GPS) ->
    gen_server:stop(GPS).

%%-----------------------------------------------------------------------------
%% @param   GPS    the GPS instance created via `start/1'
%% @returns the latest GPS reading, or `undefined', if no reading has been taken.
%% @doc     Return the latest GPS reading.
%% @end
%%-----------------------------------------------------------------------------
-spec latest_reading(GPS::gps()) -> gps_reading() | undefined | {error, Reason::term()}.
latest_reading(GPS) ->
    gen_server:call(GPS, latest_reading).

%% ====================================================================
%%
%% gen_server API
%%
%% ============================================================================

%% @hidden
init(Config) ->
    try
        Self = self(),
        Port = erlang:open_port({spawn, "atomvm_gps"}, [{receiver, Self}, {config, Config}]),
        {ok, #state{
            port=Port,
            config=Config
        }}
    catch
        _:Error ->
            {stop, Error}
    end.

%% @hidden
handle_call(stop, _From, State) ->
    do_stop(State#state.port),
    {stop, normal, ok, State};

handle_call(latest_reading, _From, State) ->
    {reply, State#state.latest_reading, State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({gps_reading, GPSReading}, State) ->
    ?TRACE("handle_info: Received {gps_reading, ~p}~n", [GPSReading]),
    Config = State#state.config,
    Self = self(),
    case maps:get(gps_reading_handler, Config, undefined) of
        undefined ->
            ok;
        Fun when is_function(Fun) ->
            NewReading = maybe_filter_gps_reading(
                GPSReading,
                maps:get(gps_reading_filter, State#state.config, undefined)
            ),
            spawn(fun() -> Fun(Self, NewReading) end);
        Pid when is_pid(Pid) ->
            NewReading = maybe_filter_gps_reading(
                GPSReading,
                maps:get(gps_reading_filter, State#state.config, undefined)
            ),
            Pid ! {gps_reading, NewReading}
    end,
    erlang:garbage_collect(),
    {noreply, State#state{latest_reading=GPSReading}};
handle_info(Info, State) ->
    io:format("Unexpected INFO message: ~p~n", [Info]),
    {noreply, State}.

%% @hidden
terminate(Reason, State) ->
    do_stop(State#state.port),
    io:format("gps gen_server process ~p terminated with reason ~p.  State: ~p~n", [self(), Reason, State]),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal operations
%%

%% @private
maybe_filter_gps_reading(GPSReading, undefined) ->
    GPSReading;
maybe_filter_gps_reading(GPSReading, Keys) ->
    maps:fold(
        fun(Key, Value, Accum) ->
            case lists:member(Key, Keys) of
                true ->
                    Accum#{Key => Value};
                _ ->
                    Accum
            end
        end,
        maps:new(),
        GPSReading
    ).


%% @private
do_stop(Port) ->
    call(Port, tini),
    Port ! stop.

%% @private
call(Port, Msg) ->
    Ref = make_ref(),
    Port ! {self(), Ref, Msg},
    receive
        {Ref, Ret} ->
            Ret
    end.


%% @private
validate_config(Config) ->
    validate_uart_port(maps:get(uart_port, Config)),
    validate_integer_or_undefined(maps:get(rx_pin, Config, undefined)),
    validate_integer(maps:get(baud_rate, Config)),
    validate_data_bits(maps:get(data_bits, Config)),
    validate_stop_bits(maps:get(stop_bits, Config)),
    validate_parity(maps:get(parity, Config)),
    validate_integer(maps:get(event_queue_size, Config)),
    validate_atom_list_or_undefined(maps:get(gps_reading_filter, Config, undefined)),
    validate_fun(maps:get(gps_reading_handler, Config)),
    Config.

%% @private
validate_uart_port(uart_0) ->   ok;
validate_uart_port(uart_1) ->   ok;
validate_uart_port(uart_2) ->   ok;
validate_uart_port(_) ->        throw(bardarg).

%% @private
validate_integer_or_undefined(undefined) -> ok;
validate_integer_or_undefined(I) ->
    validate_integer(I).

%% @private
validate_integer(I) when is_integer(I) ->   ok;
validate_integer(_) ->        throw(bardarg).

%% @private
validate_data_bits(data_bits_5) ->   ok;
validate_data_bits(data_bits_6) ->   ok;
validate_data_bits(data_bits_7) ->   ok;
validate_data_bits(data_bits_8) ->   ok;
validate_data_bits(_) ->        throw(bardarg).

%% @private
validate_stop_bits(stop_bits_1) ->      ok;
validate_stop_bits(stop_bits_1_5) ->    ok;
validate_stop_bits(stop_bits_2) ->      ok;
validate_stop_bits(_) ->                throw(bardarg).

%% @private
validate_parity(disable) -> ok;
validate_parity(odd) ->     ok;
validate_parity(even) ->    ok;
validate_parity(_) ->       throw(bardarg).

%% @private
validate_atom_list_or_undefined(undefined) -> ok;
validate_atom_list_or_undefined(List) when is_list(List) ->
    lists:foreach(fun validate_atom/1, List);
validate_atom_list_or_undefined(_) ->       throw(bardarg).

%% @private
validate_atom(Atom) when is_atom(Atom) -> ok;
validate_atom(_) ->       throw(bardarg).

%% @private
validate_fun(Fun) when is_function(Fun) -> ok;
validate_fun(_) ->       throw(bardarg).
