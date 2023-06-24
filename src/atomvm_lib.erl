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
-module(atomvm_lib).

-export([set_rtc_memory/1, get_rtc_memory/0, random/2, sleep_forever/0, to_hex/2, to_hex/1, set_date_time/1, set_date_time/2, set_time_of_day/1]).

-type year() :: integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type millisecond() :: 0..999.

-type date() :: {year(), month(), day()}.
-type time() :: {hour(), minute(), second(), millisecond()}.
-type date_time() :: {date(), time()}.

%%-----------------------------------------------------------------------------
%% @param   Data binary data to store
%% @returns ok
%% @doc     Store a blob in RTC memory.
%%
%% This operation will store data in RTC memory.  This memory will
%% be preserved during a sleep operation, but will be cleared once
%% the device restarts.
%%
%% The input binary data must be no larger than the the value set
%% in configuration at build time of the AtomVM binary.  (By default,
%% the maximum binary size is 0 bytes.  You may adjust this value
%% via `make menuconfig' when building the AtomVM image.)  An attempt
%% to store a blob larger than the maximum allowable size will result
%% in a `badarg' exception.
%% @end
%%-----------------------------------------------------------------------------
-spec set_rtc_memory(Data::binary()) -> ok.
set_rtc_memory(_Data) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns data stored in RTC memory, or the empty binary (`<<"">>'), if
%%          nothing has been stored.
%% @doc     Retrieve a blob stored in RTC memory.
%%
%% This operation will retrieve data stored in RTC memory.  This memory will
%% be preserved during a sleep operation, but will be cleared once
%% the device restarts.
%% @end
%%-----------------------------------------------------------------------------
-spec get_rtc_memory() -> binary().
get_rtc_memory() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns random 32-bit integer between `Lower' and `Upper'.
%% @doc     Returns a random 32-bit integer value between `Lower' and `Upper'.
%%
%%          Bother `Lower' and `Upper' must be integers and `Lower' must be less than `Upper'.
%% @end
%%-----------------------------------------------------------------------------
-spec random(Lower::integer(), Upper::integer()) -> integer().
random(Lower, Upper) when is_integer(Lower), is_integer(Upper), Lower < Upper ->
    R = atomvm:random(),
    P = case R < 0 of true -> -R; _ -> R end,
    Lower + (P rem (Upper - Lower));
random(_,_) ->
    throw(badarg).


%%-----------------------------------------------------------------------------
%% @doc     Sleep forever.   This function does not halt.
%% @end
%%-----------------------------------------------------------------------------
sleep_forever() ->
    timer:sleep(24*60*60*1000),
    sleep_forever().


%%-----------------------------------------------------------------------------
%% @returns hex representation of I, as a string.
%% @doc     Returns the hex representation of I, as a string.
%% @end
%%-----------------------------------------------------------------------------
-spec to_hex(I::integer()|binary()) -> string().
to_hex(I) when is_integer(I) ->
    to_hex(I, 1);
to_hex(B) when is_binary(B) ->
    lists:flatten([ "0x" ++ to_hex(I) ++ "," || I <- erlang:binary_to_list(B)]).


%%-----------------------------------------------------------------------------
%% @returns hex representation of I, as a string.
%% @doc     Returns the hex representation of I, as a string.
%% @end
%%-----------------------------------------------------------------------------
-spec to_hex(I::integer, Bytes::non_neg_integer()) -> string().
to_hex(I, Bytes) when is_integer(I) ->
    to_hex(I, Bytes * 2, []).

%% @private
to_hex(0, K, Accum) ->
    maybe_pad(K, Accum);
to_hex(I, K, Accum) ->
    Quartet = I band 16#F,
    to_hex(I bsr 4, K - 1, [hex_char(Quartet) | Accum]).

%% @private
maybe_pad(0, Accum) ->
    Accum;
maybe_pad(K, Accum) ->
    maybe_pad(K - 1, [$0 | Accum]).

%% @private
hex_char(16#0) -> $0;
hex_char(16#1) -> $1;
hex_char(16#2) -> $2;
hex_char(16#3) -> $3;
hex_char(16#4) -> $4;
hex_char(16#5) -> $5;
hex_char(16#6) -> $6;
hex_char(16#7) -> $7;
hex_char(16#8) -> $8;
hex_char(16#9) -> $9;
hex_char(16#A) -> $A;
hex_char(16#B) -> $B;
hex_char(16#C) -> $C;
hex_char(16#D) -> $D;
hex_char(16#E) -> $E;
hex_char(16#F) -> $F.

%%
%% @param   DateTime    Date and Time to set
%% @return  `ok | {error, Reason :: term()}'
%% @doc Set the system time.
%%
%% Equivalent to `set_date_time(DateTime, 0)'
%% @end
%%
-spec set_date_time(DateTime :: date_time()) -> ok.
set_date_time(DateTime) ->
    set_date_time(DateTime, 0).

%%
%% @param   DateTime    Date and Time to set
%% @param   Millisecond Millisecond granularity not encapsulated in DateTime.
%% @return  `ok | {error, Reason :: term()}'
%% @doc Set the system time.
%%
%% This function sets the system time to the specified date and time (at
%% millisecond granularity).  The specified date may not be before the
%% UNIX epoch (Jan 1, 1970).  Coordinates are all in UTC.
%%
%% Note.  Some systems may require special permissions to call this function.
%% @end
%%
-spec set_date_time(DateTime :: date_time(), Millisecond :: 0..999) -> ok.
set_date_time({{Year, Month, Day}, {Hour, Minute, Second}} = DateTime, Millisecond)
    when is_integer(Year) andalso Year >= 1970
        andalso is_integer(Month) andalso Month >= 1 andalso Month =< 12
        andalso is_integer(Day) andalso Day >= 1 andalso Day =< 31
        andalso is_integer(Hour) andalso Hour >= 0 andalso Hour =< 24
        andalso is_integer(Minute) andalso Minute >= 0 andalso Minute =< 59
        andalso is_integer(Second) andalso Second >= 0 andalso Second =< 59
        andalso is_integer(Millisecond) andalso Millisecond >= 0 andalso Millisecond =< 999
    ->
    ?MODULE:set_time_of_day(seconds_since_epoch(DateTime) * 1000 + Millisecond).

%%
%% @param   MsSinceUnixEpoch Milliseconds since the UNIX epoch
%% @param   Millisecond Millisecond granularity not encapsulated in DateTime.
%% @return  `ok | {error, Reason :: term()}'
%% @doc Set the system time.
%%
%% This function sets the system time to the specified number of milliseconds
%% after the UNIX epoch (Jan 1, 1970).  Coordinates are all in UTC.
%%
%% Note.  Some systems may require special permissions to call this function.
%% @end
%%
-spec set_time_of_day(MsSinceUnixEpoch :: non_neg_integer()) -> ok.
set_time_of_day(_MsSinceUnixEpoch) ->
    throw(nif_error).

%% @private
seconds_since_epoch({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    YDay = day_of_year(Month, Day) - 1,
    AdjYear = Year - 1900,
    %% https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_15
    Second + Minute * 60 + Hour * 3600 + YDay * 86400 +
        (AdjYear - 70) * 31536000 + ((AdjYear - 69) div 4) * 86400 -
        ((AdjYear - 1) div 100) * 86400 + ((AdjYear + 299) div 400) * 86400.

%% @private
day_of_year(1, Day) when Day =< 31 ->
    Day;
day_of_year(2, Day) when Day =< 28 ->
    31 + Day;
day_of_year(3, Day) when Day =< 31 ->
    31 + 28 + Day;
day_of_year(4, Day) when Day =< 30 ->
    31 + 28 + 31 + Day;
day_of_year(5, Day) when Day =< 31 ->
    31 + 28 + 31 + 30 + Day;
day_of_year(6, Day) when Day =< 30 ->
    31 + 28 + 31 + 30 + 31 + Day;
day_of_year(7, Day) when Day =< 31 ->
    31 + 28 + 31 + 30 + 31 + 30 + Day;
day_of_year(8, Day) when Day =< 31 ->
    31 + 28 + 31 + 30 + 31 + 30 + 31 + Day;
day_of_year(9, Day) when Day =< 30 ->
    31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + Day;
day_of_year(10, Day) when Day =< 31 ->
    31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + Day;
day_of_year(11, Day) when Day =< 30 ->
    31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + Day;
day_of_year(12, Day) when Day =< 31 ->
    31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + Day;
day_of_year(_Month, _Day) ->
    error(badarg).
