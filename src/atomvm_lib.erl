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

-export([set_rtc_memory/1, get_rtc_memory/0]).

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
