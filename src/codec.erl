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
-module(codec).

-export([encode/2]).

encode(0, hex) ->
    "00";
encode(Value, hex) when 0 < Value ->
    hex_encode(Value, []).


%% @private
hex_encode(0, Accum) ->
    Accum;
hex_encode(Value, Accum) ->
    Octet = Value band 16#FF,
    hex_encode(Value bsr 8, hex_encode_octet(Octet) ++ Accum).

%% @private
hex_encode_octet(Octet) ->
    hex_encode((Octet band 16#F0) bsr 4) ++ hex_encode(Octet band 16#0F).

%% @private
hex_encode(N) when 0 =< N andalso N =< 9 -> integer_to_list(N);
hex_encode(16#0A) -> "A";
hex_encode(16#0B) -> "B";
hex_encode(16#0C) -> "C";
hex_encode(16#0D) -> "D";
hex_encode(16#0E) -> "E";
hex_encode(16#0F) -> "F".
