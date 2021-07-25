%%
%% Copyright (c) dushin.net
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http:%%www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(qrcodegen).

-export([encode/1, encode/2, encode/6, get_side/1, get_pixel/3, to_bitmap/1]).
%% nifs
-export([nif_encode_text/6, nif_encode_bin/6, nif_get_side/1, nif_get_pixel/3, nif_to_bitmap/1]).

-type ecc() :: low | medium | quartile | high.
-type version() :: 1..40.
-type mask() :: auto | mask_0 | mask_1 | mask_2 | mask_3 | mask_4 | maks_5 | mask_6 | mask_7.

-type qrcode() :: binary().
-type side() :: 21..177.

-spec encode(Data::string() | binary()) -> QRCode::qrcode() | error.
encode(String) ->
    encode(String, low, 1, 40, auto, true).

-spec encode(Data::string() | binary(), Version::version()) -> QRCode::qrcode() | error.
encode(String, Version) ->
    encode(String, low, Version, Version, auto, true).

-spec encode(Data::string() | binary(), Ecc::ecc(), MinVersion::version(), MaxVersion::version(), Mask::mask(), BoostECL::boolean()) -> QRCode::qrcode() | error.
encode(String, Ecc, MinVersion, MaxVersion, Mask, BoostECL) when is_list(String) ->
    ?MODULE:nif_encode_text(list_to_binary(String), Ecc, MinVersion, MaxVersion, Mask, BoostECL);
encode(Bin, Ecc, MinVersion, MaxVersion, Mask, BoostECL) when is_binary(Bin) ->
    ?MODULE:nif_encode_bin(Bin, Ecc, MinVersion, MaxVersion, Mask, BoostECL).

-spec get_side(QRCode::qrcode()) -> side().
get_side(QRCode) when is_binary(QRCode) ->
    ?MODULE:nif_get_side(QRCode).

-spec get_pixel(QRCode::qrcode(), X::non_neg_integer(), Y::non_neg_integer()) -> boolean().
get_pixel(QRCode, X, Y) when is_binary(QRCode) ->
    ?MODULE:nif_get_pixel(QRCode, X, Y).

-spec to_bitmap(QRCode::qrcode()) -> binary().
to_bitmap(QRCode) when is_binary(QRCode) ->
    ?MODULE:nif_to_bitmap(QRCode).

%%%
%%% Nifs
%%%

%% @hidden
nif_encode_text(_Bin, _Ecc, _MinVersion, _MaxVersion, _Mask, _BoostECL) ->
    throw(nif_error).

%% @hidden
nif_encode_bin(_Bin, _Ecc, _MinVersion, _MaxVersion, _Mask, _BoostECL) ->
    throw(nif_error).

%% @hidden
nif_get_side(_QRCode) ->
    throw(nif_error).

%% @hidden
nif_get_pixel(_QRCode, _X, _Y) ->
    throw(nif_error).

%% @hidden
nif_to_bitmap(_QRCode) ->
    throw(nif_error).
