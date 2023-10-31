%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(json_encoder).
-export([encode/1]).

encode(false) ->
    <<"false">>;
encode(true) ->
    <<"true">>;
encode(nil) ->
    <<"nil">>;
encode(null) ->
    <<"null">>;
encode(undefined) ->
    <<"null">>;
encode(Value) when is_atom(Value) ->
    [$", erlang:atom_to_binary(Value, latin1), $"];
encode(Value) when is_binary(Value) ->
    [$", Value, $"];
encode(Value) when is_float(Value) ->
    erlang:float_to_binary(Value, [{decimals, 32}, compact]);
encode(Value) when is_integer(Value) ->
    erlang:integer_to_binary(Value);
encode(Value) when is_map(Value) ->
    encode_map(Value);
encode(V) ->
    case is_string(V) of
        true ->
            [$",erlang:list_to_binary(V), $"];
        _ ->
            encode(V, [])
    end.

encode([{_K, _V} | _T] = L, []) ->
    encode(L, ${);
encode([{Key, Value} | []], Acc) ->
    Encoded = [$", encode_key(Key), "\":", encode(Value), $}],
    [Acc | Encoded];
encode([{Key, Value} | Tail], Acc) ->
    Encoded = [$", encode_key(Key), "\":", encode(Value), $,],
    encode(Tail, [Acc | Encoded]);
encode([_V | _T] = L, []) ->
    encode(L, $[);
encode([Value | []], Acc) ->
    Encoded = [encode(Value), $]],
    [Acc | Encoded];
encode([Value | Tail], Acc) ->
    Encoded = [encode(Value), $,],
    encode(Tail, [Acc | Encoded]).

encode_key(Key) when is_atom(Key) ->
    erlang:atom_to_binary(Key, latin1);
encode_key(Key) when is_binary(Key) ->
    Key.

is_string([]) ->
    true;
is_string([H|T]) when is_integer(H) andalso 0 =< H andalso H =< 255 ->
    is_string(T);
is_string(_) ->
    false.

encode_map(Map) ->
    iterate_entries(maps:next(maps:iterator(Map)), 0, "{").

%% NB. Output is an iolist, so try to avoid gratuitous copying of data
iterate_entries(none, _K, Accum) ->
    [[Accum] | [$}]];
iterate_entries({Key, Value, Iterator}, K, Accum) ->
    MaybeComma =
        case K of
            0 ->
                "";
            _ ->
                ", "
        end,
    Encoded = [MaybeComma, $", encode_key(Key), "\": ", encode(Value)],
    iterate_entries(maps:next(Iterator), K + 1, [[Accum] | Encoded]).
