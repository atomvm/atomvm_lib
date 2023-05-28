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
-module(map_utils).

-export([deep_map_to_proplist/1, deep_maps_merge/2, remove_entry_in_path/2]).

%% convert a map of maps to a proplist of proplists
deep_map_to_proplist(Map) when is_map(Map) ->
    iterate_entries(maps:next(maps:iterator(Map)), []);
deep_map_to_proplist(Term) ->
    Term.

iterate_entries(none, Accum) ->
    Accum;
iterate_entries({Key, Value, Iterator}, Accum) ->
    iterate_entries(maps:next(Iterator), [{Key, deep_map_to_proplist(Value)} | Accum]).

%% merge a map of maps
deep_maps_merge(Map1, Map2) ->
    iterate_entries(
        maps:next(maps:iterator(Map1)),
        Map2,
        Map2
    ).

%% @private
iterate_entries(none, _Map2, Accum) ->
    Accum;
iterate_entries({Key1, Value1, Iterator1}, Map2, Accum) ->
    NewAccum = case maps:get(Key1, Map2, undefined) of
        undefined ->
            Accum#{Key1 => Value1};
        Value2 when is_map(Value1) andalso is_map(Value2) ->
            Accum#{Key1 => deep_maps_merge(Value1, Value2)};
        Value2 ->
            Accum#{Key1 => Value2}
    end,
    iterate_entries(maps:next(Iterator1), Map2, NewAccum).

%% return the result of removing the entry in Map defined by Path,
%% or `undefined', if the path cannot be found in the map.  The
%% path must reflect a valid path from the root map to a map
%% key in the input map.  The type of the entries in the path
%% must match the keys used in the input map.
%%
%% Example: Map = #{a => #{b => c, d => #{e => f, g => #{h => i}}}}
%%          Path = [a, d, e]            -> #{a, #{b => c, d => #{g => #{h => i}}}}
%%          Path = [a, d, g, h]         -> #{a => #{b => c, d => #{e => f, g => #{}}}}
%%          Path = [a, b]               -> #{a => #{d => #{e => f, g => #{h => i}}}}
%%          Path = [a, d, e, f]         -> undefined
%%          Path = [a, d]               -> #{a, #{b => c}}
%%          Path = [a, c]               -> undefined
%%          Path = [a]                  -> #{}
%%          Path = []                   -> Map
remove_entry_in_path(Map, []) when is_map(Map) ->
    Map;
remove_entry_in_path(Map, [H]) when is_map(Map) ->
    case maps:is_key(H, Map) of
        false ->
            undefined;
        _ ->
            maps:remove(H, Map)
    end;
remove_entry_in_path(Map, [H | T]) when is_map(Map) ->
    case maps:get(H, Map, undefined) of
        undefined ->
            undefined;
        V ->
            case remove_entry_in_path(V, T) of
                undefined ->
                    undefined;
                VPrime ->
                    Map#{H := VPrime}
            end
    end;
remove_entry_in_path(_Map, _Path) ->
    undefined.
