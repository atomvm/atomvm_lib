%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(httpd_ota_handler).

-export([init_handler/2, handle_http_req/2]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

%%
%% HTTP request handler implementation (for OTA)
%%

-define(TARGET_PARTITION, <<"app2.avm">>).
-define(ATOMVM_NAMESPACE, atomvm).
-define(BOOT_TARGET_PARTITION_KEY, boot_partition).

-record(state, {
    offset = 0
}).

%% @hidden
init_handler(_PathPrefix, _HandlerConfig) ->
    {ok, #state{}}.

%% @hidden
handle_http_req(#{method := post} = HttpRequest, State) ->
    #{
        headers := Headers,
        body := Body
    } = HttpRequest,
    Offset = State#state.offset,
    case Offset of
        0 ->
            io:format("Erasing partition ~p~n", [?TARGET_PARTITION]),
            esp:partition_erase_range(?TARGET_PARTITION, 0);
        _ ->
            ok
    end,
    BodyLen = erlang:byte_size(Body),
    NewOffset = Offset + BodyLen,
    ContentLength = get_content_length(Headers),
    case NewOffset < ContentLength of
        true ->
            io:format("Offset: ~p ContentLength: ~p BodyLen: ~p~n", [Offset, ContentLength, BodyLen]),
            ok = esp:partition_write(?TARGET_PARTITION, Offset, Body),
            io:format("Wrote ~p bytes at offset ~p to partition ~p.~n", [BodyLen, Offset, ?TARGET_PARTITION]),
            NewState = State#state{offset = NewOffset},
            {noreply, NewState};
        false ->
            io:format("Request complete.~n"),
            ok = esp:partition_write(?TARGET_PARTITION, Offset, Body),
            io:format("Wrote ~p bytes at offset ~p to partition ~p.~n", [BodyLen, Offset, ?TARGET_PARTITION]),
            ok = esp:nvs_set_binary(?ATOMVM_NAMESPACE, ?BOOT_TARGET_PARTITION_KEY, ?TARGET_PARTITION),
            io:format("Set boot partition to ~p~n", [?TARGET_PARTITION]),
            {close, <<"ok">>}
    end;
handle_http_req(_HttpRequest, _State) ->
    {error, internal_server_error}.

get_content_length(Headers) ->
    %% TODO handle case
    erlang:binary_to_integer(maps:get(<<"Content-Length">>, Headers, <<"0">>)).
