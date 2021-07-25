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
-module(ssd1306_example).

-export([start/0]).

start() ->
    SSD1306Config = #{
        sda_pin => 21,
        scl_pin => 22
    },
    {ok, SSD1306} = ssd1306:start(SSD1306Config),
    ssd1306:clear(SSD1306),
    ssd1306:set_contrast(SSD1306, 0),
    loop(SSD1306, text).

loop(SSD1306, text) ->
    Text = io_lib:format(
        "  !!AtomVM!!~n~nFree heap:~n~p bytes",
        [erlang:system_info(esp32_free_heap_size)]
    ),
    io:format("Displaying text: ~s~n", [Text]),
    ssd1306:clear(SSD1306),
    ssd1306:set_text(SSD1306, Text),
    timer:sleep(5000),
    loop(SSD1306, qrcode);
loop(SSD1306, qrcode) ->
    Text = io_lib:format("~p", [diag:get_all_proc_info()]),
    QRCode = qrcodegen:encode(Text, 10),

    {Side, BitMap} = qrcodegen:to_bitmap(QRCode),
    io:format("Displaying QRCode: ~p~n", [BitMap]),
    ssd1306:clear(SSD1306),
    ssd1306:set_bitmap(SSD1306, BitMap, Side, Side),

    timer:sleep(5000),
    loop(SSD1306, text).
