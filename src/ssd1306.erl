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

%%-----------------------------------------------------------------------------
%% @doc An AtomVM I2C driver for the Solomon Systech SSD1306.
%%
%% Further information about the Solomon Systech SSD1306 can be found in its datasheet:
%% https://cdn-shop.adafruit.com/datasheets/SSD1306.pdf
%%
%% @end
%%-----------------------------------------------------------------------------
-module(ssd1306).

-behaviour(gen_server).

-export([start/1, stop/1, clear/1, set_contrast/2, set_text/2, set_bitmap/4, set_qrcode/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([nif_init/1, nif_clear/1, nif_set_contrast/2, nif_set_text/2, nif_set_bitmap/4, nif_set_qrcode/2]).

-type freq_hz() :: non_neg_integer().
-type ssd1306() :: pid().
-type pin() :: non_neg_integer().
-type i2c_num() :: i2c_num_0 | i2c_num_1.
-type config() :: #{
    i2c_num => i2c_num(),
    sda_pin => pin(),
    scl_pin => pin(),
    freq_hz => freq_hz()
    , use_nif => boolean() %% temporary
}.
-type contrast() :: 0..255.

-define(DEFAULT_CONFIG, #{
    freq_hz => 700000,
    i2c_num => i2c_num_0
    , use_nif => true %% temporary
}).

%%-----------------------------------------------------------------------------
%% @param   Config      configuration map
%% @returns {ok, SSD1306} on success, or {error, Reason}, on failure
%% @doc     Start the SSD1306280 driver.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config::config()) -> {ok, SSD1306::ssd1306()} | {error, Reason::term()}.
start(Config) ->
    gen_server:start(?MODULE, maps:merge(?DEFAULT_CONFIG, Config), []).

%%-----------------------------------------------------------------------------
%% @param       SSD1306 a reference to the SSD1306 instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc Stop the SSD1306280 driver.
%%
%% Note. This function is not well tested and its use may result in a memory leak.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(SSD1306::ssd1306()) -> ok | {error, Reason::term()}.
stop(SSD1306) ->
    gen_server:stop(SSD1306).

%%-----------------------------------------------------------------------------
%% @param       SSD1306 a reference to the SSD1306 instance created via start
%% @returns     ok | {error, Reason}
%% @doc         Clear the contents of the display.
%%
%%              This function will turn off all pixels in the display.
%% @end
%%-----------------------------------------------------------------------------
-spec clear(SSD1306::ssd1306()) -> ok | {error, Reason::term()}.
clear(SSD1306) ->
    gen_server:call(SSD1306, clear).

%%-----------------------------------------------------------------------------
%% @param       SSD1306 a reference to the SSD1306 instance created via start
%% @returns     ok | {error, Reason}
%% @doc         Display the text on the SSD1306.
%%
%%              Text begins at the top left corner of the display.  Use
%%              newlines (`\n') to separate lines.  Only ASCII characters
%%              are supported.  Any unprintable characters will be displayed
%%              as an empty character (equivalent to ` ').  Make sure to
%%              clear the screen before displaying new text.
%% @end
%%-----------------------------------------------------------------------------
-spec set_contrast(SSD1306::ssd1306(), Contrast::contrast()) -> ok | {error, Reason::term()}.
set_contrast(SSD1306, Contrast) when is_integer(Contrast), 0 =< Contrast, Contrast =< 255 ->
    gen_server:call(SSD1306, {set_contrast, Contrast}).

%%-----------------------------------------------------------------------------
%% @param       SSD1306 a reference to the SSD1306 instance created via start
%% @returns     ok | {error, Reason}
%% @doc         Display the text on the SSD1306.
%%
%%              Text begins at the top left corner of the display.  Use
%%              newlines (`\n') to separate lines.  Only ASCII characters
%%              are supported.  Any unprintable characters will be displayed
%%              as an empty character (equivalent to ` ').  Make sure to
%%              clear the screen before displaying new text.
%% @end
%%-----------------------------------------------------------------------------
-spec set_text(SSD1306::ssd1306(), Text::string()) -> ok | {error, Reason::term()}.
set_text(SSD1306, Text) ->
    gen_server:call(SSD1306, {set_text, Text}).

%%-----------------------------------------------------------------------------
%% @param       SSD1306 a reference to the SSD1306 instance created via start
%% @returns     ok | {error, Reason}
%% @doc         Display a Bitmap on the SSD1306.
%%
%%
%% @end
%%-----------------------------------------------------------------------------
-spec set_bitmap(SSD1306::ssd1306(), Bitmap::binary(), Width::non_neg_integer(), Height::non_neg_integer()) -> ok | {error, Reason::term()}.
set_bitmap(SSD1306, Bitmap, Width, Height) ->
    gen_server:call(SSD1306, {set_bitmap, Bitmap, Width, Height}).

%%-----------------------------------------------------------------------------
%% @param       SSD1306 a reference to the SSD1306 instance created via start
%% @returns     ok | {error, Reason}
%% @doc         Display a QRCode on the SSD1306.
%%
%%
%% @end
%%-----------------------------------------------------------------------------
-spec set_qrcode(SSD1306::ssd1306(), QRCode::binary()) -> ok | {error, Reason::term()}.
set_qrcode(SSD1306, QRCode) ->
    gen_server:call(SSD1306, {set_qrcode, QRCode}).

%%
%% gen_server API
%%

-record(state, {
    i2c_num,
    i2c,
    zero,
    font_table,
    use_nif
}).

%% @hidden
init(#{use_nif := true} = Config) ->
    {ok, #state{
        i2c_num = maps:get(i2c_num, Config),
        i2c = ?MODULE:nif_init(Config)
        , use_nif=true %% temporary
    }}.
% init(Config) ->
%     I2C = open_port({spawn, "i2c"}, [
%         {sda_io_num, maps:get(sda_pin, Config)},
%         {scl_io_num, maps:get(scl_pin, Config)},
%         {i2c_clock_hz, maps:get(freq_hz, Config)}
%     ]),
%     ok = initialize_display(I2C),
%     {ok, #state{
%         config = Config,
%         i2c = I2C,
%         zero = create_zero(),
%         font_table = create_font_table(),
%         use_nif=false
%     }}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(clear, _From, #state{use_nif=true} = State) ->
    {reply, ?MODULE:nif_clear(State#state.i2c_num), State};
% handle_call(clear, _From, State) ->
%     {reply, do_clear(State#state.i2c, State#state.zero), State};
handle_call({set_contrast, Contrast}, _From, #state{use_nif=true} = State) ->
    {reply, ?MODULE:nif_set_contrast(State#state.i2c_num, Contrast rem 256), State};
handle_call({set_text, Text}, _From, #state{use_nif=true} = State) ->
    {reply, ?MODULE:nif_set_text(State#state.i2c_num, list_to_binary(Text)), State};
% handle_call({set_text, Text}, _From, State) ->
%     {reply, do_set_text(State#state.i2c, State#state.font_table, Text), State};
handle_call({set_bitmap, Bitmap, Width, Height}, _From, #state{use_nif=true} = State) ->
    {reply, ?MODULE:nif_set_bitmap(State#state.i2c_num, Bitmap, Width, Height), State};
handle_call({set_qrcode, QRCode}, _From, #state{use_nif=true} = State) ->
    {reply, ?MODULE:nif_set_qrcode(State#state.i2c_num, QRCode), State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%

% %% Reference implementation: https://github.com/yanbe/ssd1306-esp-idf-i2c

% %% SLA (16#3C) + WRITE_MODE (16#00) =  16#78 (0b01111000)
% -define(I2C_ADDRESS, 16#3C).
% -define(CONTROL_BYTE_CMD_SINGLE, 16#80).
% -define(CONTROL_BYTE_CMD_STREAM, 16#00).
% -define(CONTROL_BYTE_DATA_STREAM, 16#40).

% -define(CMD_SET_CONTRAST,          16#81).    %% follow with 16#7F
% -define(CMD_DISPLAY_RAM,           16#A4).
% -define(CMD_DISPLAY_ALLON,         16#A5).
% -define(CMD_DISPLAY_NORMAL,        16#A6).
% -define(CMD_DISPLAY_INVERTED,      16#A7).
% -define(CMD_DISPLAY_OFF,           16#AE).
% -define(CMD_DISPLAY_ON,            16#AF).

% %% Addressing Command Table (pg.30)
% -define(CMD_SET_MEMORY_ADDR_MODE,  16#20).    %% follow with 16#00 = HORZ mode = Behave like a KS108 graphic LCD
% -define(CMD_SET_COLUMN_RANGE,      16#21).    %% can be used only in HORZ/VERT mode - follow with 16#00 and 16#7F = COL127
% -define(CMD_SET_PAGE_RANGE,        16#22).    %% can be used only in HORZ/VERT mode - follow with 16#00 and 16#07 = PAGE7

% %% Hardware Config (pg.31)
% -define(CMD_SET_DISPLAY_START_LINE,16#40).
% -define(CMD_SET_SEGMENT_REMAP,     16#A1).
% -define(CMD_SET_MUX_RATIO,         16#A8).    %% follow with 16#3F = 64 MUX
% -define(CMD_SET_COM_SCAN_MODE,     16#C8).
% -define(CMD_SET_DISPLAY_OFFSET,    16#D3).    %% follow with 16#00
% -define(CMD_SET_COM_PIN_MAP,       16#DA).    %% follow with 16#12
% -define(CMD_NOP,                   16#E3).    %% NOP

% %% Timing and Driving Scheme (pg.32)
% -define(CMD_SET_DISPLAY_CLK_DIV,   16#D5).    %% follow with 16#80
% -define(CMD_SET_PRECHARGE,         16#D9).    %% follow with 16#F1
% -define(CMD_SET_VCOMH_DESELCT,     16#DB).    %% follow with 16#30

% %% Charge Pump (pg.62)
% -define(CMD_SET_CHARGE_PUMP,       16#8D).    %% follow with 16#14

% %% @private
% initialize_display(I2C) ->
%     try
%         ok = i2c:begin_transmission(I2C, ?I2C_ADDRESS),

%         ok = i2c:write_byte(I2C, ?CONTROL_BYTE_CMD_STREAM),
%         ok = i2c:write_byte(I2C, ?CMD_SET_CHARGE_PUMP),
%         ok = i2c:write_byte(I2C, 16#14),
%         ok = i2c:write_byte(I2C, ?CMD_SET_SEGMENT_REMAP),  %% reverse left-right mapping
%         ok = i2c:write_byte(I2C, ?CMD_SET_COM_SCAN_MODE),  %% reverse up-bottom mapping
%         ok = i2c:write_byte(I2C, ?CMD_DISPLAY_ON),
%         ok

%     after
%         ok = i2c:end_transmission(I2C)
%     end.

% %% @private
% do_clear(I2C, Zero) ->
%     clear_page(I2C, 0, Zero).

% %% @private
% clear_page(_I2C, 8, _Zero) ->
%     ok;
% clear_page(I2C, PAGE, Zero) ->
%     try
%         ok = i2c:begin_transmission(I2C, ?I2C_ADDRESS),

%         ok = i2c:write_byte(I2C, ?CONTROL_BYTE_CMD_SINGLE),
%         ok = i2c:write_byte(I2C, 16#B0 bor PAGE),
%         ok = i2c:write_byte(I2C, ?CONTROL_BYTE_DATA_STREAM),
%         ok = i2c:write_bytes(I2C, Zero)
%     after
%         ok = i2c:end_transmission(I2C)
%     end,
%     clear_page(I2C, PAGE + 1, Zero).

% %% @private
% do_set_text(I2C, FontTable, Text) ->
%     try
%         ok = i2c:begin_transmission(I2C, ?I2C_ADDRESS),
%         ok = queue_reset_command(I2C)
%     after
%         ok = i2c:end_transmission(I2C)
%     end,
%     set_chars(I2C, FontTable, Text, 0).

% %% @private
% queue_reset_command(I2C) ->
%     ok = i2c:write_byte(I2C, ?CONTROL_BYTE_CMD_STREAM),
%     ok = i2c:write_byte(I2C, 16#00),        %% reset column
%     ok = i2c:write_byte(I2C, 16#10),
%     ok = i2c:write_byte(I2C, 16#B0),        %% reset page
%     ok.

% %% @private
% set_chars(_I2C, _FontTable, [], _Page) ->
%     ok;
% set_chars(I2C, FontTable, [$\n|T], Page) ->
%     try
%         ok = i2c:begin_transmission(I2C, ?I2C_ADDRESS),

%         ok = i2c:write_byte(I2C, ?CONTROL_BYTE_CMD_STREAM),
%         ok = i2c:write_byte(I2C, 16#00),        %% reset column
%         ok = i2c:write_byte(I2C, 16#10),
%         NextPage = Page + 1,
%         ok = i2c:write_byte(I2C, 16#B0 bor NextPage) %% increment page
%     after
%         ok = i2c:end_transmission(I2C)
%     end,
%     set_chars(I2C, FontTable, T, Page + 1);
% set_chars(I2C, FontTable, [Char|T], Page) ->
%     try
%         ok = i2c:begin_transmission(I2C, ?I2C_ADDRESS),

%         ok = i2c:write_byte(I2C, ?CONTROL_BYTE_DATA_STREAM),
%         ok = i2c:write_bytes(I2C, get_font(Char, FontTable))
%     after
%         ok = i2c:end_transmission(I2C)
%     end,
%     set_chars(I2C, FontTable, T, Page).

% %% @private
% get_font(Char, FontTable) ->
%     Offset = Char * 8,
%     <<_:Offset/binary, Font:8/binary, _/binary>> = FontTable,
%     Font.

% %% @private
% create_zero() ->
%     <<
%         0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
%         0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
%         0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
%         0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
%     >>.

% %% @private
% create_font_table() ->
%     <<
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0000 (nul)
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0001
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0002
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0003
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0004
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0005
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0006
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0007
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0008
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0009
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+000A
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+000B
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+000C
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+000D
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+000E
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+000F
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0010
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0011
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0012
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0013
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0014
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0015
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0016
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0017
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0018
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0019
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+001A
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+001B
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+001C
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+001D
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+001E
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+001F
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0020 (space)
%         16#00, 16#00, 16#06, 16#5F, 16#5F, 16#06, 16#00, 16#00,   %% U+0021 (!)
%         16#00, 16#03, 16#03, 16#00, 16#03, 16#03, 16#00, 16#00,   %% U+0022 (")
%         16#14, 16#7F, 16#7F, 16#14, 16#7F, 16#7F, 16#14, 16#00,   %% U+0023 (#)
%         16#24, 16#2E, 16#6B, 16#6B, 16#3A, 16#12, 16#00, 16#00,   %% U+0024 ($)
%         16#46, 16#66, 16#30, 16#18, 16#0C, 16#66, 16#62, 16#00,   %% U+0025 (%)
%         16#30, 16#7A, 16#4F, 16#5D, 16#37, 16#7A, 16#48, 16#00,   %% U+0026 (&)
%         16#04, 16#07, 16#03, 16#00, 16#00, 16#00, 16#00, 16#00,   %% U+0027 (')
%         16#00, 16#1C, 16#3E, 16#63, 16#41, 16#00, 16#00, 16#00,   %% U+0028 (()
%         16#00, 16#41, 16#63, 16#3E, 16#1C, 16#00, 16#00, 16#00,   %% U+0029 ())
%         16#08, 16#2A, 16#3E, 16#1C, 16#1C, 16#3E, 16#2A, 16#08,   %% U+002A (*)
%         16#08, 16#08, 16#3E, 16#3E, 16#08, 16#08, 16#00, 16#00,   %% U+002B (+)
%         16#00, 16#80, 16#E0, 16#60, 16#00, 16#00, 16#00, 16#00,   %% U+002C (,)
%         16#08, 16#08, 16#08, 16#08, 16#08, 16#08, 16#00, 16#00,   %% U+002D (-)
%         16#00, 16#00, 16#60, 16#60, 16#00, 16#00, 16#00, 16#00,   %% U+002E (.)
%         16#60, 16#30, 16#18, 16#0C, 16#06, 16#03, 16#01, 16#00,   %% U+002F (/)
%         16#3E, 16#7F, 16#71, 16#59, 16#4D, 16#7F, 16#3E, 16#00,   %% U+0030 (0)
%         16#40, 16#42, 16#7F, 16#7F, 16#40, 16#40, 16#00, 16#00,   %% U+0031 (1)
%         16#62, 16#73, 16#59, 16#49, 16#6F, 16#66, 16#00, 16#00,   %% U+0032 (2)
%         16#22, 16#63, 16#49, 16#49, 16#7F, 16#36, 16#00, 16#00,   %% U+0033 (3)
%         16#18, 16#1C, 16#16, 16#53, 16#7F, 16#7F, 16#50, 16#00,   %% U+0034 (4)
%         16#27, 16#67, 16#45, 16#45, 16#7D, 16#39, 16#00, 16#00,   %% U+0035 (5)
%         16#3C, 16#7E, 16#4B, 16#49, 16#79, 16#30, 16#00, 16#00,   %% U+0036 (6)
%         16#03, 16#03, 16#71, 16#79, 16#0F, 16#07, 16#00, 16#00,   %% U+0037 (7)
%         16#36, 16#7F, 16#49, 16#49, 16#7F, 16#36, 16#00, 16#00,   %% U+0038 (8)
%         16#06, 16#4F, 16#49, 16#69, 16#3F, 16#1E, 16#00, 16#00,   %% U+0039 (9)
%         16#00, 16#00, 16#66, 16#66, 16#00, 16#00, 16#00, 16#00,   %% U+003A (:)
%         16#00, 16#80, 16#E6, 16#66, 16#00, 16#00, 16#00, 16#00,   %% U+003B (;)
%         16#08, 16#1C, 16#36, 16#63, 16#41, 16#00, 16#00, 16#00,   %% U+003C (<)
%         16#24, 16#24, 16#24, 16#24, 16#24, 16#24, 16#00, 16#00,   %% U+003D (=)
%         16#00, 16#41, 16#63, 16#36, 16#1C, 16#08, 16#00, 16#00,   %% U+003E (>)
%         16#02, 16#03, 16#51, 16#59, 16#0F, 16#06, 16#00, 16#00,   %% U+003F (?)
%         16#3E, 16#7F, 16#41, 16#5D, 16#5D, 16#1F, 16#1E, 16#00,   %% U+0040 (@)
%         16#7C, 16#7E, 16#13, 16#13, 16#7E, 16#7C, 16#00, 16#00,   %% U+0041 (A)
%         16#41, 16#7F, 16#7F, 16#49, 16#49, 16#7F, 16#36, 16#00,   %% U+0042 (B)
%         16#1C, 16#3E, 16#63, 16#41, 16#41, 16#63, 16#22, 16#00,   %% U+0043 (C)
%         16#41, 16#7F, 16#7F, 16#41, 16#63, 16#3E, 16#1C, 16#00,   %% U+0044 (D)
%         16#41, 16#7F, 16#7F, 16#49, 16#5D, 16#41, 16#63, 16#00,   %% U+0045 (E)
%         16#41, 16#7F, 16#7F, 16#49, 16#1D, 16#01, 16#03, 16#00,   %% U+0046 (F)
%         16#1C, 16#3E, 16#63, 16#41, 16#51, 16#73, 16#72, 16#00,   %% U+0047 (G)
%         16#7F, 16#7F, 16#08, 16#08, 16#7F, 16#7F, 16#00, 16#00,   %% U+0048 (H)
%         16#00, 16#41, 16#7F, 16#7F, 16#41, 16#00, 16#00, 16#00,   %% U+0049 (I)
%         16#30, 16#70, 16#40, 16#41, 16#7F, 16#3F, 16#01, 16#00,   %% U+004A (J)
%         16#41, 16#7F, 16#7F, 16#08, 16#1C, 16#77, 16#63, 16#00,   %% U+004B (K)
%         16#41, 16#7F, 16#7F, 16#41, 16#40, 16#60, 16#70, 16#00,   %% U+004C (L)
%         16#7F, 16#7F, 16#0E, 16#1C, 16#0E, 16#7F, 16#7F, 16#00,   %% U+004D (M)
%         16#7F, 16#7F, 16#06, 16#0C, 16#18, 16#7F, 16#7F, 16#00,   %% U+004E (N)
%         16#1C, 16#3E, 16#63, 16#41, 16#63, 16#3E, 16#1C, 16#00,   %% U+004F (O)
%         16#41, 16#7F, 16#7F, 16#49, 16#09, 16#0F, 16#06, 16#00,   %% U+0050 (P)
%         16#1E, 16#3F, 16#21, 16#71, 16#7F, 16#5E, 16#00, 16#00,   %% U+0051 (Q)
%         16#41, 16#7F, 16#7F, 16#09, 16#19, 16#7F, 16#66, 16#00,   %% U+0052 (R)
%         16#26, 16#6F, 16#4D, 16#59, 16#73, 16#32, 16#00, 16#00,   %% U+0053 (S)
%         16#03, 16#41, 16#7F, 16#7F, 16#41, 16#03, 16#00, 16#00,   %% U+0054 (T)
%         16#7F, 16#7F, 16#40, 16#40, 16#7F, 16#7F, 16#00, 16#00,   %% U+0055 (U)
%         16#1F, 16#3F, 16#60, 16#60, 16#3F, 16#1F, 16#00, 16#00,   %% U+0056 (V)
%         16#7F, 16#7F, 16#30, 16#18, 16#30, 16#7F, 16#7F, 16#00,   %% U+0057 (W)
%         16#43, 16#67, 16#3C, 16#18, 16#3C, 16#67, 16#43, 16#00,   %% U+0058 (X)
%         16#07, 16#4F, 16#78, 16#78, 16#4F, 16#07, 16#00, 16#00,   %% U+0059 (Y)
%         16#47, 16#63, 16#71, 16#59, 16#4D, 16#67, 16#73, 16#00,   %% U+005A (Z)
%         16#00, 16#7F, 16#7F, 16#41, 16#41, 16#00, 16#00, 16#00,   %% U+005B ([)
%         16#01, 16#03, 16#06, 16#0C, 16#18, 16#30, 16#60, 16#00,   %% U+005C (\)
%         16#00, 16#41, 16#41, 16#7F, 16#7F, 16#00, 16#00, 16#00,   %% U+005D (])
%         16#08, 16#0C, 16#06, 16#03, 16#06, 16#0C, 16#08, 16#00,   %% U+005E (^)
%         16#80, 16#80, 16#80, 16#80, 16#80, 16#80, 16#80, 16#80,   %% U+005F (_)
%         16#00, 16#00, 16#03, 16#07, 16#04, 16#00, 16#00, 16#00,   %% U+0060 (`)
%         16#20, 16#74, 16#54, 16#54, 16#3C, 16#78, 16#40, 16#00,   %% U+0061 (a)
%         16#41, 16#7F, 16#3F, 16#48, 16#48, 16#78, 16#30, 16#00,   %% U+0062 (b)
%         16#38, 16#7C, 16#44, 16#44, 16#6C, 16#28, 16#00, 16#00,   %% U+0063 (c)
%         16#30, 16#78, 16#48, 16#49, 16#3F, 16#7F, 16#40, 16#00,   %% U+0064 (d)
%         16#38, 16#7C, 16#54, 16#54, 16#5C, 16#18, 16#00, 16#00,   %% U+0065 (e)
%         16#48, 16#7E, 16#7F, 16#49, 16#03, 16#02, 16#00, 16#00,   %% U+0066 (f)
%         16#98, 16#BC, 16#A4, 16#A4, 16#F8, 16#7C, 16#04, 16#00,   %% U+0067 (g)
%         16#41, 16#7F, 16#7F, 16#08, 16#04, 16#7C, 16#78, 16#00,   %% U+0068 (h)
%         16#00, 16#44, 16#7D, 16#7D, 16#40, 16#00, 16#00, 16#00,   %% U+0069 (i)
%         16#60, 16#E0, 16#80, 16#80, 16#FD, 16#7D, 16#00, 16#00,   %% U+006A (j)
%         16#41, 16#7F, 16#7F, 16#10, 16#38, 16#6C, 16#44, 16#00,   %% U+006B (k)
%         16#00, 16#41, 16#7F, 16#7F, 16#40, 16#00, 16#00, 16#00,   %% U+006C (l)
%         16#7C, 16#7C, 16#18, 16#38, 16#1C, 16#7C, 16#78, 16#00,   %% U+006D (m)
%         16#7C, 16#7C, 16#04, 16#04, 16#7C, 16#78, 16#00, 16#00,   %% U+006E (n)
%         16#38, 16#7C, 16#44, 16#44, 16#7C, 16#38, 16#00, 16#00,   %% U+006F (o)
%         16#84, 16#FC, 16#F8, 16#A4, 16#24, 16#3C, 16#18, 16#00,   %% U+0070 (p)
%         16#18, 16#3C, 16#24, 16#A4, 16#F8, 16#FC, 16#84, 16#00,   %% U+0071 (q)
%         16#44, 16#7C, 16#78, 16#4C, 16#04, 16#1C, 16#18, 16#00,   %% U+0072 (r)
%         16#48, 16#5C, 16#54, 16#54, 16#74, 16#24, 16#00, 16#00,   %% U+0073 (s)
%         16#00, 16#04, 16#3E, 16#7F, 16#44, 16#24, 16#00, 16#00,   %% U+0074 (t)
%         16#3C, 16#7C, 16#40, 16#40, 16#3C, 16#7C, 16#40, 16#00,   %% U+0075 (u)
%         16#1C, 16#3C, 16#60, 16#60, 16#3C, 16#1C, 16#00, 16#00,   %% U+0076 (v)
%         16#3C, 16#7C, 16#70, 16#38, 16#70, 16#7C, 16#3C, 16#00,   %% U+0077 (w)
%         16#44, 16#6C, 16#38, 16#10, 16#38, 16#6C, 16#44, 16#00,   %% U+0078 (x)
%         16#9C, 16#BC, 16#A0, 16#A0, 16#FC, 16#7C, 16#00, 16#00,   %% U+0079 (y)
%         16#4C, 16#64, 16#74, 16#5C, 16#4C, 16#64, 16#00, 16#00,   %% U+007A (z)
%         16#08, 16#08, 16#3E, 16#77, 16#41, 16#41, 16#00, 16#00,   %% U+007B (<<)
%         16#00, 16#00, 16#00, 16#77, 16#77, 16#00, 16#00, 16#00,   %% U+007C (|)
%         16#41, 16#41, 16#77, 16#3E, 16#08, 16#08, 16#00, 16#00,   %% U+007D (>>)
%         16#02, 16#03, 16#01, 16#03, 16#02, 16#03, 16#01, 16#00,   %% U+007E (~)
%         16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00    %% U+007F
%     >>.

%% @hidden
nif_init(_Config) ->
    throw(nif_error).

%% @hidden
nif_clear(_I2CNum) ->
    throw(nif_error).

%% @hidden
nif_set_contrast(_I2CNum, _Contrast) ->
    throw(nif_error).

%% @hidden
nif_set_text(_I2CNum, _Data) ->
    throw(nif_error).

%% @hidden
nif_set_bitmap(_I2CNum, _QRCode, _Width, _Height) ->
    throw(nif_error).

%% @hidden
nif_set_qrcode(_I2CNum, _QRCode) ->
    throw(nif_error).
