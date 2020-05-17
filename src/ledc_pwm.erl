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

%%-----------------------------------------------------------------------------
%% @doc LED Controller high-level APIs
%%
%% @end
%%-----------------------------------------------------------------------------
-module(ledc_pwm).

-export([
    start/0, stop/0,
    create_timer/1, create_timer/2, create_channel/2, create_channel/4,
    get_freq/1, set_freq/2,
    get_dutyp/1, set_dutyp/2,
    fade/3
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-include("ledc.hrl").

-type pin() :: non_neg_integer().
-type freq() :: 1..40000000.
-type dutyp() :: 0..100.
-type timer_ref() :: reference().
-type channel_ref() :: reference().

-define(SERVER_NAME, ?MODULE).

-record(state, {
    timers = [],
    channels = [],
    fade_func_installed = false
}).

-record(timer, {
    freq_hz,
    duty_resolution,
    speed_mode,
    timer_num
}).

-record(channel, {
    gpio_num,
    speed_mode,
    channel,
    timer_ref,
    duty,
    dutyp
}).

%%-----------------------------------------------------------------------------
%% @returns ok | {error, Reason}
%% @doc     Start the ledc_pwm managment process.
%% @end
%%-----------------------------------------------------------------------------
-spec start() -> ok | {error, Reason::term()}.
start() ->
    case gen_server:start({local, ?SERVER_NAME}, ?MODULE, [], []) of
        {ok, _Pid} -> ok;
        Error -> Error
    end.

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Stop all timers and channels associated with the ledc_pwm managment process.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER_NAME, stop).

%%-----------------------------------------------------------------------------
%% @param   FreqHz          the frequency (in hz)
%% @returns {ok, TimerRef} | {error, Reason}
%% @doc     Create a timer.
%%
%%          Equivalent to create_timer(Freq, []).
%% @end
%%-----------------------------------------------------------------------------
-spec create_timer(FreqHz::freq()) -> {ok, Timer::timer_ref()} | {error, Reason::term()}.
create_timer(FreqHz) ->
    create_timer(FreqHz, []).

%%-----------------------------------------------------------------------------
%% @param   FreqHz          the frequency (in hz)
%% @param   Options         (currently unused)
%% @returns {ok, TimerRef} | {error, Reason}
%% @doc     Create a timer.
%%
%%          A timer is associated with a frequency, which determines the
%%          resolution of the duty cycles associated with this timer.
%%          The higher the frequency, the lower the duty cycle resolution.
%%          The frequency should not exceed 1/2 the value of the clock speed
%%          (see esp:freq_hz/0)
%% @end
%%-----------------------------------------------------------------------------
-spec create_timer(FreqHz::freq(), Options::list()) -> {ok, Timer::timer_ref()} | {error, Reason::term()}.
create_timer(FreqHz, Options) ->
    gen_server:call(?SERVER_NAME, {create_timer, FreqHz, Options}).

%%-----------------------------------------------------------------------------
%% @param   TimerRef        a reference to a timer
%% @param   Pin             GPIO pin to associate with this channel
%% @returns {ok, ChannelRef} | {error, Reason}
%% @doc     Create a channel.
%%
%%          Equivalent to create_channel(TimerRef, Pin, 0, []).
%% @end
%%-----------------------------------------------------------------------------
-spec create_channel(TimerRef::timer_ref(), Pin::pin()) -> {ok, ChannelRef::channel_ref()} | {error, Reason::term()}.
create_channel(TimerRef, Pin) ->
    create_channel(TimerRef, Pin, 0, []).

%%-----------------------------------------------------------------------------
%% @param   TimerRef        a reference to a timer
%% @param   Pin             GPIO pin to associate with this channel
%% @param   DutyP           the duty percentage (0..100) to generate PWM
%% @param   Options         (currently unused)
%% @returns {ok, ChannelRef} | {error, Reason}
%% @doc     Create a channel.
%%
%%          A channel is associated with a timer (which determines the frequency
%%          and duty cycle resolution), a GPIO pin, and a duty cycle.  The
%%          duty cycle is expressed as a percentage between 0 and 100, and
%%          can be thought of as the percentage of time in a cycle that the
%%          signal is high.  For example, if the duty cycle percentage is 50,
%%          the signal is high for half of the cycle.
%% @end
%%-----------------------------------------------------------------------------
-spec create_channel(TimerRef::timer_ref(), Pin::pin(), DutyP::dutyp(), Options::list()) -> {ok, ChannelRef::channel_ref()} | {error, Reason::term()}.
create_channel(TimerRef, Pin, DutyP, Options) ->
    gen_server:call(?SERVER_NAME, {create_channel, TimerRef, Pin, DutyP, Options}).

%%-----------------------------------------------------------------------------
%% @param   ChannelRef      a reference to the channel.
%% @returns current frequency (in hz)
%% @doc     Get the the current frequency (in hz) of the timer associated
%%          with this channel.
%% @end
%%-----------------------------------------------------------------------------
-spec get_freq(ChannelRef::channel_ref()) -> freq().
get_freq(ChannelRef) ->
    gen_server:call(?SERVER_NAME, {get_freq, ChannelRef}).

%%-----------------------------------------------------------------------------
%% @param   ChannelRef      a reference to the channel.
%% @param   FreqHz          the frequency (in hz)
%% @returns ok | {error, Reason}
%% @doc     Set the frequency on this channel, in hz.
%%          IMPORTANT: Changing the frequency on a channel will affect all
%%          other channels that share the same timer.  The duty cycle percentage
%%          may not change on any channel connected to the timer, but the
%%          actual duty cycle value will.
%%          The frequency should not exceed 1/2 the value of the clock speed
%%          (see esp:freq_hz/0)
%% @end
%%-----------------------------------------------------------------------------
-spec set_freq(ChannelRef::channel_ref(), FreqHz::freq()) -> ok | {error, Reason::term()}.
set_freq(ChannelRef, FreqHz) ->
    gen_server:call(?SERVER_NAME, {set_freq, ChannelRef, FreqHz}).

%%-----------------------------------------------------------------------------
%% @param   ChannelRef      a reference to the channel.
%% @returns current duty percentage
%% @doc     Get the duty cycle on the specified channel, as a percentage
%% @end
%%-----------------------------------------------------------------------------
-spec get_dutyp(ChannelRef::channel_ref()) -> dutyp().
get_dutyp(ChannelRef) ->
    gen_server:call(?SERVER_NAME, {get_dutyp, ChannelRef}).

%%-----------------------------------------------------------------------------
%% @param   ChannelRef      a reference to the channel.
%% @param   Dutyp           the duty cycle percentage (0..100)
%% @returns ok | {error, Reason}
%% @doc     Set the duty cycle on the specified channel, as a percentage
%% @end
%%-----------------------------------------------------------------------------
-spec set_dutyp(ChannelRef::channel_ref(), Dutyp::dutyp()) -> ok | {error, Reason::term()}.
set_dutyp(ChannelRef, Dutyp) ->
    gen_server:call(?SERVER_NAME, {set_dutyp, ChannelRef, Dutyp}).

%%-----------------------------------------------------------------------------
%% @param   ChannelRef      a reference to the channel.
%% @param   TargetDutyp     the target duty cycle percentage (0..100)
%% @param   FadeMs          amount of time (in ms) to fade
%% @returns ok | {error, Reason}
%% @doc     Fade to the target duty cycle on the specified channel over a specified amount of time.
%%          Control returns immediate to the calling process, and the fade will occur smmothly
%%          and automatically without explicit commands.
%% @end
%%-----------------------------------------------------------------------------
-spec fade(ChannelRef::channel_ref(), TargetDutyp::dutyp(), FadeMs::non_neg_integer()) -> ok | {error, Reason::term()}.
fade(ChannelRef, TargetDutyp, FadeMs) ->
    gen_server:call(?SERVER_NAME, {fade, ChannelRef, TargetDutyp, FadeMs}).

%%
%% gen_server API
%%

%% @hidden
init(_Args) ->
    {ok, #state{}}.

%% @hidden
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({create_timer, Freq, Options}, _From, State) ->
    {NewState, Response} = maybe_create_timer(Freq, Options, State),
    {reply, Response, NewState};
handle_call({create_channel, TimerRef, Pin, DutyP, Options}, _From, State) ->
    {NewState, Response} = maybe_create_channel(TimerRef, Pin, DutyP, Options, State),
    {reply, Response, NewState};
handle_call({get_freq, ChannelRef}, _From, State) ->
    Response = maybe_get_freq(ChannelRef, State#state.timers, State#state.channels),
    {reply, Response, State};
handle_call({set_freq, ChannelRef, FreqHz}, _From, State) ->
    case maybe_set_freq(ChannelRef, FreqHz, State#state.timers, State#state.channels) of
        {error, _Reason} = Error ->
            {reply, Error, State};
        {NewTimers, NewChannels} ->
            {reply, ok, State#state{timers=NewTimers, channels=NewChannels}}
    end;
handle_call({get_dutyp, ChannelRef}, _From, State) ->
    Response = maybe_get_dutyp(ChannelRef, State#state.channels),
    {reply, Response, State};
handle_call({set_dutyp, ChannelRef, Dutyp}, _From, State) ->
    case maybe_set_dutyp(ChannelRef, Dutyp, State#state.timers, State#state.channels) of
        {error, _Reason} = Error ->
            {reply, Error, State};
        NewChannels ->
            {reply, ok, State#state{channels=NewChannels}}
    end;
handle_call({fade, ChannelRef, TargetDutyp, FadeMs}, _From, State) ->
    NewState = case State#state.fade_func_installed of
        true ->
            State;
        false ->
            ledc:fade_func_install(0),
            State#state{fade_func_installed = true}
    end,
    Response = maybe_fade(ChannelRef, TargetDutyp, FadeMs, State#state.channels, State#state.timers),
    {reply, Response, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
    lists:foreach(
        %% TODO fix
        fun({_ChannelRef, #channel{speed_mode=SpeedMode, channel=Channel}}) ->
            ledc:stop(SpeedMode, Channel)
        end,
        State#state.channels
    ).

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal operations
%%

%% @private
maybe_create_timer(Freq, _Options, State) ->
    Timers = State#state.timers,
    case length(Timers) of
        ?LEDC_TIMER_MAX ->
            {State, {error, max_timers_exhausted}};
        NumTimers ->
            case do_create_timer(NumTimers, Freq) of
                {ok, Timer} ->
                    TimerRef = erlang:make_ref(),
                    NewState = State#state{
                        timers = [{TimerRef, Timer} | Timers]
                    },
                    {NewState, {ok, TimerRef}};
                Error ->
                    {State, Error}
            end
end.

%% @private
do_create_timer(TimerNum, Freq) ->
    DutyResolution = get_duty_resolution(Freq),
    SpeedMode = ?LEDC_HIGH_SPEED_MODE,
    TimerConfig = [
        {freq_hz, Freq},
        {duty_resolution, DutyResolution},
        {speed_mode, SpeedMode},
        {timer_num, TimerNum}
    ],
    case ledc:timer_config(TimerConfig) of
        ok ->
            Timer = #timer{
                freq_hz = Freq,
                duty_resolution = DutyResolution,
                speed_mode = SpeedMode,
                timer_num = TimerNum
            },
            {ok, Timer};
        Error ->
            Error
    end.

%% @private
get_duty_resolution(Freq) ->
    ClockFreq = esp:freq_hz(),
    MaxDuty = ClockFreq div Freq,
    num_bits(MaxDuty, 0) - 1.

%% @private
num_bits(0, Accum) ->
    Accum;
num_bits(N, Accum) ->
    num_bits(N div 2, Accum + 1).

%% @private
maybe_create_channel(TimerRef, Pin, DutyP, _Options, State) ->
    case get_value(State#state.timers, TimerRef) of
        {ok, Timer} ->
            Channels = State#state.channels,
            case length(Channels) of
                ?LEDC_CHANNEL_MAX ->
                    {State, {error, max_channels_exhausted}};
                NumChannels ->
                    case do_create_channel(NumChannels, TimerRef, Timer, Pin, DutyP) of
                        {ok, Channel} ->
                            ChannelRef = erlang:make_ref(),
                            NewState = State#state{
                                channels = [{ChannelRef, Channel} | Channels]
                            },
                            {NewState, {ok, ChannelRef}};
                            Error ->
                                {State, Error}
                    end
            end;
        _ ->
            {error, no_such_timer}
    end.


%% @private
do_create_channel(ChannelNum, TimerRef, Timer, Pin, DutyP) ->
    SpeedMode = ?LEDC_HIGH_SPEED_MODE,
    Duty = get_duty(DutyP, Timer#timer.duty_resolution),
    ChannelConfig = [
        {gpio_num, Pin},
        {speed_mode, SpeedMode},
        {channel, ChannelNum},
        {timer_sel, Timer#timer.timer_num},
        {duty, Duty},
        {hpoint, 0}
    ],
    case ledc:channel_config(ChannelConfig) of
        ok ->
            Channel = #channel{
                gpio_num = Pin,
                speed_mode = SpeedMode,
                channel = ChannelNum,
                timer_ref = TimerRef,
                duty = Duty,
                dutyp = DutyP
            },
            {ok, Channel};
        Error ->
            Error
    end.

%% @private
get_duty(0, _DutyResolution) ->
    0;
get_duty(DutyP, DutyResolution) when DutyP > 0 ->
    MaxDuty = power(2, DutyResolution),
    MaxDuty * DutyP div 100.

%% @private
power(N, M) ->
    power(N, M, 1).

%% @private
power(_N, 0, Accum) ->
    Accum;
power(N, M, Accum) ->
    power(N, M - 1, Accum * N).

%% @private
maybe_get_freq(ChannelRef, Timers, Channels) ->
    case get_value(Channels, ChannelRef) of
        {ok, Channel} ->
            TimerRef = Channel#channel.timer_ref,
            {ok, Timer} = get_value(Timers, TimerRef),
            Timer#timer.freq_hz;
        _ ->
            {error, no_such_channel}
    end.

%% @private
maybe_set_freq(ChannelRef, FreqHz, Timers, Channels) ->
    case get_value(Channels, ChannelRef) of
        {ok, Channel} ->
            TimerRef = Channel#channel.timer_ref,
            {ok, Timer} = get_value(Timers, TimerRef),
            case do_create_timer(Timer#timer.timer_num, FreqHz) of
                {ok, NewTimer} ->
                    ChannelNum = Channel#channel.channel,
                    Pin = Channel#channel.gpio_num,
                    DutyP = Channel#channel.dutyp,
                    case do_create_channel(ChannelNum, TimerRef, NewTimer, Pin, DutyP) of
                        {ok, NewChannel} ->
                            NewTimers = [{TimerRef, NewTimer} | lists:keydelete(TimerRef, 1, Timers)],
                            NewChannels = [{ChannelRef, NewChannel} | lists:keydelete(ChannelRef, 1, Channels)],
                            {NewTimers, NewChannels};
                        Error ->
                            {error, {failed_to_update_channel, Error}}
                    end;
                Error ->
                    {error, {failed_to_update_timer, Error}}
            end;
        _ ->
            {error, no_such_channel}
    end.

%% @private
maybe_get_dutyp(ChannelRef, Channels) ->
    case get_value(Channels, ChannelRef) of
        {ok, Channel} ->
            Channel#channel.dutyp;
        _ ->
            {error, no_such_channel}
    end.

%% @private
maybe_set_dutyp(ChannelRef, Dutyp, Timers, Channels) ->
    case get_value(Channels, ChannelRef) of
        {ok, Channel} ->
            ChannelNum = Channel#channel.channel,
            Pin = Channel#channel.gpio_num,
            TimerRef = Channel#channel.timer_ref,
            {ok, Timer} = get_value(Timers, TimerRef),
            case do_create_channel(ChannelNum, TimerRef, Timer, Pin, Dutyp) of
                {ok, NewChannel} ->
                    [{ChannelRef, NewChannel} | lists:keydelete(ChannelRef, 1, Channels)];
                Error ->
                    {error, {failed_to_update_channel, Error}}
            end;
        _ ->
            {error, no_such_channel}
    end.

%% @private
maybe_fade(ChannelRef, TargetDutyp, FadeMs, Channels, Timers) ->
    case get_value(Channels, ChannelRef) of
        {ok, Channel} ->
            SpeedMode = Channel#channel.speed_mode,
            ChannelNum = Channel#channel.channel,
            {ok, Timer} = get_value(Timers, Channel#channel.timer_ref),
            TargetDuty = get_duty(TargetDutyp, Timer#timer.duty_resolution),
            ledc:set_fade_with_time(SpeedMode, ChannelNum, TargetDuty, FadeMs),
            ledc:fade_start(SpeedMode, ChannelNum, ?LEDC_FADE_NO_WAIT);
        _ ->
            {error, no_such_channel}
    end.

get_value(List, Key) ->
    case proplists:get_value(Key, List) of
        undefined ->
            undefined;
        Value ->
            {ok, Value}
    end.
