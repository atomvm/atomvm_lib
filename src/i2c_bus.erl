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
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-module(i2c_bus).

-behaviour(gen_server).

-export([start/1, start_link/1, stop/1, enqueue/3, write_bytes/3, write_bytes/4, read_bytes/3, read_bytes/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([i2c_bus/0]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-type pin() :: non_neg_integer().
-type address() :: non_neg_integer().
-type register() :: non_neg_integer().
-type freq_hz() :: non_neg_integer().
-type options() :: #{
    sda => pin(),
    scl => pin(),
    freq_hz => freq_hz()
}.
-type i2c_bus() :: pid().
-type operation() :: fun((port(), address()) -> any()).
-type operations() :: [operation()].

-define(DEFAULT_OPTIONS, #{freq_hz => 400000}).

-record(state, {
    i2c
}).

%%-----------------------------------------------------------------------------
%% @param   Options
%% @returns {ok, i2c_bus()} on success, or {error, Reason}, on failure
%% @doc     Start the I2C Bus.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Options::options()) -> {ok, i2c_bus()} | {error, Reason::term()}.
start(Options) ->
    gen_server:start(?MODULE, maybe_add_defaults(Options), []).

%%-----------------------------------------------------------------------------
%% @param   Options
%% @returns {ok, i2c_bus()} on success, or {error, Reason}, on failure
%% @doc     Start the I2C Bus.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(Options::options()) -> {ok, i2c_bus()} | {error, Reason::term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, maybe_add_defaults(Options), []).

%%-----------------------------------------------------------------------------
%% @param       Bus a reference to the I2C Bus instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc Stop the I2C Bus.
%%
%% Note. This function is not well tested and its use may result in a memory leak.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(Bus::i2c_bus()) -> ok | {error, Reason::term()}.
stop(Bus) ->
    gen_server:stop(Bus).

%%-----------------------------------------------------------------------------
%% @param       Bus a reference to the Bus instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec enqueue(Bus::i2c_bus(), Address::address(), Operations::operations()) -> ok | {error, Reason::term()}.
enqueue(Bus, Address, Operations) ->
    gen_server:call(Bus, {enqueue, Address, Operations}).

%%-----------------------------------------------------------------------------
%% @param       Bus a reference to the Bus instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(Bus::i2c_bus(), Address::address(), Bytes::binary()) -> ok | {error, Reason::term()}.
write_bytes(Bus, Address, Bytes) ->
    gen_server:call(Bus, {write_bytes, Address, Bytes}).

%%-----------------------------------------------------------------------------
%% @param       Bus a reference to the Bus instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(Bus::i2c_bus(), Address::address(), Register::register(), Bytes::binary()) -> ok | {error, Reason::term()}.
write_bytes(Bus, Address, Register, Bytes) ->
    gen_server:call(Bus, {write_bytes, Address, Register, Bytes}).

%%-----------------------------------------------------------------------------
%% @param       Bus a reference to the Bus instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec read_bytes(Bus::i2c_bus(), Address::address(), Count:: non_neg_integer()) -> {ok, binary()} | {error, Reason :: term()}.
read_bytes(Bus, Address, Count) ->
    gen_server:call(Bus, {read_bytes, Address, Count}).

%%-----------------------------------------------------------------------------
%% @param       Bus a reference to the Bus instance created via start
%% @returns     ok if successful; {error, Reason}, otherwise
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec read_bytes(Bus::i2c_bus(), Address::address(), Register::register(), Count:: non_neg_integer()) -> {ok, binary()} | {error, Reason :: term()}.
read_bytes(Bus, Address, Register, Count) ->
    gen_server:call(Bus, {read_bytes, Address, Register, Count}).

%%
%% gen_server API
%%

%% @hidden
init(Options) ->
    I2C = i2c:open([
        {clock_speed_hz, maps:get(freq_hz, Options)}
        | maps:to_list(Options)
    ]),
    ?TRACE("I2C opened. Options: ~p I2C: ~p", [Options, I2C]),
    State = #state{
        i2c = I2C
    },
    {ok, State}.

%% @hidden
handle_call({enqueue, Address, Operations}, _From, State) ->
    Reply = try_enqueue_operations(State#state.i2c, Address, Operations),
    {reply, Reply, State};
handle_call({write_bytes, Address, Bytes}, _From, State) ->
    ?TRACE("Writing bytes ~p to address ~p i2c ~p", [Bytes, Address, State#state.i2c]),
    Reply = i2c:write_bytes(State#state.i2c, Address, Bytes),
    ?TRACE("Reply: ~p", [Reply]),
    {reply, Reply, State};
handle_call({write_bytes, Address, Register, Bytes}, _From, State) ->
    ?TRACE("Writing bytes ~p to address ~p register ~p i2c ~p", [Bytes, Address, Register, State#state.i2c]),
    Reply = i2c:write_bytes(State#state.i2c, Address, Register, Bytes),
    ?TRACE("Reply: ~p", [Reply]),
    {reply, Reply, State};
handle_call({read_bytes, Address, Count}, _From, State) ->
    ?TRACE("Reading bytes off address ~p count ~p i2c ~p", [Address, Count, State#state.i2c]),
    Reply = i2c:read_bytes(State#state.i2c, Address, Count),
    ?TRACE("Reply: ~p", [Reply]),
    {reply, Reply, State};
handle_call({read_bytes, Address, Register, Count}, _From, State) ->
    ?TRACE("Reading bytes off address ~p register ~p count ~p i2c ~p", [Address, Register, Count, State#state.i2c]),
    Reply = i2c:read_bytes(State#state.i2c, Address, Register, Count),
    ?TRACE("Reply: ~p", [Reply]),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
    io:format("Closing I2C ... ~n"),
    i2c:close(State#state.i2c),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal functions
%%


%% @private
maybe_add_defaults(Options) ->
    maps:merge(?DEFAULT_OPTIONS, Options).

%% @private
try_enqueue_operations(I2C, Address, Operations) ->
    ?TRACE("Enqueing on I2C ~p at address ~p", [I2C, Address]),
    case i2c:begin_transmission(I2C, Address) of
        ok ->
            try
                lists:foreach(
                    fun(Operation) ->
                        ?TRACE("Executing on I2C ~p at address ~p", [I2C, Address]),
                        Operation(I2C, Address)
                    end,
                    Operations
                )
            catch
                _:E ->
                    {error, E}
            after
                i2c:end_transmission(I2C)
            end;
        E ->
            {error, E}
    end.
