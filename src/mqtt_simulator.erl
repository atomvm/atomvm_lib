-module(mqtt_simulator).

-export([
    start/1
]).

start(Pid) ->
    Me = spawn(fun() -> loop(Pid) end),
    Pid ! {mqtt, connected},
    Me.

loop(Pid) ->
    receive
        {_Pid, Ref, {publish, Topic, Data, _Qos, _Retain}} ->
            spawn(fun() -> send_data(Pid, Ref, Topic, Data) end);
        {_Pid, Ref, {subscribe, Topic, _Qos}} ->
            spawn(fun() -> send_subscribed(Pid, Ref, Topic) end)
    end,
    loop(Pid).

send_data(Pid, Ref, Topic, Data) ->
    % erlang:display({send_data, Pid, Ref}),
    Pid ! {Ref, {mqtt, data, Topic, Data}}.

send_subscribed(Pid, Ref, _Topic) ->
    % erlang:display({send_subscribed, Pid, Ref}),
    MsgId = 10,
    Pid ! {Ref, MsgId},
    Pid ! {mqtt, subscribed, MsgId}.
