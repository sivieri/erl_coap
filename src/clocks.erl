-module(clocks).
-export([start/1, update/1, acc_start/1, acc_stop/1, start_parallel/1]).
-include("eliot.hrl").

% Public API

start_parallel(Type) ->
    Pid = spawn(fun() -> loop(none) end),
    Pid ! {start, Type},
    Pid.

start(times) ->
    Val = unixtime:times(),
    #stopwatch{type = times, start = Val};
start(getrusage) ->
    Val = unixtime:getrusage(),
    #stopwatch{type = getrusage, start = Val};
start(clock) ->
    Val = unixtime:clock(),
    #stopwatch{type = clock, start = Val};
start(gettimeofday) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    #stopwatch{type = gettimeofday, start = Val};
start(clock_gettime) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    #stopwatch{type = clock_gettime, start = Val}.

update(#stopwatch{type = clock, start = Start} = SW) ->
    Val = unixtime:clock(),
    SW#stopwatch{cur = Val - Start};
update(#stopwatch{type = times, start = {S1, S2}} = SW) ->
    {Cur1, Cur2} = unixtime:times(),
    SW#stopwatch{cur = {Cur1 - S1, Cur2 - S2}};
update(#stopwatch{type = getrusage, start = {S1, S2}} = SW) ->
    {Cur1, Cur2} = unixtime:getrusage(),
    SW#stopwatch{cur = {Cur1 - S1, Cur2 - S2}};
update(#stopwatch{type = gettimeofday, start = Start} = SW) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    SW#stopwatch{cur = Val - Start};
update(#stopwatch{type = clock_gettime, start = Start} = SW) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    SW#stopwatch{cur = Val - Start}.

acc_start(#stopwatch{type = times, acc = none} = SW) ->
    Val = unixtime:times(),
    SW#stopwatch{startacc = Val, acc = {0, 0}};
acc_start(#stopwatch{type = times} = SW) ->
    Val = unixtime:times(),
    SW#stopwatch{startacc = Val};
acc_start(#stopwatch{type = getrusage, acc = none} = SW) ->
    Val = unixtime:getrusage(),
    SW#stopwatch{startacc = Val, acc = {0, 0}};
acc_start(#stopwatch{type = getrusage} = SW) ->
    Val = unixtime:getrusage(),
    SW#stopwatch{startacc = Val};
acc_start(#stopwatch{type = clock, acc = none} = SW) ->
    Val = unixtime:clock(),
    SW#stopwatch{startacc = Val, acc = 0};
acc_start(#stopwatch{type = clock} = SW) ->
    Val = unixtime:clock(),
    SW#stopwatch{startacc = Val};
acc_start(#stopwatch{type = gettimeofday, acc = none} = SW) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    SW#stopwatch{startacc = Val, acc = 0};
acc_start(#stopwatch{type = gettimeofday} = SW) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    SW#stopwatch{startacc = Val};
acc_start(#stopwatch{type = clock_gettime, acc = none} = SW) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    SW#stopwatch{startacc = Val, acc = 0};
acc_start(#stopwatch{type = clock_gettime} = SW) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    SW#stopwatch{startacc = Val}.

acc_stop(#stopwatch{type = clock, startacc = Start, acc = Acc} = SW) ->
    Val = unixtime:clock(),
    SW#stopwatch{acc = Acc + Val - Start, last = Val - Start};
acc_stop(#stopwatch{type = times, startacc = {S1, S2}, acc = {Acc1, Acc2}} = SW) ->
    {Cur1, Cur2} = unixtime:times(),
    SW#stopwatch{acc = {Acc1 + Cur1 - S1, Acc2 + Cur2 - S2}, last = {Cur1 - S1, Cur2 - S2}};
acc_stop(#stopwatch{type = getrusage, startacc = {S1, S2}, acc = {Acc1, Acc2}} = SW) ->
    {Cur1, Cur2} = unixtime:getrusage(),
    SW#stopwatch{acc = {Acc1 + Cur1 - S1, Acc2 + Cur2 - S2}, last = {Cur1 - S1, Cur2 - S2}};
acc_stop(#stopwatch{type = gettimeofday, startacc = Start, acc = Acc} = SW) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    SW#stopwatch{acc = Acc + Val - Start, last = Val - Start};
acc_stop(#stopwatch{type = clock_gettime, startacc = Start, acc = Acc} = SW) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    SW#stopwatch{acc = Acc + Val - Start, last = Val - Start}.

% Private API

loop(SW) ->
    receive
        {start, Type} ->
            loop(start(Type));
        update ->
            loop(update(SW));
        acc_start ->
            loop(acc_start(SW));
        acc_stop ->
            loop(acc_stop(SW));
        {get, Pid} ->
            Pid ! SW,
            loop(SW);
        Any ->
            io:format("CLOCK: unknown message ~p~n", [Any]),
            loop(SW)
    end.
