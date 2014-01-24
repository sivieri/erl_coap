-module(test).
-export([get_test/2, get_multiple/2]).
-include("eliot.hrl").
-define(TESTS, 10000).
-define(FNAME, "tests.txt").
-record(counter, {clock, series = []}).

% Public API

get_test(Host, Path) ->
    case coap_client:get(Host, Path) of
        {ok, Result} ->
            io:format("~p~n", [Result]);
        {error, Reason} ->
            io:format("CoAP error: ~p~n", Reason)
    end.

get_multiple(Host, Path) ->
    Ck = #counter{clock = clocks:start(clock_gettime)},
    Series = loop(Host, Path, Ck, ?TESTS),
    {ok, Dev} = file:open(?FNAME, [append]),
    io:format(Dev, "~p~n", [Series]),
    file:close(Dev).

% Private API

loop(_, _, #counter{series = Series}, Times) when Times == 0 ->
    Series;
loop(Host, Path, #counter{clock = Clock, series = Series}, Times) when Times > 0 ->
    TmpClock = clocks:acc_start(Clock),
    coap_client:get(Host, Path),
    FinalClock = clocks:acc_stop(TmpClock),
    FinalSeries = [FinalClock#stopwatch.last|Series],
    loop(Host, Path, #counter{clock = FinalClock, series = FinalSeries}, Times - 1).
