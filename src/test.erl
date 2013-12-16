-module(test).
-export([get_test/1]).

get_test(Host) ->
    case coap_client:get(Host, "time") of
        {ok, Result} ->
            io:format("~p~n", [Result]);
        {error, Reason} ->
            io:format("CoAP error: ~p~n", Reason)
    end.
