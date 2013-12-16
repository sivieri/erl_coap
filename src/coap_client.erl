-module(coap_client).
-export([get/2]).
-define(MAX_ID, 65536).
-define(TOKEN_LENGTH, 8).
-define(PORT, 5683).
-define(TMP_PORT, 6666).

% Public API

get(Host, URI) ->
    Token = make_token(),
    ID = make_message_id(),
    {ok, PDU} = pdu:make_pdu(0, 1, Token, ID, URI),
    {ok, Address} = inet_parse:address(Host),
    case gen_udp:open(?TMP_PORT, [binary, inet, {active, false}]) of
        {ok, Socket} ->
            gen_udp:send(Socket, Address, ?PORT, PDU),
            Res = case gen_udp:recv(Socket, 0) of
                {ok, {Address, ?PORT, Packet}} ->
                    pdu:get_content(Packet);
                {error, Reason} ->
                    {error, Reason}
            end,
            gen_udp:close(Socket),
            Res;
         {error, Reason} ->
             {error, Reason}
    end.

% Private API

make_token() ->
    make_token(?TOKEN_LENGTH, []).

make_token(Remaining, Acc) when Remaining == 0 ->
    Acc;
make_token(Remaining, Acc) ->
    make_token(Remaining - 1, [random:uniform(256 - 1)|Acc]).

make_message_id() ->
    random:uniform(?MAX_ID) - 1.
