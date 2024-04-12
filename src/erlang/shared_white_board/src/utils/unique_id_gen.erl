-module(unique_id_gen).

-export([generate_unique_id/0]).

generate_unique_id() ->
    RandBytes = crypto:strong_rand_bytes(16),
    HexString = bin_to_hex(RandBytes),
    list_to_binary(HexString).

bin_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Bin)]).
