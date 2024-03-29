-module(jwt_utils).
-export([decode_jwt/1, get_username_from_payload/1]).

-include_lib("jwerl/include/jwerl.hrl").

decode_jwt(Token) ->
    case jwerl:decode(Token, <<"secret">>, [hs256]) of % to do -> loading secret
        {ok, Payload} ->
            {ok, Payload};
        _Error ->
            error
    end.

get_username_from_payload(Payload) ->
    case maps:get(<<"username">>, Payload, undefined) of
        undefined -> error;
        Username -> {ok, Username}
    end.
