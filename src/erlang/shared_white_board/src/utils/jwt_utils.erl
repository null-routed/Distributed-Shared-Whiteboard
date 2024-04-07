-module(jwt_utils).
-export([decode_jwt/1, get_username_from_payload/1, find_jwt_cookie/1]).

-define(SECRET, <<"yourRandomSecretKey">>). % secret

decode_jwt(Token) ->
    case jwt:decode(Token, ?SECRET) of
        {ok, Payload} ->
            {ok, Payload};
        {error, _Reason} ->
            error
    end.

get_username_from_payload(Payload) ->
    case maps:get(<<"username">>, Payload, undefined) of
        undefined -> error;
        Username -> {ok, Username}
    end.

find_jwt_cookie([]) -> error;
find_jwt_cookie([{<<"jwt">>, Token}|_]) -> {ok, Token};
find_jwt_cookie([_|T]) -> find_jwt_cookie(T).
