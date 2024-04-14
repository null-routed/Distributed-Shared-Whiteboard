-module(jwt_utils).
-export([validate_jwt/1]).

-define(SECRET, <<"12345678912345678912345678912345">>). % secret

decode_jwt(Token) ->
    case jwt:decode(Token, ?SECRET) of
        {ok, Payload} ->
            {ok, Payload};
        {error, _Reason} ->
            error
    end.

get_username_from_payload(Payload) ->
    case maps:get(<<"sub">>, Payload, undefined) of
        undefined -> error;
        Username -> {ok, Username}
    end.

find_jwt_param([]) -> error;
find_jwt_param([{<<"jwt">>, Token}|_]) -> {ok, Token};
find_jwt_param([_|T]) -> find_jwt_param(T).

validate_jwt(Req) ->
    Params = cowboy_req:parse_qs(Req),
    io:format("Params: ~p~n", [Params]),
    case find_jwt_param(Params) of
        {ok, Token} ->
            case decode_jwt(Token) of
                {ok, Payload} ->
                    get_username_from_payload(Payload);
                error ->
                    error
            end;
        error ->
            error
    end.
