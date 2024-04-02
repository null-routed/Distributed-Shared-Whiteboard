-module(websocket_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/3, terminate/2]).

-export([validate_jwt/1]).

init(Req, Opts) ->
    case validate_jwt(Req) of
        {ok, Username} ->
            case cowboy_req:binding(whiteboardId, Req) of
                undefined ->
                    Req2 = cowboy_req:reply(400, #{}, <<"Missing whiteboard ID">>, Req),
                    {ok, Req2, Opts};
                WhiteboardId ->
                    case mnesia_queries:get_permissions(WhiteboardId, Username) of
                        {ok, Permission} ->
                            State = #{username => Username,
                                      whiteboardId => WhiteboardId,
                                      permission => Permission},
                            {cowboy_websocket, Req, State};
                        {error, _Reason} ->
                            Req2 = cowboy_req:reply(401, #{}, <<"Unauthorized: No permissions">>, Req),
                            {ok, Req2, Opts}
                    end
            end;
        error ->
            Req2 = cowboy_req:reply(403, #{}, <<"Forbidden: Invalid JWT token">>, Req),
            {ok, Req2, Opts}
    end.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    #{username := Username, whiteboardId := WhiteboardId} = State,
    io:format("Message from ~s on whiteboard ~s: ~s~n", [Username, WhiteboardId, Msg]),
    {ok, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _State) ->
    ok.

validate_jwt(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case find_jwt_cookie(Cookies) of
        {ok, Token} ->
            case jwt_utils:decode_jwt(Token) of
                {ok, Payload} ->
                    jwt_utils:get_username_from_payload(Payload);
                error ->
                    error
            end;
        error ->
            error
    end.

find_jwt_cookie([]) -> error;
find_jwt_cookie([{<<"jwt">>, Token}|_]) -> {ok, Token};
find_jwt_cookie([_|T]) -> find_jwt_cookie(T).
