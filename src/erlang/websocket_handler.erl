-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/3, websocket_info/3, terminate/3]).

-export([validate_jwt/1]).

init(Req, Opts) ->
    case validate_jwt(Req) of
        {ok, Username} ->
            {ok, WhiteboardId, Req1} = cowboy_req:binding(whiteboardId, Req),
            {cowboy_websocket, Req1, #{username => Username, whiteboardId => WhiteboardId}};
        error ->
            {reply, {close, 4000, <<"Invalid JWT token">>}, Req, Opts, 0}
    end.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, Req, State) ->
    #{username := Username, whiteboardId := WhiteboardId} = State,
    io:format("Message from ~s on whiteboard ~s: ~s~n", [Username, WhiteboardId, Msg]),
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

validate_jwt(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case maps:get(<<"jwt">>, Cookies, undefined) of
        undefined ->
            error;
        Token ->
            case jwt_utils:decode_jwt(Token) of
                {ok, Payload} ->
                    jwt_utils:get_username_from_payload(Payload);
                error ->
                    error
            end
    end.
