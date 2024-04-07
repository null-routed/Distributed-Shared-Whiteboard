-module(websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-export([validate_jwt/1]).

init(Req, Opts) ->
    case validate_jwt(Req) of
        {ok, Username} ->
            case cowboy_req:binding(whiteboardId, Req) of
                undefined ->
                    Req2 = cowboy_req:reply(400, #{}, <<"Missing whiteboard ID">>, Req),
                    {ok, Req2, Opts};
                WhiteboardId ->
                    %% Store the whiteboardId and username in the state
                    Result = whiteboard:check_permissions(WhiteboardId, Username),
                    case Result of
                        {ok, State, _} ->
                            {cowboy_websocket, Req, State};
                        {error, _, Message} ->
                            Req2 = cowboy_req:reply(403, #{}, Message, Req),
                            {ok, Req2, Opts}
                    end
            end;
        error ->
            Req2 = cowboy_req:reply(403, #{}, <<"Forbidden: Invalid JWT token">>, Req),
            {ok, Req2, Opts}
    end.

websocket_init(State) ->
    #{username := Username, whiteboardId := WhiteboardId} = State,
    io:format("User ~p connected to whiteboard ~p~n", [Username, WhiteboardId]),
    whiteboard:notify_user_connection(WhiteboardId, Username, node()),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    io:format("Received message: ~p~n", [Msg]),
    try jsx:decode(Msg, [return_maps]) of
        Map when is_map(Map) ->
            io:format("Decoded message: ~p~n", [Map]),
            whiteboard:handle_websocket_message(Map, State)
    catch
        _:_ ->
            {ok, State}
    end,
    {ok, State}.

websocket_info({close, Reason}, State) ->
    {[{close, 1000, Reason}], State};
websocket_info({send, Message}, State) ->
    {[{text, Message}], State};
websocket_info(_, State) -> 
    io:format("Unknown message received~n"),
    {ok, State}.

terminate(_Reason, _Req, State) ->
    #{username := Username, whiteboardId := WhiteboardId} = State,
    whiteboard:notify_user_disconnection(WhiteboardId, Username, node()),
    ok.

validate_jwt(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case jwt_utils:find_jwt_cookie(Cookies) of
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

