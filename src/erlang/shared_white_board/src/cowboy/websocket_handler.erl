-module(websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%%% Connection callback that handles the initial HTTP request.
init(Req, Opts) ->
    case jwt_utils:validate_jwt(Req) of
        {ok, Username} ->
            io:format("User ~p connected.~n", [Username]),
            validate_whiteboard_id(Req, Username, Opts);
        error ->
            io:format("Invalid JWT token.~n"),
            reply_with_error(Req, 403, <<"Forbidden: Invalid JWT token">>, Opts)
    end.

%%% Validates the presence of the whiteboardId and checks user permissions.
validate_whiteboard_id(Req, Username, Opts) ->
    case cowboy_req:binding(whiteboardId, Req) of
        undefined ->
            reply_with_error(Req, 400, <<"Missing whiteboard ID">>, Opts);
        WhiteboardId ->
            check_user_permissions(Req, WhiteboardId, Username, Opts)
    end.

%%% Checks user permissions for the specified whiteboard.
check_user_permissions(Req, WhiteboardId, Username, Opts) ->
    case whiteboard:check_permissions(WhiteboardId, Username) of
        {ok, State} ->
            {cowboy_websocket, Req, State};
        {error, Message} ->
            reply_with_error(Req, 403, Message, Opts)
    end.

%%% Replies with an error message and status code.
reply_with_error(Req, StatusCode, Message, Opts) ->
    Req2 = cowboy_req:reply(StatusCode, #{}, Message, Req),
    {ok, Req2, Opts}.

%%% Callback that handles websocket upgrade (from HTTP)
websocket_init(State) ->
    #{username := Username, whiteboard_id := WhiteboardId} = State,
    whiteboard:notify_user_connection(WhiteboardId, Username, node()),
    whiteboard:regenerate_strokes(WhiteboardId, Username),
    {ok, State}.

%%% Callback that handles incoming WebSocket messages from the client
websocket_handle({text, Msg}, State) ->
    #{permission := Permission} = State,
    case Permission of
        1 -> 
            case decode_message(Msg) of
                {ok, Map} -> whiteboard:process_websocket_message(Map, State);
                error -> ok
            end;
        _ -> ok
    end,
    {ok, State}.

%%% Decodes a JSON-formatted string message into a map.
decode_message(Msg) ->
    try jsx:decode(Msg, [return_maps]) of
        Map when is_map(Map) -> 
            {ok, Map}
    catch
        _:_ -> error
    end.

%%% Handles Erlang messages to the Websocket Process
websocket_info({close, Reason}, State) ->
    {[{close, 1000, Reason}], State};
websocket_info({send, Message}, State) ->
    {[{text, Message}], State};
websocket_info(_, State) -> 
    {ok, State}.

%%% Callback called when the WebSocket connection is terminated by the client.
terminate(_Reason, _Req, State) ->
    case State of
        #{username := Username, whiteboard_id := WhiteboardId} ->
            whiteboard:notify_user_disconnection(WhiteboardId, Username, node()),
            ok;
        _ ->
            ok
    end.

