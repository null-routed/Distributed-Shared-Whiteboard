-module(whiteboard).

-export([notify_user_connection/3, notify_user_disconnection/3, check_permissions/2, handle_websocket_message/2]).

% Broadcast a message to all nodes except the origin and current node.
broadcast_message_to_nodes(Func, Params, OriginNode) ->
    case OriginNode == node() of
        false ->
            ok;
        true ->
            Nodes = [Node || Node <- nodes(), Node /= node(), Node /= OriginNode],
            lists:foreach(fun(Node) ->
                              rpc:call(Node, whiteboard, Func, Params)
                          end, Nodes)
    end.

notify_user_disconnection(WhiteboardId, Username, OriginNode) ->
    mnesia_queries:remove_user_connection(WhiteboardId, Username),
    notify_existing_users(WhiteboardId),
    broadcast_message_to_nodes(notify_user_disconnection, [WhiteboardId, Username], OriginNode).

% Handle permissions for a new user connection
check_permissions(WhiteboardId, Username) ->
    PermissionsResult = mnesia_queries:get_permissions(WhiteboardId, Username),
    case PermissionsResult of 
        {ok, Permission} -> 
            {ok, #{username => Username, whiteboardId => WhiteboardId, permission => Permission}, <<>>};
        {error, _} -> 
            {error, {}, <<"No permission to access whiteboard">>}
    end.

% Notify about a user connection
notify_user_connection(WhiteboardId, Username, OriginNode) ->
    ExistingConnection = mnesia_queries:get_user_connection(WhiteboardId, Username),
    io:format("Existing connection: ~p~n", [ExistingConnection]),
    handle_existing_connection(ExistingConnection),
    WebSocketPid = case node() == OriginNode of
        true -> self();
        false -> undefined
    end,
    mnesia_queries:update_or_add_user_connection(WhiteboardId, Username, WebSocketPid),
    notify_existing_users(WhiteboardId),
    broadcast_message_to_nodes(notify_user_connection, [WhiteboardId, Username], OriginNode).

% Handle an existing user connection
handle_existing_connection({ok, OldPid}) when is_pid(OldPid) ->
    io:format("Closing old connection for user ~p~n", [OldPid]),
    OldPid ! {close, <<"Connected from another location">>},
    ok;
handle_existing_connection(_) ->
    ok.

notify_existing_users(WhiteboardId) ->
    ExistingUsers = mnesia_queries:get_connected_users(WhiteboardId),
    UsernamesList = [Username || {Username, _Pid} <- ExistingUsers],
    EncodedMessage = jsx:encode(#{action => <<"updateUserList">>, users => UsernamesList}),
    lists:foreach(fun({_, Pid}) ->
                      Pid ! {send, EncodedMessage}
                  end, ExistingUsers).

notify_new_stroke(StrokeId, WhiteboardId, StrokeData, TempId) ->
    ExistingUsers = mnesia_queries:get_connected_users(WhiteboardId),
    EncodedMessage = jsx:encode(#{
        action => <<"addStroke">>, 
        strokeId => StrokeId,
        data => StrokeData,
        tempId => TempId
    }),
    lists:foreach(fun({_, Pid}) ->
                    Pid ! {send, EncodedMessage}
                end, ExistingUsers).

handle_add_stroke(StrokeId, WhiteboardId, Username, StrokeData, TempId, Timestamp) ->
    mnesia_queries:add_stroke(StrokeId, WhiteboardId, Username, StrokeData, Timestamp),
    notify_new_stroke(StrokeId, WhiteboardId, StrokeData, TempId),
    broadcast_message_to_nodes(handle_add_stroke, [StrokeId, WhiteboardId, Username, StrokeData, TempId, Timestamp], node()).

handle_cursor_position_update(WhiteboardId, Username, Data) -> 
    ExistingUsers = mnesia_queries:get_connected_users(WhiteboardId),
    EncodedMessage = jsx:encode(#{
        action => <<"updateCursorPosition">>, 
        username => Username,
        data => Data
    }),
    lists:foreach(fun({_, Pid}) ->
                    Pid ! {send, EncodedMessage}
                end, ExistingUsers).

handle_websocket_message(Map, State) ->
    #{username := Username, whiteboardId := WhiteboardId} = State,
    case maps:get(<<"action">>, Map) of
        <<"addStroke">> ->
            StrokeId = unique_id_gen:generate_unique_id(),
            Timestamp = erlang:timestamp(),
            #{data := Data, tempId := TempId} = Map,
            handle_add_stroke(StrokeId, WhiteboardId, Username, Data, TempId, Timestamp);
        <<"updateCursorPosition">> ->
            #{data := Data} = Map,
            handle_cursor_position_update(WhiteboardId, Username, Data);
        % <<"undoStroke">> ->
        % <<"redoStroke">> ->
        _ ->
            ok
    end.