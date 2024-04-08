-module(whiteboard).

-export([notify_user_connection/3, notify_user_disconnection/3, check_permissions/2, handle_websocket_message/2, manage_whiteboard_access/4]).

-record(whiteboard_strokes_log, {id, whiteboard_id, username, action, stroke_id, data, timestamp}).
-record(redo_stack, {id, stroke_id, whiteboard_id, data, action, username, timestamp}).

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

notify_stroke(StrokeId, Action, WhiteboardId, StrokeData, TempId) ->
    ExistingUsers = mnesia_queries:get_connected_users(WhiteboardId),
    EncodedMessage =
    case Action of
        add ->
            jsx:encode(#{
                action => <<"addStroke">>,
                strokeId => StrokeId,
                data => StrokeData,
                tempId => TempId
            });
        delete ->
            jsx:encode(#{
                action => <<"deleteStroke">>,
                strokeId => StrokeId
            })
    end,
    lists:foreach(fun({_, Pid}) ->
                    Pid ! {send, EncodedMessage}
                end, ExistingUsers).

handle_stroke(StrokeId, Action, WhiteboardId, Username, StrokeData, TempId, Timestamp, OriginNode) ->
    mnesia_queries:log_stroke(StrokeId, WhiteboardId, Action, Username, StrokeData, Timestamp),
    notify_stroke(StrokeId, Action, WhiteboardId, StrokeData, TempId),
    broadcast_message_to_nodes(handle_add_stroke, [StrokeId, WhiteboardId, Action, Username, StrokeData, TempId, Timestamp], OriginNode).

handle_cursor_position_update(WhiteboardId, Username, Data, OriginNode) -> 
    ExistingUsers = mnesia_queries:get_connected_users(WhiteboardId),
    EncodedMessage = jsx:encode(#{
        action => <<"updateCursorPosition">>, 
        username => Username,
        data => Data
    }),
    lists:foreach(fun({_, Pid}) ->
                    Pid ! {send, EncodedMessage}
                end, ExistingUsers),
    broadcast_message_to_nodes(handle_cursor_position_update, [WhiteboardId, Username, Data], OriginNode).


handle_new_whiteboard_access(WhiteboardId, Username, Permission, OriginNode) ->
    mnesia_queries:add_whiteboard_access(WhiteboardId, Username, Permission),
    broadcast_message_to_nodes(handle_new_whiteboard_access, [WhiteboardId, Username, Permission], OriginNode).

handle_whiteboard_removal(WhiteboardId, OriginNode) ->
    ConnectedUsers = mnesia_queries:get_connected_users(WhiteboardId),
        lists:foreach(fun({_, Pid}) ->
                    Pid ! {close, <<"This whiteboard has been deleted.">>}
                end, ConnectedUsers),
    mnesia_queries:remove_whiteboard(WhiteboardId),
    broadcast_message_to_nodes(handle_whiteboard_removal, [WhiteboardId], OriginNode).

manage_whiteboard_access(Action, WhiteboardId, Username, Permission) ->
    % Action -> Insert
    case Action of
        insert -> % Sia per share che per nuova whiteboard
            handle_new_whiteboard_access(WhiteboardId, Username, Permission, node());
        delete -> % Proprietario cancella la whiteboard
            handle_whiteboard_removal(WhiteboardId, node());
        removeParticipant -> % Proprietario rimuove un partecipante oppure un partecipante (non proprietario) elimina se stesso dalla whiteboard 
            mnesia_queries:remove_participant(WhiteboardId, Username),
            mnesia_queries:remove_user_connection(WhiteboardId, Username),
            notify_existing_users(WhiteboardId) 
    end.

handle_undo(WhiteboardId, Username, OriginNode) ->
    case mnesia_queries:undo_stroke(WhiteboardId, Username) of
        {ok, #whiteboard_strokes_log{stroke_id = StrokeId, whiteboard_id = WhiteboardId, data = StrokeData, action = Action, username = Username, timestamp = Timestamp}} ->
            InvertedAction = case Action of
                add -> delete;
                delete -> add
            end,
            notify_stroke(StrokeId, InvertedAction, WhiteboardId, StrokeData, undefined),
            broadcast_message_to_nodes(handle_undo, [StrokeId, WhiteboardId, Action, Username, StrokeData, undefined, Timestamp], OriginNode);
        {error, no_strokes_found} ->
            io:format("No strokes found to undo.~n")
end.

handle_redo(WhiteboardId, Username, OriginNode) ->
    case mnesia_queries:redo_stroke(WhiteboardId, Username) of
        {ok, #redo_stack{stroke_id = StrokeId, whiteboard_id = WhiteboardId, data = StrokeData, action = Action, username = Username, timestamp = Timestamp}} ->
            notify_stroke(StrokeId, Action, WhiteboardId, StrokeData, undefined),
            broadcast_message_to_nodes(handle_redo, [StrokeId, WhiteboardId, Action, Username, StrokeData, undefined, Timestamp], OriginNode);
        {error, no_strokes_found} ->
            io:format("No strokes found to redo.~n")
end.

handle_websocket_message(Map, State) ->
    #{username := Username, whiteboardId := WhiteboardId} = State,
    case maps:get(<<"action">>, Map) of
        <<"addStroke">> ->
            StrokeId = unique_id_gen:generate_unique_id(),
            Timestamp = erlang:timestamp(),
            #{data := Data, tempId := TempId} = Map,
            handle_stroke(StrokeId, add, WhiteboardId, Username, Data, TempId, Timestamp, node());
        <<"deleteStroke">> ->
            #{strokeId := StrokeId} = Map,
            Timestamp = erlang:timestamp(),
            handle_stroke(StrokeId, delete, WhiteboardId, Username, undefined, undefined, Timestamp, node());
        <<"updateCursorPosition">> ->
            #{data := Data} = Map,
            handle_cursor_position_update(WhiteboardId, Username, Data, node());
        <<"undoStroke">> ->
            handle_undo(WhiteboardId, Username, node());
        <<"redoStroke">> ->
            handle_redo(WhiteboardId, Username, node());
        _ ->
            ok
    end.