-module(whiteboard).

-export([
    handle_stroke_operation/5,
    notify_user_connection/3,
    notify_user_disconnection/3,
    check_permissions/2,
    regenerate_strokes/2,
    handle_whiteboard_change/4
]).

-record(whiteboard_strokes_log, {id, whiteboard_id, username, action, stroke_id, data, timestamp}).

%%% Helper Functions
%%% These functions are used to manage the whiteboard strokes and user connections.

send_message(Pid, Message) ->
    EncodedMessage = jsx:encode(Message),
    Pid ! {send, EncodedMessage},
    ok.

broadcast_to_other_nodes(Func, Params, OriginNode) ->
    Nodes = [Node || Node <- nodes(), Node /= node(), Node /= OriginNode],
    lists:foreach(fun(Node) ->
                      rpc:call(Node, ?MODULE, Func, Params ++ [OriginNode])
                  end, Nodes),
    ok.

%%% User connection / disconnection notifications
%%% These functions are used to notify all users connected to a whiteboard when a new user connects or disconnects.

notify_all_users(WhiteboardId, Action, ExtraProps) ->
    Users = mnesia_queries:get_connected_users(WhiteboardId),
    lists:foreach(fun({_, Pid}) ->
                      send_message(Pid, #{action => Action} ++ ExtraProps)
                  end, Users),
    ok.

handle_user_connection(IsNewConnection, WhiteboardId, Username, WebSocketPid) ->
    ExistingConnection = mnesia_queries:get_user_connection(WhiteboardId, Username),
    case ExistingConnection of
        {ok, OldPid} when is_pid(OldPid) -> OldPid ! {close, <<"Connected from another location">>};
        _ -> ok
    end,
    case IsNewConnection of
        true -> mnesia_queries:update_or_add_user_connection(WhiteboardId, Username, WebSocketPid);
        false -> ok
    end,
    notify_all_users(
        WhiteboardId, <<"updateUserList">>, 
        #{users => [User || {User, _} <- mnesia_queries:get_connected_users(WhiteboardId)]}),

    ok.

check_permissions(WhiteboardId, Username) ->
    case mnesia_queries:get_permissions(WhiteboardId, Username) of
        {ok, Permission} -> 
            {ok, #{username => Username, whiteboard_id => WhiteboardId, permission => Permission}};
        {error, _} -> 
            {error, <<"No permission to access whiteboard">>}
    end.

notify_user_connection(WhiteboardId, Username, OriginNode) ->
    WebSocketPid = if node() == OriginNode -> self(); true -> undefined end,
    handle_user_connection(true, WhiteboardId, Username, WebSocketPid),
    broadcast_to_other_nodes(notify_user_connection, [WhiteboardId, Username], OriginNode).

notify_user_disconnection(WhiteboardId, Username, OriginNode) ->
    mnesia_queries:remove_user_connection(WhiteboardId, Username),
    notify_all_users(
        WhiteboardId, <<"updateUserList">>, #{users => [User || {User, _} <- mnesia_queries:get_connected_users(WhiteboardId)]}),
    broadcast_to_other_nodes(notify_user_disconnection, [WhiteboardId, Username], OriginNode).

regenerate_strokes(WhiteboardId, Username) ->
    case mnesia_queries:get_user_connection(WhiteboardId, Username) of
        {ok, Pid} ->
            ActiveStrokes = mnesia_queries:get_active_strokes(WhiteboardId),
            lists:foreach(fun({StrokeId, Data}) ->
                              send_message(Pid, 
                                #{action => <<"addStroke">>, strokeId => StrokeId, data => Data, tempId => undefined})
                          end, ActiveStrokes);
        _ -> ok
    end.

%%% Managing whiteboard access and changes

handle_whiteboard_change(Action, WhiteboardId, Username, Permission) ->
    case Action of
        insert ->
            new_whiteboard(WhiteboardId, Username, Permission, node());
        delete ->
            remove_whiteboard(WhiteboardId, node());
        removeParticipant ->
            remove_participant(WhiteboardId, Username, node())
    end.

new_whiteboard(WhiteboardId, Username, Permission, OriginNode) ->
    mnesia_queries:add_whiteboard_access(WhiteboardId, Username, Permission),
    broadcast_to_other_nodes(new_whiteboard, [WhiteboardId, Username, Permission], OriginNode).

remove_whiteboard(WhiteboardId, OriginNode) ->
    ConnectedUsers = mnesia_queries:get_connected_users(WhiteboardId),
    lists:foreach(fun({_, Pid}) -> Pid ! {close, <<"This whiteboard has been deleted.">>} end, ConnectedUsers),
    mnesia_queries:remove_whiteboard(WhiteboardId),
    broadcast_to_other_nodes(remove_whiteboard, [WhiteboardId], OriginNode).

remove_participant(WhiteboardId, Username, OriginNode) ->
    case mnesia_queries:get_user_connection(WhiteboardId, Username) of 
        {ok, Pid} -> Pid ! {close, <<"You have been removed from this whiteboard.">>};
        _ -> ok
    end,
    mnesia_queries:remove_participant(WhiteboardId, Username),
    broadcast_to_other_nodes(remove_participant, [WhiteboardId, Username], OriginNode).

%%% Stroke handling (Undo/Redo and Websocket Message Processing)
handle_stroke(StrokeId, Action, WhiteboardId, Username, Data, TempId, Timestamp, OriginNode) ->
    mnesia_queries:log_stroke(StrokeId, WhiteboardId, Action, Username, Data, Timestamp),
    notify_stroke(StrokeId, Action, WhiteboardId, Data, TempId),
    broadcast_to_other_nodes(
        handle_stroke, [StrokeId, Action, WhiteboardId, Username, Data, TempId, Timestamp], OriginNode).

notify_stroke(StrokeId, Action, WhiteboardId, Data, TempId) ->
    ExtraProps = #{strokeId => StrokeId, action => Action, data => Data, tempId => TempId},
    notify_all_users(WhiteboardId, Action, ExtraProps).

handle_cursor_position_update(WhiteboardId, Username, Data, OriginNode) ->
    %% Assuming there's a function to log the cursor position, if needed
    %% mnesia_queries:log_cursor_position(WhiteboardId, Username, Data),
    notify_all_users(WhiteboardId, <<"updateCursorPosition">>, #{username => Username, data => Data}),
    broadcast_to_other_nodes(handle_cursor_position_update, [WhiteboardId, Username, Data], OriginNode).

handle_stroke_operation(WhiteboardId, Username, Action, Map, OriginNode) ->
    case Action of
        undo ->
            case mnesia_queries:undo_stroke(WhiteboardId, Username) of
                {ok, StrokeLog} -> process_stroke_log(StrokeLog, OriginNode);
                {error, no_strokes_found} -> io:format("No strokes found to undo.~n")
            end;
        redo ->
            case mnesia_queries:redo_stroke(WhiteboardId, Username) of
                {ok, RedoLog} -> process_stroke_log(RedoLog, OriginNode);
                {error, no_strokes_found} -> io:format("No strokes found to redo.~n")
            end;
        _ ->
            process_websocket_message(Map, #{username => Username, whiteboard_id => WhiteboardId}, OriginNode)
    end.

process_stroke_log(StrokeLog, OriginNode) ->
    InvertedAction = case StrokeLog#whiteboard_strokes_log.action of
                         add -> delete;
                         delete -> add
                     end,
    notify_stroke(
        StrokeLog#whiteboard_strokes_log.stroke_id, 
        InvertedAction, 
        StrokeLog#whiteboard_strokes_log.whiteboard_id, 
        StrokeLog#whiteboard_strokes_log.data, undefined),
    broadcast_to_other_nodes(
        handle_stroke_operation, 
        [
            StrokeLog#whiteboard_strokes_log.stroke_id, 
            StrokeLog#whiteboard_strokes_log.whiteboard_id, 
            InvertedAction, StrokeLog#whiteboard_strokes_log.username, 
            StrokeLog#whiteboard_strokes_log.data, 
            undefined, 
            StrokeLog#whiteboard_strokes_log.timestamp
        ], 
        OriginNode).

process_websocket_message(Map, #{username := Username, whiteboardId := WhiteboardId}, OriginNode) ->
    Action = maps:get(<<"action">>, Map),
    case Action of
        <<"addStroke">> ->
            StrokeId = unique_id_gen:generate_unique_id(),
            Timestamp = erlang:timestamp(),
            Data = maps:get(<<"data">>, Map, undefined),
            TempId = maps:get(<<"tempId">>, Map, undefined),
            handle_stroke(StrokeId, add, WhiteboardId, Username, Data, TempId, Timestamp, OriginNode);
        <<"deleteStroke">> ->
            StrokeId = maps:get(<<"strokeId">>, Map, undefined),
            Timestamp = erlang:timestamp(),
            handle_stroke(StrokeId, delete, WhiteboardId, Username, undefined, undefined, Timestamp, OriginNode);
        <<"updateCursorPosition">> ->
            Data = maps:get(<<"data">>, Map),
            handle_cursor_position_update(WhiteboardId, Username, Data, OriginNode);
        _ ->
            ok
    end.


