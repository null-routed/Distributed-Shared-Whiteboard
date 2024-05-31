-module(whiteboard).

-export([
    process_websocket_message/2,
    notify_user_connection/3,
    notify_user_disconnection/3,
    check_permissions/2,
    regenerate_strokes/2,
    handle_whiteboard_change/4,
    new_whiteboard/4,
    handle_undo/3,
    handle_redo/3,
    remove_participant/3,
    remove_whiteboard/2,
    handle_stroke/8,
    handle_cursor_position_update/4
]).

-record(whiteboard_strokes_log, {id, whiteboard_id, username, action, stroke_id, data, timestamp}).

%%% Helper Functions
%%% These functions are used to manage the whiteboard strokes and user connections.

send_message(Pid, Message) ->
    EncodedMessage = jsx:encode(Message),
    Pid ! {send, EncodedMessage},
    ok.

%% Broadcasting to other nodes using RPC cast
broadcast_to_other_nodes(Func, Params, OriginNode) ->
    if 
        OriginNode == node() ->
            io:format("Broadcasting to other nodes.~n"),
            Nodes = get_alive_nodes(),
            io:format("Nodes: ~p~n", [Nodes]),
            lists:foreach(fun(Node) ->
                            case Func of 
                                handle_cursor_position_update -> 
                                    rpc:cast(Node, ?MODULE, Func, Params ++ [OriginNode]);
                                _Port ->
                                    rpc_manager ! {call_rpc, Node, ?MODULE, Func, Params ++ [OriginNode]}
                            end
                          end, Nodes),
            ok;
        true -> 
            ok 
    end.

%% Get the list of alive nodes
get_alive_nodes() ->
    node_monitor_process ! {request_nodes, self()},
    receive
        {node_list, AliveNodes} ->
            io:format("Alive nodes: ~p~n", [AliveNodes]),
            AliveNodes
    after 5000 -> 
        io:format("Timeout waiting for response from node_monitor_process.~n"),
        []
    end.


%%% User connection / disconnection notifications
%%% These functions are used to notify all users connected to a whiteboard when a new user connects or disconnects.

% Function that notifies all users connected to a whiteboard when a new user connects
notify_user_connection(WhiteboardId, Username, OriginNode) ->
    % The WebSocket PID is the same as the process PID if the node is the origin node because
    % the Websocket process is the one that executes this code
    WebSocketPid = if node() == OriginNode -> self(); true -> undefined end,
    handle_user_connection(WhiteboardId, Username, WebSocketPid),
    broadcast_to_other_nodes(notify_user_connection, [WhiteboardId, Username], OriginNode).

% Function that handles the user connection
handle_user_connection(WhiteboardId, Username, WebSocketPid) ->
    % Verifying if the user is already connected from another location
    ExistingConnection = mnesia_queries:get_user_connection(WhiteboardId, Username),
    io:format("Existing connection: ~p~n", [ExistingConnection]),

    % Three possible scenarios:
    % - User is not connected, so the query returns error (no existing connection)
    % - User is connected, but not on the current node, so the query returns undefined
    % - User is already connected on the current node, so the query returns the PID

    case ExistingConnection of
        {ok, OldPid} when is_pid(OldPid) -> OldPid ! {close, <<"Connected from another location">>};
        _ -> ok
    end,
    mnesia_queries:update_or_add_user_connection(WhiteboardId, Username, WebSocketPid),
    notify_all_users(
        WhiteboardId, <<"updateUserList">>,
        #{users => [User || {User, _} <- mnesia_queries:get_connected_users(WhiteboardId)]},
        <<"">>),
    ok.

% Sends a message to all users connected to a whiteboard, excluding the user with the specified username
notify_all_users(WhiteboardId, Action, ExtraProps, ExcludeUsername) ->
    Users = mnesia_queries:get_connected_users(WhiteboardId),
    FilteredUsers = lists:filter(fun({Username, _}) -> Username /= ExcludeUsername end, Users),
    lists:foreach(fun({_, Pid}) ->
        case is_pid(Pid) of
            true -> send_message(Pid, maps:merge(#{action => Action}, ExtraProps));
            false -> ok
        end
    end, FilteredUsers),
    ok.

% Function that regenerates the strokes for a user when they connect to a whiteboard
regenerate_strokes(WhiteboardId, Username) ->
    case mnesia_queries:get_user_connection(WhiteboardId, Username) of
        {ok, Pid} ->
            ActiveStrokes = mnesia_queries:get_active_strokes(WhiteboardId),
            case ActiveStrokes of
                {atomic, Strokes} ->
                    io:format("Strokes: ~p~n", [Strokes]),
                    lists:foreach(fun({StrokeId, Data}) ->
                                    send_message(Pid,
                                        #{action => <<"addStroke">>, strokeId => StrokeId, data => Data, tempId => undefined})
                                end, Strokes);
                _ -> ok
            end;
        _ ->
            ok
    end.

% Functions that checks permissions for a user to access a whiteboard
check_permissions(WhiteboardId, Username) ->
    case mnesia_queries:get_permissions(WhiteboardId, Username) of
        {ok, Permission} -> 
            {ok, #{username => Username, whiteboard_id => WhiteboardId, permission => Permission}};
        {error, _} -> 
            {error, <<"No permission to access whiteboard">>}
    end.

% Function that notifies all users connected to a whiteboard when a user disconnects
notify_user_disconnection(WhiteboardId, Username, OriginNode) ->
    mnesia_queries:remove_user_connection(WhiteboardId, Username),
    notify_all_users(
        WhiteboardId, <<"updateUserList">>, 
        #{users => [User || {User, _} <- mnesia_queries:get_connected_users(WhiteboardId)]},
        Username),
    broadcast_to_other_nodes(notify_user_disconnection, [WhiteboardId, Username], OriginNode).

%%% Managing whiteboard access and changes coming from Java

% This function is called by the Java application to handle whiteboard changes via RPC
handle_whiteboard_change(Action, WhiteboardId, Username, Permission) ->
    io:format("Params: ~p, ~p, ~p, ~p~n", [Action, WhiteboardId, Username, Permission]),
    case Action of
        insert ->
            new_whiteboard(WhiteboardId, Username, Permission, node());
        delete ->
            remove_whiteboard(WhiteboardId, node());
        removeParticipant ->
            remove_participant(WhiteboardId, Username, node());
        _Reason ->
            io:format("Invalid action: ~p~n", [Action])
    end,
    ok.

new_whiteboard(WhiteboardId, Username, Permission, OriginNode) ->
    mnesia_queries:add_whiteboard_access(WhiteboardId, Username, Permission),
    broadcast_to_other_nodes(new_whiteboard, [WhiteboardId, Username, Permission], OriginNode).

remove_whiteboard(WhiteboardId, OriginNode) ->
    ConnectedUsers = mnesia_queries:get_connected_users(WhiteboardId),
    lists:foreach(fun({_, Pid}) ->
                      if is_pid(Pid) ->
                          Pid ! {close, <<"This whiteboard has been deleted.">>};
                      true ->
                          ok
                      end
                  end, ConnectedUsers),
    mnesia_queries:remove_whiteboard(WhiteboardId),
    broadcast_to_other_nodes(remove_whiteboard, [WhiteboardId], OriginNode).

remove_participant(WhiteboardId, Username, OriginNode) ->
    case mnesia_queries:get_user_connection(WhiteboardId, Username) of 
        {ok, Pid} when is_pid(Pid) ->
            Pid ! {close, <<"You have been removed from this whiteboard.">>};
        _ ->
            ok
    end,
    mnesia_queries:remove_user_from_whiteboard(WhiteboardId, Username),
    broadcast_to_other_nodes(remove_participant, [WhiteboardId, Username], OriginNode).

%%% Stroke handling (Undo/Redo and Websocket Message Processing)
process_websocket_message(Map, State) ->
    #{whiteboard_id := WhiteboardId, username := Username} = State,
    Action = maps:get(<<"action">>, Map),
    case Action of
        <<"addStroke">> ->
            StrokeId = unique_id_gen:generate_unique_id(),
            Timestamp = erlang:timestamp(),
            Data = maps:get(<<"data">>, Map, undefined),
            TempId = maps:get(<<"tempId">>, Map, undefined),
            handle_stroke(StrokeId, addStroke, WhiteboardId, Username, Data, TempId, Timestamp, node());
        <<"deleteStroke">> ->
            StrokeId = maps:get(<<"strokeId">>, Map, undefined),
            Timestamp = erlang:timestamp(),
            handle_stroke(StrokeId, deleteStroke, WhiteboardId, Username, undefined, undefined, Timestamp, node());
        <<"updateUserCursor">> ->
            Data = maps:get(<<"data">>, Map),
            handle_cursor_position_update(WhiteboardId, Username, Data, node());
        <<"undo">> ->
            handle_undo(WhiteboardId, Username, node());
        <<"redo">> ->
            handle_redo(WhiteboardId, Username, node());
        _ ->
            ok
    end.

handle_stroke(StrokeId, Action, WhiteboardId, Username, Data, TempId, Timestamp, OriginNode) ->
    io:format("handle_stroke called with params: ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p~n", [StrokeId, Action, WhiteboardId, Username, Data, TempId, Timestamp, OriginNode]),
    mnesia_queries:log_stroke(StrokeId, WhiteboardId, Action, Username, Data, Timestamp),
    notify_stroke(StrokeId, Action, WhiteboardId, Data, TempId),
    broadcast_to_other_nodes(
        handle_stroke, [StrokeId, Action, WhiteboardId, Username, Data, TempId, Timestamp], OriginNode).

notify_stroke(StrokeId, Action, WhiteboardId, Data, TempId) ->
    ExtraProps = #{strokeId => StrokeId, action => Action, data => Data, tempId => TempId},
    notify_all_users(WhiteboardId, Action, ExtraProps, <<"">>).

handle_cursor_position_update(WhiteboardId, Username, Data, OriginNode) ->
    io:format("Handling cursor position update: ~p, ~p, ~p~n", [WhiteboardId, Username, Data]),
    notify_all_users(WhiteboardId, <<"updateUserCursor">>, #{username => Username, data => Data}, Username),
    broadcast_to_other_nodes(handle_cursor_position_update, [WhiteboardId, Username, Data], OriginNode).

handle_undo(WhiteboardId, Username, OriginNode) ->
    case mnesia_queries:undo_stroke(WhiteboardId, Username) of
        {atomic, {ok, StrokeLog}} ->
            io:format("Processing stroke log: ~p~n", [StrokeLog]),
            InvertedAction = case StrokeLog#whiteboard_strokes_log.action of
                                 addStroke -> deleteStroke;
                                 deleteStroke -> addStroke
                             end,
            io:format("Inverted action: ~p~n", [InvertedAction]),
            notify_stroke(
                StrokeLog#whiteboard_strokes_log.stroke_id, 
                InvertedAction, 
                StrokeLog#whiteboard_strokes_log.whiteboard_id, 
                StrokeLog#whiteboard_strokes_log.data, undefined),
            broadcast_to_other_nodes(
                handle_undo, 
                [WhiteboardId, Username], 
                OriginNode);
        {atomic, {error, no_strokes_found}} -> 
            io:format("No strokes found to undo.~n"),
            ok
    end.

handle_redo(WhiteboardId, Username, OriginNode) ->
    case mnesia_queries:redo_stroke(WhiteboardId, Username) of
        {atomic, {ok, StrokeLog}} ->
            notify_stroke(
                StrokeLog#whiteboard_strokes_log.stroke_id, 
                StrokeLog#whiteboard_strokes_log.action, 
                StrokeLog#whiteboard_strokes_log.whiteboard_id, 
                StrokeLog#whiteboard_strokes_log.data, 
                undefined),
            broadcast_to_other_nodes(
                        handle_redo, 
                        [WhiteboardId, Username], 
                        OriginNode);
        {atomic, {error, no_redo_entries_found}} ->
            io:format("No redo entries found.~n"),
            ok
    end.




