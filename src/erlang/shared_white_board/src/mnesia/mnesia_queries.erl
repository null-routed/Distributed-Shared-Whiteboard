-module(mnesia_queries).

-export([
    get_permissions/2,
    update_or_add_user_connection/3,
    get_connected_users/1,
    get_user_connection/2,
    remove_user_connection/3,
    add_user_access/3,
    print_all_records/0,
    log_stroke/6,
    undo_stroke/2,
    redo_stroke/2,
    get_active_strokes/1,
    remove_whiteboard/1,
    remove_user_from_whiteboard/2,
    add_whiteboard_access/3
]).

-record(whiteboard_access, {whiteboard_id_username, whiteboard_id, username, permission}).
-record(whiteboard_users, {whiteboard_id_username, whiteboard_id, username, join_time, websocket_pid}).
-record(whiteboard_strokes_log, {id, whiteboard_id, username, action, stroke_id, data, timestamp}).
-record(redo_stack, {id, stroke_id, whiteboard_id, data, action, username, timestamp}).

get_active_strokes(WhiteboardId) ->
    mnesia:transaction(fun() ->
        % Fetch all actions for the whiteboard
        AllActions = mnesia:match_object(#whiteboard_strokes_log{whiteboard_id = WhiteboardId, _ = '_'}),
        
        % Separate Add and Delete Actions
        {AddActions, DeleteActions} = lists:foldl(fun(Action, {AddAcc, DelAcc}) ->
            case Action#whiteboard_strokes_log.action of
                addStroke -> 
                    {[Action | AddAcc], DelAcc};
                deleteStroke -> 
                    {AddAcc, [Action | DelAcc]}
            end
        end, {[], []}, AllActions),
        
        % Extract Stroke IDs from Delete Actions
        DeleteStrokeIds = [Delete#whiteboard_strokes_log.stroke_id || Delete <- DeleteActions],
        
        % Filter out Add Actions that have corresponding Delete Actions
        ActiveStrokes = lists:filter(fun(Add) ->
            not lists:member(Add#whiteboard_strokes_log.stroke_id, DeleteStrokeIds)
        end, AddActions),

        % Sort Active Strokes by Timestamp and extract the needed fields
        SortedActiveStrokes = lists:sort(fun(A, B) ->
            A#whiteboard_strokes_log.timestamp < B#whiteboard_strokes_log.timestamp
        end, ActiveStrokes),
        
        Result = [{Stroke#whiteboard_strokes_log.stroke_id, 
                   Stroke#whiteboard_strokes_log.data} || Stroke <- SortedActiveStrokes],
        
        Result
    end).

remove_whiteboard(WhiteboardId) ->
    Fun = fun() ->
        RedoStackRecords = mnesia:match_object(#redo_stack{whiteboard_id = WhiteboardId, _ = '_'}),
        lists:foreach(fun(Record) -> mnesia:delete_object(Record) end, RedoStackRecords),

        StrokesLogRecords = mnesia:match_object(#whiteboard_strokes_log{whiteboard_id = WhiteboardId, _ = '_'}),
        lists:foreach(fun(Record) -> mnesia:delete_object(Record) end, StrokesLogRecords),

        UsersRecords = mnesia:match_object(#whiteboard_users{whiteboard_id = WhiteboardId, _ = '_'}),
        lists:foreach(fun(Record) -> mnesia:delete_object(Record) end, UsersRecords),

        AccessRecords = mnesia:match_object(#whiteboard_access{whiteboard_id = WhiteboardId, _ = '_'}),
        lists:foreach(fun(Record) -> mnesia:delete_object(Record) end, AccessRecords),

        ok
    end,
    mnesia:transaction(Fun).

remove_user_from_whiteboard(WhiteboardId, Username) ->
    Fun = fun() ->
        RedoStackRecords = mnesia:match_object(#redo_stack{whiteboard_id = WhiteboardId, username = Username, _ = '_'}),
        lists:foreach(fun(Record) -> mnesia:delete_object(Record) end, RedoStackRecords),

        StrokesLogRecords = mnesia:match_object(#whiteboard_strokes_log{whiteboard_id = WhiteboardId, username = Username, _ = '_'}),
        lists:foreach(fun(Record) -> mnesia:delete_object(Record) end, StrokesLogRecords),

        UsersRecords = mnesia:match_object(#whiteboard_users{whiteboard_id = WhiteboardId, username = Username, _ = '_'}),
        lists:foreach(fun(Record) -> mnesia:delete_object(Record) end, UsersRecords),

        AccessRecords = mnesia:match_object(#whiteboard_access{whiteboard_id = WhiteboardId, username = Username, _ = '_'}),
        lists:foreach(fun(Record) -> mnesia:delete_object(Record) end, AccessRecords),

        ok
    end,
    mnesia:transaction(Fun).

add_whiteboard_access(WhiteboardId, Username, Permission) -> 
    Fun = fun() ->
        mnesia:write(#whiteboard_access{
            whiteboard_id_username = <<WhiteboardId/binary, Username/binary>>, 
            whiteboard_id = WhiteboardId, 
            username = Username, 
            permission = Permission
        })
    end,
    mnesia:transaction(Fun).

get_permissions(WhiteboardId, Username) ->
    case mnesia:transaction(fun() ->
            mnesia:match_object(
                #whiteboard_access{
                    whiteboard_id = WhiteboardId, 
                    username = Username, 
                    _ = '_'
                })
        end) of
        {atomic, [Record]} ->
            % A match was found, extract the permission
            {ok, Record#whiteboard_access.permission};
        {atomic, []} ->
            % No match was found
            {error, no_permission};
        {aborted, Reason} ->
            % The query was aborted for some reason
            {error, Reason}
    end.

update_or_add_user_connection(WhiteboardId, Username, Pid) ->
    Fun = fun() ->
              Record = #whiteboard_users{
                whiteboard_id_username =  <<WhiteboardId/binary, Username/binary>>, 
                whiteboard_id = WhiteboardId, 
                username = Username, 
                join_time = erlang:timestamp(), 
                websocket_pid = Pid
            },
              mnesia:write(Record)
          end,
    mnesia:transaction(Fun).

remove_user_connection(WhiteboardId, Username, Pid) ->
    Fun = fun() ->
              Records = mnesia:match_object(
                #whiteboard_users{
                    whiteboard_id = WhiteboardId, 
                    username = Username, 
                    websocket_pid = Pid,
                    _ = '_'
                }),
              lists:foreach(fun(Record) ->
                                mnesia:delete_object(Record)
                            end, Records)
          end,
    mnesia:transaction(Fun).


get_connected_users(WhiteboardId) ->
    MatchPattern = #whiteboard_users{
        whiteboard_id = WhiteboardId, 
        _ = '_'
    },
    Fun = fun() -> mnesia:match_object(MatchPattern) end,
    case mnesia:transaction(Fun) of
        {atomic, Records} ->
            lists:map(
                fun(
                    #whiteboard_users{
                        username = Username, 
                        websocket_pid = Pid}) -> {Username, Pid} 
                end, Records);
        {aborted, Reason} ->
            {error, Reason}
    end.

get_user_connection(WhiteboardId, Username) ->
    MatchPattern = #whiteboard_users{
        whiteboard_id = WhiteboardId, 
        username = Username, 
        _ = '_'},
    Fun = fun() -> mnesia:match_object(MatchPattern) end,
    case mnesia:transaction(Fun) of
        {atomic, [Record]} ->
            {ok, Record#whiteboard_users.websocket_pid};
        {atomic, []} ->
            {error, not_found};
        {aborted, Reason} ->
            {error, Reason}
    end.

add_user_access(WhiteboardId, Username, Permission) ->
        Fun = fun() ->
            Record = #whiteboard_access{
                whiteboard_id_username = <<WhiteboardId/binary, Username/binary>>, 
                whiteboard_id = WhiteboardId, 
                username = Username,
                permission = Permission
            },
            mnesia:write(Record)
        end,
    mnesia:transaction(Fun).

log_stroke(StrokeId, WhiteboardId, Action, Username, StrokeData, Timestamp) ->
    Fun = fun() ->
        % Adjust the data field if action is deleteStroke
        FinalStrokeData = case Action of
            deleteStroke ->
                Res = get_stroke_data_for_deletion(WhiteboardId, StrokeId),
                case Res of
                    {ok, OriginalData} -> 
                        OriginalData;
                    error -> StrokeData % Default to provided StrokeData if not found
                end;
            _ -> 
                StrokeData
        end,
        LogId = unique_id_gen:generate_unique_id(),
        LogRecord = #whiteboard_strokes_log{
            id = LogId,
            stroke_id = StrokeId,
            whiteboard_id = WhiteboardId,
            data = FinalStrokeData,
            action = Action,
            username = Username,
            timestamp = Timestamp
        },
        mnesia:write(LogRecord),
        % Clear redo stack for this user and whiteboard
        clear_redo_stack(WhiteboardId, Username)
    end,
    mnesia:transaction(Fun).

get_stroke_data_for_deletion(WhiteboardId, StrokeId) ->
    MatchPattern = #whiteboard_strokes_log{
        stroke_id = StrokeId,
        whiteboard_id = WhiteboardId,
        action = addStroke,
        _ = '_'},
    Res = mnesia:match_object(MatchPattern),
    case Res of
        [Record] -> % Directly matching the list pattern
            {ok, Record#whiteboard_strokes_log.data};
        [] -> 
            error;
        _ -> error
    end.

clear_redo_stack(WhiteboardId, Username) ->
    RedoStackEntries = mnesia:match_object(
        #redo_stack{
            whiteboard_id = WhiteboardId, 
            username = Username, 
            _ = '_'}),
    lists:foreach(fun(RedoEntry) ->
        mnesia:delete_object(RedoEntry)
    end, RedoStackEntries).


undo_stroke(WhiteboardId, Username) ->
    Fun = fun() ->
        StrokeLogs = mnesia:match_object(#whiteboard_strokes_log{
            whiteboard_id = WhiteboardId, 
            username = Username, _ = '_'}),
        SortedLogs = lists:sort(
            fun(A, B) -> 
                A#whiteboard_strokes_log.timestamp > B#whiteboard_strokes_log.timestamp end, 
                StrokeLogs),
        case SortedLogs of
            [LatestLog | _Rest] ->
                % Create a redo_stack record
                RedoRecord = #redo_stack{
                    id = LatestLog#whiteboard_strokes_log.id,
                    stroke_id = LatestLog#whiteboard_strokes_log.stroke_id,
                    whiteboard_id = LatestLog#whiteboard_strokes_log.whiteboard_id,
                    data = LatestLog#whiteboard_strokes_log.data,
                    action = LatestLog#whiteboard_strokes_log.action,
                    username = LatestLog#whiteboard_strokes_log.username,
                    timestamp = LatestLog#whiteboard_strokes_log.timestamp
                },
                mnesia:write(RedoRecord),
                mnesia:delete_object(LatestLog),
                {ok, LatestLog};
            [] ->
                {error, no_strokes_found}
        end
    end,
    mnesia:transaction(Fun).

redo_stroke(WhiteboardId, Username) ->
    Fun = fun() ->
        RedoEntries = mnesia:match_object(#redo_stack{
            whiteboard_id = WhiteboardId, 
            username = Username, 
            _ = '_'}),
        SortedEntries = lists:sort(
            fun(A, B) -> A#redo_stack.timestamp < B#redo_stack.timestamp end, RedoEntries),
        case SortedEntries of
            [EarliestEntry | _Rest] ->
                LogRecord = #whiteboard_strokes_log{
                    id = EarliestEntry#redo_stack.id,  % Ensure IDs are correctly transferred
                    stroke_id = EarliestEntry#redo_stack.stroke_id,
                    whiteboard_id = EarliestEntry#redo_stack.whiteboard_id,
                    data = EarliestEntry#redo_stack.data,
                    action = EarliestEntry#redo_stack.action,
                    username = EarliestEntry#redo_stack.username,
                    timestamp = EarliestEntry#redo_stack.timestamp
                },
                mnesia:write(LogRecord),
                mnesia:delete_object(EarliestEntry),
                {ok, LogRecord};
            [] ->
                {error, no_redo_entries_found}
        end
    end,
    mnesia:transaction(Fun).


print_all_records() ->  
    Fun = fun() ->
        FoldFun = fun(Record, Acc) ->
            io:format("~p~n", [Record]),
            Acc
        end,
        mnesia:foldl(FoldFun, ok, whiteboard_access)
    end,
    mnesia:transaction(Fun).

