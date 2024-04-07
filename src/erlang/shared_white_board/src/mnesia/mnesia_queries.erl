-module(mnesia_queries).

-export([
    get_permissions/2,
    update_or_add_user_connection/3,
    get_connected_users/1,
    get_user_connection/2,
    remove_user_connection/2,
    add_user_access/3,
    print_all_records/0,
    add_stroke/5
]).

-record(whiteboard_access, {whiteboard_id_username, whiteboard_id, username, permission}).
-record(whiteboard_users, {whiteboard_id_username, whiteboard_id, username, join_time, websocket_pid}).
-record(whiteboard_strokes, {stroke_id, whiteboard_id, data, username, timestamp}).

get_permissions(WhiteboardId, Username) ->
    case mnesia:transaction(fun() ->
            mnesia:match_object(
                #whiteboard_access{
                    whiteboard_id_username = '_', 
                    whiteboard_id = WhiteboardId, 
                    username = Username, 
                    permission = '_'
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
              Record = #whiteboard_users{whiteboard_id_username =  <<WhiteboardId/binary, Username/binary>>, whiteboard_id = WhiteboardId, username = Username, join_time = erlang:timestamp(), websocket_pid = Pid},
              mnesia:write(Record)
          end,
    mnesia:transaction(Fun).

remove_user_connection(WhiteboardId, Username) ->
    Fun = fun() ->
              Records = mnesia:match_object(#whiteboard_users{whiteboard_id = WhiteboardId, username = Username, _ = '_'}),
              lists:foreach(fun(Record) ->
                                mnesia:delete_object(Record)
                            end, Records)
          end,
    mnesia:transaction(Fun).


get_connected_users(WhiteboardId) ->
    MatchPattern = #whiteboard_users{whiteboard_id_username = '_', whiteboard_id = WhiteboardId, username = '_', join_time = '_', websocket_pid = '_'},
    Fun = fun() -> mnesia:match_object(MatchPattern) end,
    case mnesia:transaction(Fun) of
        {atomic, Records} ->
            lists:map(fun(#whiteboard_users{username = Username, websocket_pid = Pid}) -> {Username, Pid} end, Records);
        {aborted, Reason} ->
            {error, Reason}
    end.

get_user_connection(WhiteboardId, Username) ->
    MatchPattern = #whiteboard_users{whiteboard_id_username = '_', whiteboard_id = WhiteboardId, username = Username, _ = '_'},
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
            Record = #whiteboard_access{whiteboard_id_username = <<WhiteboardId/binary, Username/binary>>, whiteboard_id = WhiteboardId, username = Username, permission = Permission},
            mnesia:write(Record)
        end,
    mnesia:transaction(Fun).

add_stroke(StrokeId, WhiteboardId, StrokeData, Username, Timestamp) ->
    Fun = fun() ->
        Record = #whiteboard_strokes{
            stroke_id = StrokeId,
            whiteboard_id = WhiteboardId,
            data = StrokeData,
            username = Username,
            timestamp = Timestamp
        },
        mnesia:write(Record)
    end,
    mnesia:transaction(Fun).

print_all_records() ->
    Fun = fun() ->
        FoldFun = fun(Record, Acc) ->
            io:format("~p~n", [Record]),
            Acc
        end,
        mnesia:foldl(FoldFun, ok, whiteboard_users)
    end,
    mnesia:transaction(Fun).

