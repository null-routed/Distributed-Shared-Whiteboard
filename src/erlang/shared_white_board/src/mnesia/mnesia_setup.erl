-module(mnesia_setup).
-export([init/0, connect_nodes/1, setup_mnesia/0, create_tables_if_not_exist/0]).

-include_lib("stdlib/include/assert.hrl").

-record(whiteboard_strokes, {stroke_id, whiteboard_id, data, username, timestamp}).
-record(whiteboard_access, {whiteboard_id_username, whiteboard_id, username, permission}).
-record(whiteboard_users, {whiteboard_id_username, whiteboard_id, username, join_time, websocket_pid}).

init() ->
    Nodes = application:get_env(shared_white_board, cluster_nodes, []),
    connect_nodes(Nodes),
    setup_mnesia(),
    create_tables_if_not_exist().

connect_nodes(Nodes) ->
    lists:foreach(
        fun(Node) ->
            case net_adm:ping(Node) of
                pong -> ok; 
                pang -> 
                    io:format("Node ~p not reachable.~n", [Node])
            end
        end, Nodes).

setup_mnesia() ->
    MnesiaDir = "mnesia/" ++ erlang:atom_to_list(node()),
    ok = filelib:ensure_dir(MnesiaDir ++ "/schema.DAT"), % Ensure the directory exists
    application:set_env(mnesia, dir, MnesiaDir),
    case mnesia:create_schema([node()]) of
        ok -> io:format("Mnesia schema created successfully.~n");
        {error, _Reason} -> io:format("Mnesia schema already exists or error in creation: ~p~n", [_Reason])
    end,
    mnesia:start(),
    timer:sleep(1000), % Wait for 5 seconds for Mnesia to stabilize
    ok.


create_tables_if_not_exist() ->
    wait_for_mnesia(),
    create_table_whiteboard_access(),
    create_table_whiteboard_strokes(),
    create_connected_user_table().

wait_for_mnesia() ->
    case mnesia:wait_for_tables([schema], 10000) of
        ok -> io:format("Mnesia operational.~n");
        _ -> io:format("Timeout waiting for Mnesia to start.~n"), exit({mnesia_not_started, {}})
    end.

create_table_whiteboard_access() ->
    case mnesia:create_table(whiteboard_access, [
            {attributes, record_info(fields, whiteboard_access)},
            {disc_copies, [node()]},
            {type, set}
        ]) of
        {atomic, ok} ->
            io:format("Table whiteboard_access created successfully.~n");
        {error, {already_exists, whiteboard_access}} ->
            io:format("Table whiteboard_access already exists.~n");
        Error ->
            io:format("Failed to create table whiteboard_access: ~p~n", [Error])
    end.

create_table_whiteboard_strokes() ->
    case mnesia:create_table(whiteboard_strokes, [
            {attributes, record_info(fields, whiteboard_strokes)},
            {disc_copies, [node()]},
            {type, set}
        ]) of
        {atomic, ok} ->
            io:format("Table whiteboard_strokes created successfully.~n");
        {error, {already_exists, whiteboard_strokes}} ->
            io:format("Table whiteboard_strokes already exists.~n");
        Error ->
            io:format("Failed to create table whiteboard_strokes: ~p~n", [Error])
    end.

create_connected_user_table() ->
    case mnesia:create_table(whiteboard_users, [
            {attributes, record_info(fields, whiteboard_users)},
            {disc_copies, [node()]},
            {type, set}
        ]) of
        {atomic, ok} ->
            io:format("Table whiteboard_users created successfully.~n");
        {error, {already_exists, whiteboard_users}} ->
            io:format("Table whiteboard_users already exists.~n");
        Error ->
            io:format("Failed to create table whiteboard_users: ~p~n", [Error])
    end.