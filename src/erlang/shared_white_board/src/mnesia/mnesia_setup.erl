-module(mnesia_setup).
-export([init/0, connect_nodes/1, setup_mnesia/0, create_tables_if_not_exist/0, truncate_user_access/0]).

-include_lib("stdlib/include/assert.hrl").

-record(whiteboard_access, {whiteboard_id_username, whiteboard_id, username, permission}).
-record(whiteboard_users, {whiteboard_id_username, whiteboard_id, username, join_time, websocket_pid}).
-record(whiteboard_strokes_log, {id, whiteboard_id, username, action, stroke_id, data, timestamp}).
-record(redo_stack, {id, stroke_id, whiteboard_id, data, action, username, timestamp}).

init() ->
    Nodes = application:get_env(shared_white_board, cluster_nodes, []),
    connect_nodes(Nodes),
    setup_mnesia(),
    create_tables_if_not_exist(),
    truncate_user_access().

connect_nodes(Nodes) ->
    lists:foreach(fun(Node) ->
            case net_adm:ping(Node) of
                pong -> ok; 
                pang -> io:format("Node ~p not reachable.~n", [Node])
            end
        end, Nodes).

setup_mnesia() ->
    MnesiaDir = "mnesia/" ++ erlang:atom_to_list(node()),
    ok = filelib:ensure_dir(MnesiaDir ++ "/schema.DAT"),
    application:set_env(mnesia, dir, MnesiaDir),
    mnesia:create_schema([node()]),
    mnesia:start(),
    timer:sleep(1000).

create_tables_if_not_exist() ->
    wait_for_mnesia(),
    Tables = [
        {whiteboard_access, record_info(fields, whiteboard_access), set},
        {whiteboard_strokes_log, record_info(fields, whiteboard_strokes_log), set},
        {whiteboard_users, record_info(fields, whiteboard_users), set},
        {redo_stack, record_info(fields, redo_stack), set}
    ],
    lists:foreach(fun({Name, Fields, Type}) -> create_table(Name, Fields, Type) end, Tables).

wait_for_mnesia() ->
    mnesia:wait_for_tables([schema], 10000) orelse io:format("Timeout waiting for Mnesia to start.~n").

create_table(Name, Fields, Type) ->
    case mnesia:create_table(Name, [{attributes, Fields}, {disc_copies, [node()]}, {type, Type}]) of
        {atomic, ok} -> io:format("Table ~p created successfully.~n", [Name]);
        {error, {already_exists, Name}} -> io:format("Table ~p already exists.~n", [Name]);
        Error -> io:format("Failed to create table ~p: ~p~n", [Name, Error])
    end.

truncate_user_access() ->
    case mnesia:table_info(whiteboard_access, size) > 0 of
        true ->
            mnesia:clear_table(whiteboard_access),
            io:format("Table whiteboard_access truncated successfully.~n");
        false ->
            io:format("Table whiteboard_access is already empty or does not exist.~n")
    end.
