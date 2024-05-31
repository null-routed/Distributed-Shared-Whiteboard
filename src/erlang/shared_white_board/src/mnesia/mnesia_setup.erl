-module(mnesia_setup).
-export([init/0]).

-include_lib("stdlib/include/assert.hrl").

-record(whiteboard_access, {whiteboard_id_username, whiteboard_id, username, permission}).
-record(whiteboard_users, {whiteboard_id_username, whiteboard_id, username, join_time, websocket_pid}).
-record(whiteboard_strokes_log, {id, whiteboard_id, username, action, stroke_id, data, timestamp}).
-record(redo_stack, {id, stroke_id, whiteboard_id, data, action, username, timestamp}).

init() ->
    setup_mnesia(),
    create_tables_if_not_exist(),
    truncate_whiteboard_users().

% Setup mnesia database
setup_mnesia() ->
    MnesiaDir = "mnesia/" ++ erlang:atom_to_list(node()),
    ok = filelib:ensure_dir(MnesiaDir ++ "/schema.DAT"),
    application:set_env(mnesia, dir, MnesiaDir),
    mnesia:create_schema([node()]),
    mnesia:start(),
    timer:sleep(1000).

% Create tables if they do not exist
create_tables_if_not_exist() ->
    wait_for_mnesia(),
    Tables = [
        {whiteboard_access, record_info(fields, whiteboard_access), set},
        {whiteboard_strokes_log, record_info(fields, whiteboard_strokes_log), set},
        {whiteboard_users, record_info(fields, whiteboard_users), set},
        {redo_stack, record_info(fields, redo_stack), set}
    ],
    lists:foreach(fun({Name, Fields, Type}) -> create_table(Name, Fields, Type) end, Tables).

% Wait for mnesia to start
wait_for_mnesia() ->
        io:format("creating mnesia tables"),
    case mnesia:wait_for_tables([schema], 10000) of
        ok -> io:format("Mnesia operational.~n");
        _ -> io:format("Timeout waiting for Mnesia to start.~n"), exit({mnesia_not_started, {}})
    end.

% Create a table
create_table(Name, Fields, Type) ->
    case mnesia:create_table(Name, [{attributes, Fields}, {disc_copies, [node()]}, {type, Type}]) of
        {atomic, ok} -> io:format("Table ~p created successfully.~n", [Name]);
        {error, {already_exists, Name}} -> io:format("Table ~p already exists.~n", [Name]);
        Error -> io:format("Failed to create table ~p: ~p~n", [Name, Error])
    end.

% Truncate the whiteboard_users table on node startup
truncate_whiteboard_users() ->
    case mnesia:table_info(whiteboard_users, size) > 0 of
        true ->
            mnesia:clear_table(whiteboard_users),
            io:format("Table whiteboard_users truncated successfully.~n");
        false ->
            io:format("Table whiteboard_users is already empty or does not exist.~n")
    end.
