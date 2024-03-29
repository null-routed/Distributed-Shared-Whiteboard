-module(mnesia_setup).
-export([init/0, read_env/0, connect_nodes/1, setup_mnesia/0, create_tables_if_not_exist/0]).

-include_lib("stdlib/include/assert.hrl").

-record(whiteboard_access, {whiteboard_id, user_id, permission}).
-record(whiteboard_strokes, {stroke_id, whiteboard_id, data, timestamp}).

init() ->
    Nodes = read_env(),
    connect_nodes(Nodes),
    setup_mnesia(),
    create_tables_if_not_exist().

read_env() ->
    {ok, Data} = file:read_file(".env"),
    Lines = string:split(Data, "\n", all),
    Nodes = lists:filtermap(
        fun(Line) ->
            [_, Value] = string:split(Line, "=", all),
            {true, list_to_atom(Value)}
        end, Lines),
    Nodes.

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
    mnesia:start(),
    Nodes = [node() | nodes()],
    mnesia:change_config(extra_db_nodes, Nodes),
    ok.

create_tables_if_not_exist() ->
    Tables = [#whiteboard_access{}, #whiteboard_strokes{}],
    lists:foreach(
        fun(Rec) ->
            Tab = element(1, Rec),
            case mnesia:table_info(Tab, type) of
                {aborted, no_exists} ->
                    mnesia:create_table(Tab, [
                        {attributes, record_info(fields, Tab)},
                        {disc_copies, [node() | nodes()]},
                        {type, set}
                    ]);
                _Type -> ok
            end
        end, Tables).

