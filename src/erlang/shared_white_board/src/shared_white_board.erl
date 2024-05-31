-module(shared_white_board).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Node started~n"),

    % Process that monitors the alive nodes
    node_monitor:start_monitor(),

    % Process that handles RPC calls with retries
    rpc_manager:start(),

    % Initialize the mnesia database
    mnesia_setup:init(),

    % Start the shared whiteboard supervisor
    shared_white_board_sup:start_link(),
    
    % Fetch the port for the current node
    CurrentNode = node(),
    NodePortList = application:get_env(shared_white_board, cluster_nodes, []),
    {_, Port} = lists:keyfind(CurrentNode, 1, NodePortList),
    
    % Start Cowboy with the port
    {ok, _} = cowboy_setup:start(Port),
    
    {ok, self()}.

stop(_State) ->
    ok.
