-module(node_monitor).
-export([start_monitor/0, monitor_loop/0, node_list/0, update_alive_nodes/0]).

-define(UPDATE_INTERVAL, 1000).  % Update interval in milliseconds

% Start monitor process
start_monitor() ->
    Pid = spawn(fun monitor_loop/0),
    register(node_monitor_process, Pid).

% Fetching alive nodes
monitor_loop() ->
    AliveNodes = update_alive_nodes(),
    loop_monitor(AliveNodes).

% Main loop for monitoring nodes
loop_monitor(AliveNodes) ->
    receive
        {request_nodes, From} ->
            From ! {node_list, AliveNodes},
            loop_monitor(AliveNodes);
        _Other ->
            loop_monitor(AliveNodes)
    after ?UPDATE_INTERVAL ->
        NewAliveNodes = update_alive_nodes(),
        loop_monitor(NewAliveNodes)
    end.

% Filtering nodes that are alive
update_alive_nodes() ->
    Nodes = node_list(),
    lists:filter(fun(Node) -> net_adm:ping(Node) == pong end, Nodes).

% Reading the list of nodes from the configuration
node_list() ->
    CurrentNode = node(),
    NodePortList = application:get_env(shared_white_board, cluster_nodes, []),
    [Node || {Node, _Port} <- NodePortList, Node /= CurrentNode].
