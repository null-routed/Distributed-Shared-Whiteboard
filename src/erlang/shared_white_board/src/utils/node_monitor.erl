-module(node_monitor).
-export([start_monitor/0, monitor_loop/0, node_list/0, update_alive_nodes/0]).

-define(UPDATE_INTERVAL, 1000).  % Update interval in milliseconds

start_monitor() ->
    Pid = spawn(fun monitor_loop/0),
    register(node_monitor_process, Pid).

monitor_loop() ->
    AliveNodes = update_alive_nodes(),
    loop_monitor(AliveNodes).

loop_monitor(AliveNodes) ->
    receive
        {request_nodes, From} ->
            From ! {node_list, AliveNodes},
            loop_monitor(AliveNodes)
    after ?UPDATE_INTERVAL ->
        NewAliveNodes = update_alive_nodes(),
        loop_monitor(NewAliveNodes)
    end.

update_alive_nodes() ->
    Nodes = node_list(),
    lists:filter(fun(Node) -> net_adm:ping(Node) == pong end, Nodes).

node_list() ->
    CurrentNode = node(),
    NodePortList = application:get_env(shared_white_board, cluster_nodes, []),
    [Node || {Node, _Port} <- NodePortList, Node /= CurrentNode].
