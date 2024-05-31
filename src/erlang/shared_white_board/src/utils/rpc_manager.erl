-module(rpc_manager).
-export([start/0, rpc_process/0, listen/0, handle_rpc/5]).

% Start the RPC manager process
start() ->
    Pid = spawn(?MODULE, rpc_process, []),
    register(rpc_manager, Pid).

rpc_process() ->
    io:format("Process started and listening...~n"),
    listen().

% Listening for RPC calls
listen() ->
    receive
        {call_rpc, Node, Module, Function, Params} ->
            handle_rpc(Node, Module, Function, Params, 0),
            listen();
        {call_rpc, Node, Module, Function, Params, RetryCount} ->
            handle_rpc(Node, Module, Function, Params, RetryCount),
            listen();
        _Other ->
            io:format("Received message: ~p~n", [_Other]),
            listen()
    end.

handle_rpc(Node, Module, Function, Params, RetryCount) when RetryCount < 5 ->
    case rpc:call(Node, Module, Function, Params) of
        {badrpc, Reason} ->
            io:format("RPC call failed due to: ~p. Retry count: ~p. Scheduling retry...~n", [Reason, RetryCount]),
            timer:send_after(5000, self(), {call_rpc, Node, Module, Function, Params, RetryCount + 1});
        Result ->
            io:format("RPC call succeeded with result: ~p~n", [Result])
    end,
    ok;
handle_rpc(Node, Module, Function, Params, RetryCount) when RetryCount >= 5 ->
    io:format("Maximum retry limit reached for {~p, ~p, ~p, ~p}. Aborting...~n", [Node, Module, Function, Params]),
    ok.
