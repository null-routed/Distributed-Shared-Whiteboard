-module(shared_white_board).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Node started"),
    mnesia_setup:init(),
    
    shared_white_board_sup:start_link(),
    
    cowboy_setup:start(),
    
    {ok, self()}.

stop(_State) ->
    ok.
