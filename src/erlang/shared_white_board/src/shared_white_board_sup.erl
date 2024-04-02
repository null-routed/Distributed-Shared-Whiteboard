-module(shared_white_board_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [],
    {ok, {{one_for_one, 5, 10}, Children}}.
