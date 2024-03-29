-module(websocket_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    mnesia_setup:init(),
    Dispatch = cowboy_router:compile([
                {'_', [
                    {"/ws/:whiteboardId", websocket_handler, []}
                ]}
            ]),
    {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    websocket_sup:start_link().

stop(_State) ->
    ok.
