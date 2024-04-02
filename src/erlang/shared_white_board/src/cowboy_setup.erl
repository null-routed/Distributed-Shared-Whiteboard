-module(cowboy_setup).
-export([start/0]).

start() ->
    Dispatch = cowboy_router:compile([
                {'_', [
                    {"/ws/:whiteboardId", websocket_handler, []}
                ]}
            ]),
    {ok, _} = cowboy:start_clear(my_http_listener, 
                [{port, 8080}], 
                #{env => #{dispatch => Dispatch}}).
