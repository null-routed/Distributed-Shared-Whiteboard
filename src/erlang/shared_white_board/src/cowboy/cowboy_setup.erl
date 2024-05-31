-module(cowboy_setup).
-export([start/1]).

start(Port) ->
    Dispatch = cowboy_router:compile([
                {'_', [
                    {"/ws/:whiteboardId", websocket_handler, []}
                ]}
            ]),
    {ok, _} = cowboy:start_clear(my_http_listener, 
                [{port, Port}], 
                #{env => #{dispatch => Dispatch}}).

