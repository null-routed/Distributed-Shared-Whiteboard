-module(mnesia_queries).

-export[get_permissions/2].

-record(whiteboard_access, {whiteboard_id, username, permission}).

get_permissions(WhiteboardId, Username) ->
    case mnesia:transaction(fun() ->
            mnesia:match_object(#whiteboard_access{whiteboard_id = WhiteboardId, username = Username, permission = '_'})
        end) of
        {atomic, [Record]} ->
            % A match was found, extract the permission
            {ok, Record#whiteboard_access.permission};
        {atomic, []} ->
            % No match was found
            {error, no_permission};
        {aborted, Reason} ->
            % The query was aborted for some reason
            {error, Reason}
    end.

   
