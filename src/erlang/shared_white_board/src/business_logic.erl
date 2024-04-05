-module(business_logic).

-export([notify_user_connection/3, notify_user_disconnection/3, check_permissions/2]).

% Broadcast a message to all nodes except the origin and current node.
broadcast_message_to_nodes(Func, Params, OriginNode) ->
    case OriginNode == node() of
        true ->
            ok;
        false ->
            Nodes = [Node || Node <- nodes(), Node /= node(), Node /= OriginNode],
            lists:foreach(fun(Node) ->
                              rpc:call(Node, business_logic, Func, Params)
                          end, Nodes)
    end.

notify_user_disconnection(WhiteboardId, Username, OriginNode) ->
    mnesia_queries:remove_user_connection(WhiteboardId, Username),
    notify_existing_users(WhiteboardId),
    broadcast_message_to_nodes(notify_user_disconnection, [WhiteboardId, Username], OriginNode).

% Handle permissions for a new user connection
check_permissions(WhiteboardId, Username) ->
    PermissionsResult = mnesia_queries:get_permissions(WhiteboardId, Username),
    case PermissionsResult of 
        {ok, Permission} -> 
            {ok, #{username => Username, whiteboardId => WhiteboardId, permission => Permission}, <<>>};
        {error, _} -> 
            {error, {}, <<"No permission to access whiteboard">>}
    end.

% Notify about a user connection
notify_user_connection(WhiteboardId, Username, OriginNode) ->
    ExistingConnection = mnesia_queries:get_user_connection(WhiteboardId, Username),
    io:format("Existing connection: ~p~n", [ExistingConnection]),
    handle_existing_connection(ExistingConnection),
    WebSocketPid = case node() == OriginNode of
        true -> self();
        false -> undefined
    end,
    mnesia_queries:update_or_add_user_connection(WhiteboardId, Username, WebSocketPid),
    notify_existing_users(WhiteboardId),
    broadcast_message_to_nodes(notify_user_connection, [WhiteboardId, Username], OriginNode).

% Handle an existing user connection
handle_existing_connection({ok, OldPid}) when is_pid(OldPid) ->
    io:format("Closing old connection for user ~p~n", [OldPid]),
    OldPid ! {close, <<"Connected from another location">>},
    ok;
handle_existing_connection(_) ->
    ok.

notify_existing_users(WhiteboardId) ->
    ExistingUsers = mnesia_queries:get_connected_users(WhiteboardId),
    UsernamesList = [Username || {Username, _Pid} <- ExistingUsers],
    EncodedMessage = encode_user_list_message(UsernamesList),
    broadcast_message_to_users(EncodedMessage, ExistingUsers).

% Encode user list into a message
encode_user_list_message(UsernamesList) ->
    UserListMessage = #{action => <<"updateUserList">>, users => UsernamesList},
    jsx:encode(UserListMessage).

% Broadcast message to all users, excluding sender
broadcast_message_to_users(Message, Users) ->
    lists:foreach(fun({_, Pid}) ->
                      Pid ! {send, Message}
                  end, Users).


