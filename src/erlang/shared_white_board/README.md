# Overview
The Erlang nodes are responsible for handling the core logic of the application. More specifically, they are designed to:
1. **Handle and log interactions**: which includes receiving, logging and processing new strokes, strokes deletions, undo and redo actions incoming from clients, as well as forwarding these interactions to other nodes for consistency.
2. **Update Cursor Positions**: Nodes receive cursor position updates from connected clients and forward them to the other connected users.
3. **Enforce access control**: Nodes perform permission checks and enforce access control actions when a user is removed / added to a whiteboard.

## How to run
In order to compile and run the Erlang node, it's mandatory to have `rebar3` installed and in PATH. <br>
After ensuring that, place yourself in `Distributed-Shared-Whiteboard\src\erlang\shared_white_board` and run `./start_node.sh <node_name>` to start a node, for example `./start_node.sh node1@localhost`. 
