#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: $0 node_name"
    exit 1
fi

NODE_NAME=$1
COOKIE="shared_whiteboard_app"  

echo "Compiling project..."
rebar3 get-deps
rebar3 compile

echo "Starting node ${NODE_NAME}..."
rebar3 shell --name ${NODE_NAME} --setcookie ${COOKIE} --config ./config/sys.config
