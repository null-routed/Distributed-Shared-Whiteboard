REM Check for no arguments
if "%~1"=="" (
    echo Usage: %0 node_name
    exit /b 1
)

set NODE_NAME=%1
set COOKIE=shared_whiteboard_app

echo Compiling project...


echo Starting node %NODE_NAME%...
rebar3 shell --name %NODE_NAME% --setcookie %COOKIE% --config ./config/sys.config
