![Mnesia](https://img.shields.io/badge/Mnesia-ED7B06?style=flat&logo=erlang&logoColor=white)
![Erlang](https://img.shields.io/badge/Erlang-A90533?style=flat&logo=erlang&logoColor=white)
![MySQL](https://img.shields.io/badge/MySQL-4479A1?style=flat&logo=mysql&logoColor=white)
![Java](https://img.shields.io/badge/Java-ED8B00?style=flat&logo=openjdk&logoColor=white)
![JavaScript](https://img.shields.io/badge/JavaScript-F7DF1E?style=flat&logo=javascript&logoColor=black)
![Bootstrap](https://img.shields.io/badge/Bootstrap-7952B3?style=flat&logo=bootstrap&logoColor=white)

# Distributed Shared Whiteboards

## Overview
Shared Whiteboards is a distributed application designed to allow users to collaborate on shared whiteboards. The main features we developed are:
- **Whiteboard Management**: Allows users to create, edit, and delete whiteboards.
- **Real-Time Collaboration**: Enables multiple users to draw, erase, and view updates on whiteboards in real time.
- **Action Broadcasting**: Uses Erlang processes to broadcast actions like adding or removing strokes across nodes, ensuring data consistency.
- **Undo/Redo Actions**: Supports undoing and redoing actions on the whiteboard via a managed stack structure.
- **WebSocket Communication**: Facilitates real-time communication between the client and server using WebSockets.
- **Load Balancing**: Implements custom load balancing strategies with NGINX.

### Application Components
![dsmt](https://github.com/null-routed/Distributed-Shared-Whiteboard/assets/55241343/a9935406-9e6d-43eb-bdcf-f9b3dcb9eeaa)

- **NGINX**: Employed as a proxy server to ensure proper load balancing.
- **Erlang**: Erlang servers manage whiteboard activities through WebSocket communication. These servers handle everything from user authentication using JWTs for secure sessions to the real-time updating of whiteboard content.
- **Glassfish**: Serves as the Jakarta EE Application Server.
- **MySQL**: Utilized for storing persistent data such as user credentials and whiteboard metadata.
- **Mnesia Database**: Integrated within Erlang nodes to store whiteboard-specific data like access permissions, user connections, and action logs.

### Short demo
https://github.com/null-routed/Distributed-Shared-Whiteboard/assets/57362927/abc52d0d-c873-4831-98df-819aa2478a09

<br>

## Installing, configuring and running
You can refer to [this](https://github.com/null-routed/Distributed-Shared-Whiteboard/tree/main/src) README file for all the necessary instructions.

<br>

## Contributors
* [Marco Imbelli Cai](https://github.com/marcoimbee)
* [Niccolò Mulè](https://github.com/null-routed)
* [Nicolò Picchi](https://github.com/NicoUniPi)
