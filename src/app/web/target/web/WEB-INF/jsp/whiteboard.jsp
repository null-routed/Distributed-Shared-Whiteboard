<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
<%@ page contentType="text/html;charset=UTF-8"%>
<html>
<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null)
        return;

    // Getting whiteboard data
    MinimalWhiteboardDTO whiteboardData = (MinimalWhiteboardDTO) request.getAttribute("whiteboardData");

    // Check if user can actually access this whiteboard
    List<String> whiteboardParticipants = (List<String>) request.getAttribute("whiteboardParticipants");
    if (!whiteboardParticipants.contains(loggedUserDTO.getUsername())) {
%>
<script>
    alert("You are not allowed to perform any operation on this whiteboard.");
    location.href = "${pageContext.request.contextPath}/homepage";
</script>
<%
    }
%>
<head>
    <title><%= whiteboardData.getName() %></title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/common.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/whiteboard.css">
    <script type="text/javascript" src="${pageContext.request.contextPath}/assets/javascript/whiteboard_modal.js"></script>
</head>

<body>
    <div class="whiteboard-nav">
        <div id="left-container">
            <button class="custom-generic-button" onclick="location.href = '${pageContext.request.contextPath}/homepage'">Back</button>
        </div>
        <h1><%=whiteboardData.getName()%></h1>
        <div id="right-container">
            <button class="custom-generic-button" id="participants-button">Participants</button>
        </div>
        <div class="modal" id="participants-modal">
            <div class="modal-content">
                <span class="close">&times;</span>
                <h2>Participants to <%=whiteboardData.getName()%></h2>
                <div class="participants-list">
                <%
                    for(int i = 0; i < whiteboardParticipants.size(); i++){
                %>
                    <div class="whiteboard-participant" id="participant-<%=i%>"> &bullet; <%=whiteboardParticipants.get(i)%></div>
                <%
                    }
                %>
                </div>
            </div>

        </div>

    </div>
    <div class="whiteboard-container">
        <div id="canvas-container">
            <canvas id="whiteboard"></canvas>
        </div>
        <script>
            // wait for the content of the window element
            // to load, then performs the operations.
            // This is considered best practice.
            window.addEventListener('load', ()=>{
                resize(); // Resizes the canvas once the window loads
                document.addEventListener('mousedown', startPainting);
                document.addEventListener('mouseup', stopPainting);
                document.addEventListener('mousemove', sketch);
                window.addEventListener('resize', resize);
            });

            const canvas = document.querySelector('#whiteboard');

            // Context for the canvas for 2 dimensional operations
            const ctx = canvas.getContext('2d');

            // Resizes the canvas to the available size of the window.
            function resize(){
                ctx.canvas.width = window.innerWidth;
                ctx.canvas.height = window.innerHeight;
            }

            // Stores the initial position of the cursor
            let coords = {x:0 , y:0};

            // This is the flag that we are going to use to
            // trigger drawing
            let paint = false;

            // Updates the coordinates of the cursor when
            // an event e is triggered to the coordinates where
            // the said event is triggered.
            function getPosition(event){
                coords.x = event.clientX; // - canvas.offsetLeft;
                coords.y = event.clientY; //- canvas.offsetTop;
            }

            // The following functions toggle the flag to start
            // and stop drawing
            function startPainting(event){
                paint = true;
                getPosition(event);
            }
            function stopPainting(){
                paint = false;
            }

            function sketch(event){
                if (!paint) return;
                ctx.beginPath();

                ctx.lineWidth = 5;

                // Sets the end of the lines drawn
                // to a round shape.
                ctx.lineCap = 'round';

                ctx.strokeStyle = 'black';

                // The cursor to start drawing
                // moves to this coordinate
                ctx.moveTo(coords.x, coords.y);

                // The position of the cursor
                // gets updated as we move the
                // mouse around.
                getPosition(event);

                // A line is traced from start
                // coordinate to this coordinate
                ctx.lineTo(coords.x , coords.y);

                // Draws the line.
                ctx.stroke();
            }
        </script>       <!-- drawing code test -->
        <div class="whiteboard-sidebar">
            <div id="tools-container">
                <div id="tools-name">Tools</div>
                <div id="sidebar-tools">
                    <button class="custom-generic-button-with-icon" id="pen-button">
                        <img alt="pen-icon" src="${pageContext.request.contextPath}/assets/images/pen.svg">
                    </button>
                    <button class="custom-generic-button-with-icon" id="rubber-button">
                        <img alt="eraser-icon" src="${pageContext.request.contextPath}/assets/images/eraser.svg">
                    </button>
                </div>
            </div>
        </div>
    </div>
</body>
</html>
