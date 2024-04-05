<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
<%@ page contentType="text/html;charset=UTF-8"%>
<html>
<head>
    <title>Title</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/common.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/whiteboard.css">
</head>
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
<body>
    <div class="whiteboard-nav">
        <button class="custom-generic-button">back</button>
        <h1>whiteboard name</h1>
        <button class="custom-generic-button">participants</button>
    </div>
    <div class="whiteboard-container">
        <div>canvas</div>
        <div class="whiteboard-sidebar">
            <div>Tools</div>
            <div id="sidebar-tools">
                <button id="pen-button"></button>       <!-- replace with clickable images? -->
                <button id="rubber-button"></button>
            </div>
        </div>
    </div>
</body>
</html>
