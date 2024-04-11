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

    MinimalWhiteboardDTO whiteboardData = (MinimalWhiteboardDTO) session.getAttribute("whiteboardData");

    boolean isLoggedUserOwner = whiteboardData.getOwner().equals(loggedUserDTO.getUsername());

    List<String> whiteboardParticipants = (List<String>) session.getAttribute("whiteboardParticipants");
    if (!whiteboardParticipants.contains(loggedUserDTO.getUsername())) {
%>
<script>
    alert("You are not allowed to perform any operation on this whiteboard.");
    location.href = "${pageContext.request.contextPath}/homepage";
</script>

<%
    }
%>
<%--    boolean redirectedAfterAddOperation = request.getAttribute("redirectAfterAddOperation") != null;--%>
<%--    if (redirectedAfterAddOperation && request.getAttribute("newlyInsertedParticipant") != null) {--%>
<%--        whiteboardParticipants.add((String) request.getAttribute("newlyInsertedParticipant"));--%>
<%--        request.getSession().setAttribute("whiteboardParticipants", whiteboardParticipants);--%>
<%--    }--%>
<%--%>--%>

<%--<%--%>
<%--    if (request.getAttribute("newlyInsertedUsername") != null) {--%>
<%--%>--%>
<%--    <h1><%=request.getAttribute("newlyInsertedUsername")%></h1>--%>
<%--<% } %>--%>

<head>
    <title><%= whiteboardData.getName() %></title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/common.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/whiteboard.css">
    <script type="text/javascript" src="${pageContext.request.contextPath}/assets/javascript/whiteboard.js"></script>
    <script type="text/javascript" src="${pageContext.request.contextPath}/assets/javascript/whiteboard_sketching.js"></script>
</head>

<body>
<div class="whiteboard-nav">
    <div id="left-container">
        <button class="custom-generic-button" onclick="location.href = '${pageContext.request.contextPath}/homepage'">Back</button>
    </div>
    <h1><%= whiteboardData.getName() %></h1>
    <div id="right-container">
        <button class="custom-generic-button" id="participants-button">Participants</button>
    </div>
    <div class="modal" id="participants-modal">
        <div class="modal-content">
            <span class="close">&times;</span>
            <h2>Participants to <%= whiteboardData.getName() %></h2>
            <div class="participants-list">
                <%
                    for(int i = 0; i < whiteboardParticipants.size(); i++){
                %>
                <div class="whiteboard-participant" id="participant-<%= i %>"> &bullet; <%= whiteboardParticipants.get(i) %></div>
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

        <!-- Conditionally show the share button if the user is the owner -->
        <% if (isLoggedUserOwner) { %>
        <!-- Button to open modal for sharing -->
        <button class="custom-generic-button" id="share-button">Share</button>
        <% } %>

        <!-- Modal for sharing -->
<%--        <script>--%>
<%--            let redirectedAfterAddOperation = <%= redirectedAfterAddOperation %>;--%>
<%--            document.addEventListener("DOMContentLoaded", function() {--%>
<%--                if (redirectedAfterAddOperation)--%>
<%--                    document.getElementById("share-modal").style.display = "block";--%>
<%--            });--%>
<%--        </script>--%>
        <div class="modal" id="share-modal">
            <div class="modal-content">
                <span class="close">&times;</span>
                <h2>Add a participant to <%= whiteboardData.getName() %></h2>
                <input type="hidden" id="whiteboardID" name="whiteboardID" value="<%= whiteboardData.getId() %>">
                <label for="username"></label>
                <input type="text" id="username" name="username" placeholder="Enter a username" required>
                <button class="custom-generic-button" id="share-button-modal" type="button">Add participant</button>
            </div>
            <script>
                let newlyInsertedUsername = undefined;
                let redirectedAfterAddOperation = undefined;

                document.addEventListener("DOMContentLoaded", function () {
                    document.getElementById("share-button-modal").addEventListener("click", function (event) {
                        event.preventDefault();

                        let whiteboardID = document.getElementById("whiteboardID").value;
                        let username = document.getElementById("username").value;

                        let xhttp = new XMLHttpRequest();
                        xhttp.onreadystatechange = function () {
                            if (this.readyState === 4 && this.status === 200) {
                                console.log("Response: " + this.responseText);
                                <%
                                    System.out.println(request.getAttribute("newlyInsertedParticipant"));
                                    whiteboardParticipants.add((String) request.getAttribute("newlyInsertedParticipant"));
                                    request.getSession().setAttribute("whiteboardParticipants", whiteboardParticipants);
                                %>
                            }
                        }

                        xhttp.open("POST", "${pageContext.request.contextPath}/share_whiteboard", true);
                        xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

                        xhttp.send("whiteboardID=" + whiteboardID + "&" + "username=" + username);
                    });
                });

                <%--<%--%>
                <%--    }--%>

                <%--    boolean redirectedAfterAddOperation = request.getAttribute("redirectAfterAddOperation") != null;--%>
                <%--    if (redirectedAfterAddOperation && request.getAttribute("newlyInsertedParticipant") != null) {--%>
                //         whiteboardParticipants.add((String) request.getAttribute("newlyInsertedParticipant"));
                //         request.getSession().setAttribute("whiteboardParticipants", whiteboardParticipants);
                <%--    }--%>
                <%--%>--%>
            </script>
        </div>
    </div>
</div>
</body>
</html>
