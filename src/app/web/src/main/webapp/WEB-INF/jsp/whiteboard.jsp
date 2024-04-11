<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
<%@ page import="java.util.Objects" %>
<%@ page contentType="text/html;charset=UTF-8"%>
<html>
<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null)
        return;

    MinimalWhiteboardDTO whiteboardData = (MinimalWhiteboardDTO) session.getAttribute("whiteboardData");

    boolean isLoggedUserOwner = whiteboardData.getOwner().equals(loggedUserDTO.getUsername());

    List<String> participantsOnWhiteboardOpen = (List<String>) session.getAttribute("whiteboardParticipants");
    if (!participantsOnWhiteboardOpen.contains(loggedUserDTO.getUsername())) {
%>
        <script>
            alert("You are not allowed to perform any operation on this whiteboard.");
            location.href = "${pageContext.request.contextPath}/homepage";
        </script>
<% } %>

<head>
    <title><%= whiteboardData.getName() %></title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/common.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/whiteboard.css">
    <script type="text/javascript" src="${pageContext.request.contextPath}/assets/javascript/whiteboard.js"></script>
    <script type="text/javascript" src="${pageContext.request.contextPath}/assets/javascript/whiteboard_sketching.js"></script>
    <script type="text/javascript" src="${pageContext.request.contextPath}/assets/javascript/whiteboardWebSocket.js"></script>
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
                    <% for(int i = 0; i < participantsOnWhiteboardOpen.size(); i++) { %>
                    <div class="whiteboard-participant <%= i % 2 == 0 ? "even-participant" : "odd-participant" %>" id="<%=participantsOnWhiteboardOpen.get(i)%>">
                        <div class="participant-username"> &bullet; <%= participantsOnWhiteboardOpen.get(i) %></div>
                        <% if (isLoggedUserOwner) { %>
                        <div class="participant-remove-button">
                            <button
                                    class="custom-generic-button"
                                    onclick="removeParticipantAJAX('<%= participantsOnWhiteboardOpen.get(i) %>')"
                                    <% if (loggedUserDTO.getUsername().equals(participantsOnWhiteboardOpen.get(i))) {%>
                                        disabled
                                    <% } %> >
                                Remove
                            </button>
                        </div>
                        <script>
                            function removeParticipantAJAX (username) {
                                let whiteboardID = document.getElementById("whiteboardID").value;

                                console.log("params: " + username + ", " + whiteboardID);

                                let xhttp = new XMLHttpRequest();
                                xhttp.onreadystatechange = function () {
                                    if (this.readyState === 4 && this.status === 200) {
                                        console.log("AJAX response: " + this.responseText)

                                        const jsonResponse = JSON.parse(this.responseText);

                                        if (jsonResponse.success === true) {      // removing participant from the list
                                            let removedParticipantDiv = document.getElementById(username);
                                            removedParticipantDiv.remove();
                                        }

                                        // displaying error or success message
                                        let participantsListDiv = document.getElementsByClassName("participants-list")[0];
                                        let outcomeMessageDiv = document.createElement("div");
                                        outcomeMessageDiv.style.marginTop = "10px";
                                        if (jsonResponse.success)
                                            outcomeMessageDiv.setAttribute("class", "success-msg");
                                        else
                                            outcomeMessageDiv.setAttribute("class", "error-msg");
                                        outcomeMessageDiv.textContent = jsonResponse.message;
                                        participantsListDiv.append(outcomeMessageDiv);

                                        // if he's connected to the whiteboard make him go back to homepage with message
                                        // + send notification ?
                                    }
                                }

                                xhttp.open("POST", "${pageContext.request.contextPath}/remove_participant", true);
                                xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

                                xhttp.send("whiteboardID=" + whiteboardID + "&" + "username=" + username);
                            }
                        </script>
                        <% } %>
                    </div>
                <% } %>
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
                document.addEventListener("DOMContentLoaded", function () {
                    document.getElementById("share-button-modal").addEventListener("click", function (event) {
                        event.preventDefault();

                        let whiteboardID = document.getElementById("whiteboardID").value;
                        let username = document.getElementById("username").value;

                        let xhttp = new XMLHttpRequest();
                        xhttp.onreadystatechange = function () {
                            if (this.readyState === 4 && this.status === 200) {
                                console.log("AJAX response: " + this.responseText)

                                const jsonResponse = JSON.parse(this.responseText);

                                if (jsonResponse.success === true) {        // adding new participant to list
                                    let participantsListDiv = document.getElementsByClassName("participants-list")[0];

                                    let newlyInsertedParticipantDiv = document.createElement("div");
                                    newlyInsertedParticipantDiv.setAttribute("class", "whiteboard-participant");
                                    newlyInsertedParticipantDiv.setAttribute("id", username);
                                    newlyInsertedParticipantDiv.textContent = "• " + username;

                                    participantsListDiv.append(newlyInsertedParticipantDiv);
                                }

                                // displaying error or success message
                                let usernameTextBox = document.getElementsByClassName("modal-content")[1];
                                let outcomeMessageDiv = document.createElement("div");
                                outcomeMessageDiv.style.marginTop = "10px";
                                if (jsonResponse.success)
                                    outcomeMessageDiv.setAttribute("class", "success-msg");
                                else
                                    outcomeMessageDiv.setAttribute("class", "error-msg");
                                outcomeMessageDiv.textContent = jsonResponse.message;
                                usernameTextBox.append(outcomeMessageDiv);
                            }
                        }

                        xhttp.open("POST", "${pageContext.request.contextPath}/share_whiteboard", true);
                        xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

                        xhttp.send("whiteboardID=" + whiteboardID + "&" + "username=" + username);
                    });
                });
            </script>
        </div>
    </div>
</div>
</body>
</html>
