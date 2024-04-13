<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
<%@ page contentType="text/html;charset=UTF-8"%>
<!DOCTYPE html>
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
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/whiteboard.css">
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <link href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap-icons/1.9.1/font/bootstrap-icons.min.css" rel="stylesheet">
</head>
<body>
    <div class="toolbar">
        <!-- Tools on the left -->
        <div>
            <button class="btn btn-light" id="undo-button" title="Undo"><i class="bi bi-arrow-counterclockwise"></i></button>
            <button class="btn btn-light" id="redo-button" title="Redo"><i class="bi bi-arrow-clockwise"></i></button>
            <button class="btn btn-light active" id="pen-button" title="Pen"><i class="bi bi-pencil-fill"></i></button>
            <button class="btn btn-light" id="rubber-button" title="Rubber"><i class="bi bi-eraser-fill"></i></button>
        </div>

        <!-- Additional actions on the right -->
        <div>
            <button class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#participantsModal" title="Participants">Participants</button>
            <button class="btn btn-primary" onclick="window.location='/'" title="Home">Home</button>
            <% if (isLoggedUserOwner) { %>
                <button class="btn btn-success" data-bs-toggle="modal" data-bs-target="#shareModal" title="Share">Share</button>
            <% } %>
    </div>
    <!-- Whiteboard Canvas -->
    <canvas id="whiteboard"></canvas>

    <!-- Modal for showing participants -->
    <div class="modal fade" id="participantsModal" tabindex="-1" aria-labelledby="participantsModalLabel" aria-hidden="true">
        <div class="modal-dialog">
            <div class="modal-content">
                <div class="modal-header">
                    <h5 class="modal-title" id="participantsModalLabel">Current Participants</h5>
                    <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                </div>
                <div class="modal-body">
                    <% for (String participant : participantsOnWhiteboardOpen) { %>
                        <p><%= participant %></p>
                    <% } %>
                </div>
            </div>
        </div>
    </div>

    <!-- Modal for sharing the whiteboard -->
    <div class="modal fade" id="shareModal" tabindex="-1" aria-labelledby="shareModalLabel" aria-hidden="true">
        <div class="modal-dialog">
            <div class="modal-content">
                <div class="modal-header">
                    <h5 class="modal-title" id="shareModalLabel">Share Whiteboard</h5>
                    <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                </div>
                <div class="modal-body">
                    <div class="mb-3">
                        <label for="username" class="form-label">Username to share with:</label>
                        <input type="text" class="form-control" id="username">
                    </div>
                    <button id="share-button-modal" class="btn btn-primary">Share</button>
                </div>
            </div>
        </div>
    </div>

    <!-- Toast Container -->
    <div id="toast-container" class="position-fixed bottom-0 end-0 p-3" style="z-index: 11; min-height: 100px;"></div>

    <!-- Scripts -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/fabric.js/5.3.1/fabric.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jquery/3.6.0/jquery.min.js" defer></script>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js" defer></script>
    <script type="module" src="${pageContext.request.contextPath}/assets/javascript/whiteboard-main.js" defer></script>
    <input type="hidden" id="username" value="<%= loggedUserDTO.getUsername() %>">
</body>