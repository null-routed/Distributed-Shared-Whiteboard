<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
<%@ page contentType="text/html;charset=UTF-8"%>
<!DOCTYPE html>
<html>
<%
    MinimalWhiteboardDTO whiteboardData = (MinimalWhiteboardDTO) session.getAttribute("whiteboardData");
    String selfUsername = request.getAttribute("selfUsername").toString();
    boolean isLoggedUserOwner = whiteboardData.getOwner().equals(selfUsername);
    boolean hasWritePermission = isLoggedUserOwner || !whiteboardData.isReadOnly();
    List<String> participantsOnWhiteboardOpen = (List<String>) session.getAttribute("whiteboardParticipants");
    if (!participantsOnWhiteboardOpen.contains(selfUsername)) {
%>
<script>
    alert("You are not allowed to perform any operation on this whiteboard.");
    location.href = "${pageContext.request.contextPath}/homepage";
</script>
<% } %>
<head>
    <title><%= whiteboardData.getName() %></title>
    <!-- Custom CSS -->
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/whiteboard.css">
    <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/assets/images/favicon.ico">
    <!-- Bootstrap CSS -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <link href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap-icons/1.9.1/font/bootstrap-icons.min.css" rel="stylesheet">
    <!-- Custom JavaScript -->
    <script> let contextPath = "${pageContext.request.contextPath}"; </script>
</head>
<body>
    <div class="toolbar">
        <!-- Tools on the left -->
        <div>
            <button class="btn btn-light" id="undo-button" title="Undo" <%= !hasWritePermission ? "disabled" : "" %>><i class="bi bi-arrow-counterclockwise"></i></button>
            <button class="btn btn-light" id="redo-button" title="Redo" <%= !hasWritePermission ? "disabled" : "" %>><i class="bi bi-arrow-clockwise"></i></button>
            <button class="btn btn-light active" id="pen-button" title="Pen" <%= !hasWritePermission ? "disabled" : "" %>><i class="bi bi-pencil-fill"></i></button>
            <button class="btn btn-light" id="rubber-button" title="Rubber" <%= !hasWritePermission ? "disabled" : "" %>><i class="bi bi-eraser-fill"></i></button>
            <button class="btn btn-light" id="color-palette-button" data-bs-toggle="popover" title="Color Palette" <%= !hasWritePermission ? "disabled" : "" %>><i class="bi bi-palette"></i></button>
        </div>

        <!-- Additional actions on the right -->
        <div>
            <button class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#participantsModal" title="Participants">Participants</button>
            <button class="btn btn-primary" onclick="location.href = '${pageContext.request.contextPath}/homepage'"
                    title="Home">Home</button>
            <% if (isLoggedUserOwner) { %>
                <button class="btn btn-success" data-bs-toggle="modal" data-bs-target="#shareModal" title="Share">Share</button>
            <% } %>
        </div>
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
                <div class="modal-body" id="participants">
                    <% for (String participant : participantsOnWhiteboardOpen) { %>
                        <div class="d-flex justify-content-between align-items-center mb-2" id="<%= participant %>-container">
                            <p class="mb-0"><%= participant %></p>
                            <% if (isLoggedUserOwner && !participant.equals(selfUsername)) { %>
                            <button type="button" class="btn btn-danger btn-sm remove-participant-btn" data-participant="<%= participant %>">
                                <i class="bi bi-x"></i>
                            </button>
                            <% } else if (!isLoggedUserOwner && participant.equals(selfUsername)) { %>
                            <button type="button" class="btn btn-danger btn-sm remove-participant-btn" data-participant="<%= participant %>">
                                <i class="bi bi-x"></i>
                            </button>
                            <% } %>
                        </div>
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
                        <label for="new-participant-username" class="form-label">Username to share with:</label>
                        <input type="text" class="form-control" id="new-participant-username">
                    </div>
                    <button id="share-button-modal" class="btn btn-primary">Share</button>
                </div>
            </div>
        </div>
    </div>

    <!-- Toast Container -->
    <div id="toast-container" class="position-fixed bottom-0 end-0 p-3"></div>

    <!-- Scripts -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/fabric.js/5.3.1/fabric.min.js"></script>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js" defer></script>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js" defer></script>
    <script type="module" src="${pageContext.request.contextPath}/assets/javascript/whiteboard-main.js" defer></script>
    <input type="hidden" id="self-username" value="<%= selfUsername %>">
    <input type="hidden" id="writePermission" value="<%= hasWritePermission %>">
    <input type="hidden" id="whiteboard-name" value="<%= whiteboardData.getName() %>">
    <input type="hidden" id="whiteboard-id" value="<%= whiteboardData.getId() %>">
</body>
</html>