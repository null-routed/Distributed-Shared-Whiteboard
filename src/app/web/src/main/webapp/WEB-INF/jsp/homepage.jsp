<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
<%@ page contentType="text/html;charset=UTF-8"%>

<html>
<head>
    <title>Homepage</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/common.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/homepage.css">
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    <script type="text/javascript"> let pageContext = "${pageContext.request.contextPath}"; </script>
    <script type="module" src="${pageContext.request.contextPath}/assets/javascript/homepage.js"></script>
    <script type="module" src="${pageContext.request.contextPath}/assets/javascript/homepage-main.js" defer></script>
</head>
<body>
<div id="homepage-container">
    <jsp:include page="/WEB-INF/jsp/common/top_bar.jsp" />

    <% LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO != null) { %>

    <% if (request.getParameter("insertionFailed") != null || request.getParameter("deletionFailed") != null) { %>
        <script>        // make error modal visible if any error occurred
            document.addEventListener("DOMContentLoaded", function () {
                let error_modal = document.getElementById("error-display-modal");
                error_modal.style.display = "block";
            });
        </script>
    <% } %>

    <!-- MODAL FOR ERROR DISPLAYING -->
    <div id="error-display-modal" class="modal">
        <div class="modal-content">
            <span class="close">&times;</span>
            <h2>Error</h2>
            <% if (request.getParameter("insertionFailed") != null) { %>
                <p class="error-msg">Failed to add a new whiteboard. Try again or try in a few minutes.</p>
            <% } %>
            <% if (request.getParameter("deletionFailed") != null) { %>
                <p class="error-msg">Failed to delete the whiteboard. Try again or try in a few minutes.</p>
            <% } %>
        </div>
    </div>


    <!-- MAIN PAGE -->
    <div class="main-container">
        <br>
        <div>
            <h1>Your Whiteboards</h1>
        </div>
        <br>
        <div class="whiteboard-selection-container">
            <div class="whiteboard-search">
                <div class="search-form">
                    <form action="${pageContext.request.contextPath}/homepage" method="get">
                        <input type="text" id="search-input" name="search_input" placeholder="Type the whiteboard name ...">
                        <button type="submit" class="homepage-custom-button">Search</button>
                    </form>
                </div>
                <div class="button-swap-form">
                    <form action="${pageContext.request.contextPath}/homepage" method="GET">
                        <% String isSharedView = request.getAttribute("shared") != null ? request.getAttribute("shared").toString() : "";
                            if (isSharedView.equals("true")) { %>
                        <button type="submit" name="shared" class="homepage-custom-button" value="false">Other</button>
                        <% } else { %>
                        <button type="submit" name="shared" class="homepage-custom-button" value="true">Shared with me</button>
                        <% }
                        %>
                    </form>
                </div>

                <!-- INSERT NEW WHITEBOARD MODAL -->
                <div id="insert-whiteboard-modal" class="modal fade" tabindex="-1" aria-labelledby="modalLabel" aria-hidden="true">
                    <div class="modal-dialog">
                        <div class="modal-content">
                            <div class="modal-header">
                                <h5 class="modal-title" id="modalLabel">Create a New Whiteboard</h5>
                                <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                            </div>
                            <div class="modal-body">
                                <form id="addForm" action="${pageContext.request.contextPath}/insert_whiteboard" method="POST">
                                    <div class="mb-3">
                                        <label for="whiteboardName" class="form-label">Whiteboard Name:</label>
                                        <input type="text" class="form-control" id="whiteboardName" name="whiteboardName">
                                    </div>
                                    <div class="mb-3">
                                        <label for="whiteboardDescription" class="form-label">Description:</label>
                                        <textarea class="form-control" id="whiteboardDescription" name="whiteboardDescription"></textarea>
                                    </div>
                                    <div class="mb-3 form-check">
                                        <input type="checkbox" class="form-check-input" id="readOnly" name="readOnly">
                                        <label class="form-check-label" for="readOnly">Read-only</label>
                                    </div>
                                    <div class="modal-footer">
                                        <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>
                                        <button type="submit" class="btn btn-primary">Submit</button>
                                    </div>
                                </form>
                            </div>
                        </div>
                    </div>
                </div>
            </div>

            <hr class="divider">

            <div class="whiteboard-grid-container">
                <% List<MinimalWhiteboardDTO> whiteboards = (List<MinimalWhiteboardDTO>) request.getAttribute("whiteboards"); %>
                <div class="grid-item-add">
                    <img alt="Add a new whiteboard" id="add-button" src="${pageContext.request.contextPath}/assets/images/add_img_unclicked.svg">
                </div>
                <% for (MinimalWhiteboardDTO whiteboard : whiteboards) { %>
                    <div class="grid-item-whiteboard">
<%--                        <div class="whiteboard-name">--%>
<%--                            <p><%= whiteboard.getName() %></p>--%>
<%--                        </div>--%>
                        <img
                            class="whiteboard-snapshot"
                            alt="<%= whiteboard.getName() %>"
                            src="${pageContext.request.contextPath}/snapshot_manager?whiteboardID=<%= whiteboard.getId() %>&userID=<%= loggedUserDTO.getId() %>"
                            id="whiteboard_<%= whiteboard.getId() %>"
                            onclick="location.href = '${pageContext.request.contextPath}/whiteboard?whiteboardID=<%= whiteboard.getId() %>'"
                        >
                        <button type="button" class="delete-whiteboard-button" onclick="confirmDelete(<%= whiteboard.getId() %>)">&times;</button>
                    </div>
                <% } %>
            </div>
        </div>
        <% }
            assert loggedUserDTO != null;%>
    </div>
</div>

<form id="deleteWhiteboardForm" action="${pageContext.request.contextPath}/delete_whiteboard" method="POST" style="display: none;">
    <input type="hidden" id="whiteboardIdToDelete" name="whiteboardIdToDelete">
</form>
<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
</body>
</html>
