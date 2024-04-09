    <%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
    <%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
    <%@ page import="java.util.List" %>
    <%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
    <%@ page contentType="text/html;charset=UTF-8"%>

    <html>
    <head>
        <title>Homepage</title>
        <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/homepage.css">
    </head>
    <body>
    <div id="homepage-container">
        <jsp:include page="/WEB-INF/jsp/common/top_bar.jsp" />

        <% LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
            if (loggedUserDTO != null) { %>

        <% if (request.getParameter("insertionFailed") != null) { %>
            <script>
                alert("Failed to add a new whiteboard. Try again or try in a few minutes.");
            </script>
        <% }
            if (request.getParameter("deletionFailed") != null) { %>
            <script>
                alert("Failed to delete the whiteboard. Try again or try in a few minutes.");
            </script>
        <% } %>

        <br>
        <h1>Your Whiteboards</h1>
        <br>
        <div id="whiteboard-selection-container">
            <div id="whiteboard-selection">
                <div class="whiteboard-search">
                    <form class="search-form" action="${pageContext.request.contextPath}/homepage" method="get">
                        <label class="search-input-label" for="search-input">Search Whiteboards:</label>
                        <input type="text" id="search-input" name="search_input" placeholder="Type the whiteboard name ...">
                        <button type="submit" class="homepage-button">Search</button>
                    </form>
                </div>

                <div id="whiteboard-type">
                    <button type="button" id="addButton" class="nav-button">New</button>

                    <div id="myModal" class="modal">
                        <div class="modal-content">
                            <span class="close">&times;</span>
                            <h2>Create a new Whiteboard</h2>
                                <form id="addForm" action="${pageContext.request.contextPath}/insert_whiteboard" method="POST">
                                <label for="whiteboardName">Whiteboard Name:</label>
                                <input type="text" id="whiteboardName" name="whiteboardName">

                                <label for="whiteboardDescription">Description:</label>
                                <textarea id="whiteboardDescription" name="whiteboardDescription"></textarea>

                                <label for="readOnly">Read-only:</label>
                                <input type="checkbox" id="readOnly" name="readOnly">

                                <button type="submit">Submit</button>
                            </form>
                        </div>
                    </div>
                    <form class="search-form" action="${pageContext.request.contextPath}/homepage" method="GET">
                        <% String isSharedView = request.getAttribute("shared") != null ? request.getAttribute("shared").toString() : "";
                            if (isSharedView.equals("true")) { %>
                                <button type="submit" name="shared" class="nav-button" value="false">Other</button>
                            <% } else { %>
                                <button type="submit" name="shared" class="nav-button" value="true">Shared with me</button>
                            <% }
                        %>
                    </form>
                </div>
            </div>
            <hr class="hr-style">
            <div id="whiteboards">
                <% List<MinimalWhiteboardDTO> whiteboards = (List<MinimalWhiteboardDTO>) request.getAttribute("whiteboards");
                    if (whiteboards != null) {
                        int counter = 0;
                        for (MinimalWhiteboardDTO whiteboard : whiteboards) {
                            if (counter == 0) { %>
                <div class="whiteboard-row-buttons">
                    <% }
                        counter++; %>
                    <div class="whiteboard-button-container">
                        <button type="button" id="whiteboard_<%= whiteboard.getId() %>" class="selected-whiteboards"
                                onclick="location.href = '${pageContext.request.contextPath}/whiteboard?whiteboardID=<%= whiteboard.getId() %>'">
                            <%= whiteboard.getName() %>
                        </button>
                        <button type="button" onclick="confirmDelete(<%= whiteboard.getId() %>)" class="delete-button">&times;</button>
                    </div>
                    <% if (counter == 3 || whiteboards.indexOf(whiteboard) == whiteboards.size() - 1) { %>
                </div>
                <% }
                }
                } %>
            </div>
        </div>
        <% } %>
    </div>

    Form for deleting whiteboard
    <form id="deleteWhiteboardForm" action="${pageContext.request.contextPath}/delete_whiteboard" method="POST" style="display: none;">
        <input type="hidden" id="whiteboardIdToDelete" name="whiteboardIdToDelete">
    </form>

    <script src="${pageContext.request.contextPath}/assets/javascript/homepage.js"></script>

    </body>
    </html>
