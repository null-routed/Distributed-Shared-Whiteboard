<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
<%@ page contentType="text/html;charset=UTF-8"%>

<html>
<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null) {
        return;
    }

    String isSharedView = "";

    @SuppressWarnings("unchecked")
    List<MinimalWhiteboardDTO> whiteboards = (List<MinimalWhiteboardDTO>) request.getAttribute("whiteboards");

    if(request.getAttribute("shared") != null){
        isSharedView = request.getAttribute("shared").toString();
        System.out.println(isSharedView);
    }
%>

<head>
    <title>Homepage</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/homepage.css">
</head>
<body>
<div id="homepage-container">
    <jsp:include page="/WEB-INF/jsp/common/top_bar.jsp" />

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
                <button type="button" id="addButton" onclick="" class="nav-button">New</button>
                <!-- The modal -->
                <div id="myModal" class="modal">
                    <!-- Modal content -->
                    <div class="modal-content">
                        <span class="close">&times;</span>
                        <h2>Create a new Whiteboard</h2>
                        <form id="addForm" action="${pageContext.request.contextPath}/homepage" method="POST">
                            <!-- Your form fields here -->
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
                <script>
                    // Get the modal
                    let modal = document.getElementById("myModal");

                    // Get the button that opens the modal
                    let btn = document.getElementById("addButton");

                    // Get the <span> element that closes the modal
                    let span = document.getElementsByClassName("close")[0];

                    // When the user clicks on the button, open the modal
                    btn.onclick = function() {
                        modal.style.display = "block";
                    }

                    // When the user clicks on <span> (x), close the modal
                    span.onclick = function() {
                        modal.style.display = "none";
                    }

                    // When the user clicks anywhere outside the modal, close it
                    window.onclick = function(event) {
                        if (event.target === modal) {
                            modal.style.display = "none";
                        }
                    }
                </script>
                <form class="search-form" action="${pageContext.request.contextPath}/homepage" method="GET">
                    <%
                        if(isSharedView.equals("true")){
                    %>
                    <button type="submit" name="shared" class="nav-button" value="false">
                        Other
                    </button>
                    <%
                    }
                    else{
                    %>
                    <button type="submit" name="shared" class="nav-button" value="true">
                        Shared
                    </button>
                    <%
                        }
                    %>
                </form>
            </div>
        </div>
        <hr class="hr-style">
        <div id="whiteboards">
            <script>
                const whiteboardsDiv = document.getElementById("whiteboards");
                whiteboardsDiv.innerHTML = "";
            </script>
            <%
                int counter = 0;
                if (whiteboards != null) {
                    for (MinimalWhiteboardDTO whiteboard : whiteboards) {
                        if(counter == 0){
            %>
            <div class="whiteboard-row-buttons">
                <%
                    }
                %>
                <button type="button" id="<%= whiteboard.getName() %>" class="selected-whiteboards"
                        onclick="location.href = '${pageContext.request.contextPath}/whiteboard?whiteboardID=<%= whiteboard.getId() %>'">
                    <%= whiteboard.toString() %>
                </button>
                <%
                    if(counter == 2 || whiteboards.indexOf(whiteboard) == whiteboards.size() - 1){
                        counter = 0;
                %>
            </div>
            <%
                        }
                        else{
                            counter++;
                        }
                    }
                }
            %>
        </div>
    </div>
</div>
</body>
</html>
