<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO" %>
<%@ page import="java.util.Objects" %>
<%@ page contentType="text/html;charset=UTF-8"%>

<% List<MinimalWhiteboardDTO> whiteboards = (List<MinimalWhiteboardDTO>) request.getAttribute("whiteboards"); %>
<% String isSharedView = request.getParameter("shared"); %>
<% String selfUsername = request.getParameter("selfUsername"); %>
<html>
<head>
    <title>Homepage</title>
    <!-- Custom CSS -->
    <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/assets/images/favicon.ico">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/common.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/homepage.css">
    <!-- Bootstrap CSS -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    <link href="https://cdnjs.cloudflare.com/ajax/libs/bootstrap-icons/1.9.1/font/bootstrap-icons.min.css" rel="stylesheet">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js"></script>
    <!-- Custom JavaScript -->
    <script type="text/javascript"> let pageContext = "${pageContext.request.contextPath}"; </script>
</head>
<body>
<div id="homepage-container">
    <jsp:include page="/WEB-INF/jsp/common/top_bar.jsp" />
     <div class="modal fade" id="error-modal" tabindex="-1" role="dialog" aria-labelledby="errorModalLabel" aria-hidden="true">
        <div class="modal-dialog" role="document">
            <div class="modal-content">
                <div class="modal-header">
                    <h5 class="modal-title" id="errorModalLabel">Error</h5>
                    <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                </div>
                <div class="modal-body">
                    <p>Operation failed. Try again or try in a few minutes.</p>
                </div>
            </div>
        </div>
    </div>

    <!-- MAIN PAGE -->
    <div class="container" style="max-width: 900px; margin: 0 auto;">
        <div class="row mb-4 justify-content-center">
            <div class="col-12 mt-3">
                <h1 class="text-center">Your Whiteboards</h1>
            </div>
        </div>

        <div class="row">
            <div class="col-lg-12">
                <div class="d-flex justify-content-between align-items-center">
                    <form action="${pageContext.request.contextPath}/homepage" method="get" class="d-flex flex-grow-1 mr-2">
                        <div class="flex-grow-1 mr-2">
                            <label for="search-input" class="sr-only visually-hidden">Search</label>
                            <input type="text"
                                   id="search-input"
                                   name="search_input"
                                   class="form-control form-control-sm w-100 py-2"
                                   placeholder="Type the whiteboard name ...">
                        </div>
                        <button type="submit" class="btn btn-primary btn-sm mx-1 py-2">Search</button>
                    </form>
                    <% if (isSharedView != null && isSharedView.equals("true")) { %>
                        <button id="all-view" class="btn btn-secondary btn-sm py-2" >Other</button>
                    <% } else { %>
                        <button id="shared-view" class="btn btn-secondary btn-sm py-2" >Shared with me</button>
                    <% } %>
                </div>
            </div>

        </div>

        <hr>
        <div class="row" id="whiteboard-container">
            <div class="col-sm-6 col-md-4 col-lg-3 mb-4">
                <div class="card border-0">
                    <div class="card-img-top-wrapper">
                        <button id="new-whiteboard"
                                type="button"
                                class="btn btn-outline-primary btn-add"
                                data-bs-toggle="modal"
                                data-bs-target="#insert-whiteboard-modal"
                                style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
                        >+</button>
                    </div>
                </div>
            </div>
            <% if (whiteboards != null) { %>
                <% for (MinimalWhiteboardDTO whiteboard : whiteboards) { %>
                    <div class="col-sm-6 col-md-4 col-lg-3 mb-4" id="whiteboard_<%= whiteboard.getId() %>">
                        <div class="card">
                            <div class="card-img-top-wrapper">
                                <a href="${pageContext.request.contextPath}/whiteboard?whiteboardID=<%= whiteboard.getId() %>">
                                    <img class="card-img-top" alt="<%= whiteboard.getName() %>"
                                         data-toggle="popover"
                                         data-bs-title="<%= whiteboard.getName() %>"
                                         <% if (!whiteboard.getDescription().isEmpty()) {%>
                                            data-bs-content="<%= whiteboard.getDescription() %>"
                                         <%}%>
                                         data-placement="right"
                                         src="${pageContext.request.contextPath}/snapshot_manager?whiteboardID=<%= whiteboard.getId() %>">
                                </a>
                                <button type="button" class="delete-whiteboard-button btn btn-danger btn-sm" data-wb-id="<%= whiteboard.getId() %>">
                                    <i class="bi bi-x"></i>
                                </button>
                            </div>
                        </div>
                    </div>
                <% } %>
            <% } %>
        </div>

    </div>
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
                    <button id="insert-new-wb-submit" type="button" class="btn btn-primary">Submit</button>
                </div>
            </div>
        </div>
    </div>
</div>

<!-- Toast Container -->
<div id="toast-container" class="position-fixed bottom-0 end-0 p-3"></div>

<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
<script type="module" src="${pageContext.request.contextPath}/assets/javascript/homepage.js"></script>
<input type="hidden" id="self-username" value="<%= selfUsername %>">
</body>
</html>
