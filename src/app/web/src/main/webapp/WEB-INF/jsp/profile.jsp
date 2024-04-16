<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.AdditionalUserDataDTO" %>

<jsp:useBean id="userProfileServlet" class="it.unipi.dsmt.jakartaee.app.servlets.UserProfileServlet" />
<%
    AdditionalUserDataDTO userData = (AdditionalUserDataDTO) request.getAttribute("userData");
%>

<!DOCTYPE html>
<html>
<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null) {
        return;
    }
    System.out.println("@profile.jsp: visiting page with logged user " + loggedUserDTO.getUsername());
%>
<head>
    <title><%= loggedUserDTO.getUsername() %>'s Profile</title>
    <!-- Bootstrap CSS -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <!-- Custom CSS -->
    <link href="${pageContext.request.contextPath}/assets/css/profile.css" rel="stylesheet">
</head>
<body>
<!-- Include the top bar -->
<jsp:include page="/WEB-INF/jsp/common/top_bar.jsp" />

<!-- Profile container -->
<div class="container mt-4">
    <div class="row">
        <div class="col-md-6 offset-md-3">
            <div class="card">
                <div class="card-header">
                    <h5 class="card-title text-center">Profile</h5>
                </div>
                <div class="card-body">
                    <form>
                        <div class="mb-3">
                            <label for="name" class="form-label">Name:</label>
                            <input type="text" class="form-control" id="name" value="<%=userData.getName()%>" readonly>
                        </div>
                        <div class="mb-3">
                            <label for="surname" class="form-label">Surname:</label>
                            <input type="text" class="form-control" id="surname" value="<%=userData.getSurname()%>" readonly>
                        </div>
                        <div class="mb-3">
                            <label for="email" class="form-label">Email:</label>
                            <input type="email" class="form-control" id="email" value="<%=userData.getEmail()%>" readonly>
                        </div>
                        <div class="mb-3">
                            <label for="username" class="form-label">Username:</label>
                            <input type="text" class="form-control" id="username" value="<%=userData.getUsername()%>" readonly>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    </div>
</div>
</body>
</html>