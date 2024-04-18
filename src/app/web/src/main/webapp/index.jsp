<%@ page contentType="text/html;charset=UTF-8" %>
<%@ page isELIgnored="false" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.ClientRedirector" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>

<html>
<head>
    <!-- Bootstrap CSS -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <!-- Custom CSS -->
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/login.css">
    <link rel="icon" type="image/x-icon" href="${pageContext.request.contextPath}/assets/images/favicon.ico">
    <!-- Bootstrap JS for form validation -->
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"></script>
    <!-- Custom JavaScript -->
    <script src="${pageContext.request.contextPath}/assets/javascript/login.js"></script>
</head>
<body>
<div class="container">
    <div class="custom-header">
        <div class="text-section">
            <span>LOGIN TO SHARED WHITEBOARDS</span>
        </div>
    </div>
    <div class="form-container">
        <form method="POST" action="${pageContext.request.contextPath}/login" class="needs-validation" novalidate>
            <div class="mb-3">
                <label for="username" class="form-label">Username:</label>
                <input type="text" id="username" name="username" class="form-control" required />
            </div>
            <div class="mb-3">
                <label for="password" class="form-label">Password:</label>
                <input type="password" id="password" name="password" class="form-control" required />
                <% if (request.getAttribute("loginError") != null && Boolean.TRUE.equals(request.getAttribute("loginError"))) { %>
                <div class="error-msg">Wrong username or password.</div>
                <% } %>
            </div>
            <button type="submit" class="btn btn-primary">Login</button>
        </form>
    </div>
    <div class="form-footer">
        <p>Don't have an account? <a href="${pageContext.request.contextPath}/signup">Join us</a></p>
    </div>
</div>
</body>
</html>
<%
    // Redirect user to main page if already logged in
    LoggedUserDTO loggedUser = AccessController.getLoggedUser(request);
    if (loggedUser != null) {
        ClientRedirector.redirectToMainPage(request, response);
    }
%>