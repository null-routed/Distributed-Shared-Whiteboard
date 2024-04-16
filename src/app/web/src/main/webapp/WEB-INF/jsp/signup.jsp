<%@ page contentType="text/html;charset=UTF-8" %>
<%@ page import="java.util.Objects" %>

<html>
<head>
    <title>Signup to Shared Whiteboards</title>
    <!-- Bootstrap CSS -->
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
    <!-- Custom CSS -->
    <!--<link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/common.css">-->
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/signup.css">
    <script src="${pageContext.request.contextPath}/assets/javascript/login.js"></script>

</head>
<body>
<%
    String signupError = (String) request.getAttribute("signupError");
%>
<div class="container">
    <div class="custom-header">
        <div class="text-section">
            <span>CREATE AN ACCOUNT IN SHARED WHITEBOARDS</span>
        </div>
    </div>
    <div class="form-container">
        <form method="POST" action="${pageContext.request.contextPath}/signup">
            <div class="mb-3">
                <label for="name" class="form-label">Name:</label>
                <input type="text" id="name" name="name" class="form-control" required />
            </div>
            <div class="mb-3">
                <label for="surname" class="form-label">Surname:</label>
                <input type="text" id="surname" name="surname" class="form-control" required />
            </div>
            <div class="mb-3">
                <label for="email" class="form-label">Email address:</label>
                <input type="email" id="email" name="email" class="form-control" required />
                <% if (signupError != null && signupError.equals("email")) { %>
                <div class="error-msg"><%= request.getAttribute("errorMessage") %></div>
                <% } %>
            </div>
            <div class="mb-3">
                <label for="username" class="form-label">Choose your username:</label>
                <input type="text" id="username" name="username" class="form-control" required />
                <% if (signupError != null && signupError.equals("username")) { %>
                <div class="error-msg"><%= request.getAttribute("errorMessage") %></div>
                <% } %>
            </div>
            <div class="mb-3">
                <label for="password" class="form-label">Choose a password:</label>
                <input type="password" id="password" name="password" class="form-control" required />
            </div>
            <div class="mb-3">
                <label for="password-repeat" class="form-label">Repeat the password:</label>
                <input type="password" id="password-repeat" name="password-repeat" class="form-control" required />
            </div>
            <% if (signupError != null && (signupError.equals("generic") || signupError.equals("paramValidation"))) { %>
            <div class="error-msg"><%= request.getAttribute("errorMessage") %></div>
            <% } %>
            <button type="submit" class="btn btn-primary">Signup</button>
            <% if (signupError != null && signupError.equals("none")) { %>
            <div class="success-msg"><%= request.getAttribute("successMessage") %></div>
            <% } %>
        </form>
    </div>
    <div class="form-footer">
        <p>Already have an account? <a href="${pageContext.request.contextPath}/">Sign in!</a></p>
    </div>
</div>
</body>
</html>
