<%--
    Webapp login page
--%>
<%@ page contentType="text/html;charset=UTF-8" %>
<%@ page isELIgnored="false" %>

<%@ page import="it.unipi.dsmt.jakartaee.app.utility.ClientRedirector" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>

<html>
<head>
    <title>Shared Whiteboards</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common.css">
</head>
<body>
<div>
    <div class="custom-header">
        <div class="text-section">
            <span> LOGIN TO SHARED WHITEBOARDS </span>
        </div>
    </div>
    <div class="form-container">
        <form method="POST" action="${pageContext.request.contextPath}/login">
            <div class="form-field">
                <label> Username: </label>
                <input type="text" name="username" required />
            </div>
            <div class="form-field">
                <label> Password: </label>
                <input type="password" name="password" required />
                <% if (request.getParameter("loginError") != null) { %>             <!-- error message span visibility check -->
                <span class="error-msg"> Wrong username or password. </span>
                <% } %>
            </div>
            <button type="submit" class="custom-submit-button"> LOGIN </button>
        </form>
    </div>
    <div class="form-footer">
        <a onclick="location.href = '${pageContext.request.contextPath}/signup'">
            Join us
        </a>
    </div>
</div>

<%
    // Redirect user to main page
    LoggedUserDTO loggedUser = AccessController.getLoggedUser(request);
    if (loggedUser != null) {
        ClientRedirector.redirectToMainPage(request, response);
    }
%>

</body>
</html>
