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
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/login.css">
</head>
<body>
<div>
    <h1>Welcome to Shared Whiteboards!</h1>

    <div>
        <h2>Login</h2>
        <h2>Context path: <%= request.getContextPath() %></h2>

        <form method="POST" action="${pageContext.request.contextPath}/login">
            <label>
                Username:
                <input type="text" name="username" placeholder="Username" required />
            </label>
            <br>
            <label>
                Password:
                <input type="password" name="password" placeholder="Password" required />
            </label>
            <br>
            <button type="submit" class="submit-button">LOGIN</button>
        </form>
    </div>

    <button onclick="location.href = '${pageContext.request.contextPath}/signup'">
        You don't have an account? Sign up now!
    </button>
</div>


<%
    // Login failure check
    String outcomeParam = request.getParameter("param");
    if (outcomeParam != null && outcomeParam.equals("error")) {
%>
        <script>
            console.log("Wrong username or pwd");   // TODO: show error messages under text box SNH-like
            alert("Error: wrong username or password");
            location.href = "${pageContext.request.contextPath}/";
        </script>
<%
    }

    // Redirect user to main page
    LoggedUserDTO loggedUser = AccessController.getLoggedUser(request);
    if (loggedUser != null) {
        ClientRedirector.redirectToMainPage(request, response);
    }
%>

</body>
</html>
