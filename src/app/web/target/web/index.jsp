<%--
    Webapp login page
--%>
<%-- <%@ page import="it.unipi.dsmt.student_platform.utility.ClientRedirector" %> --%>
<%--<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>--%>
<%--<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>--%>
<%@ page contentType="text/html;charset=UTF-8" %>

<html>
<head>
    <title>Shared Whiteboards</title>
<%--    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/login.css">--%>
</head>
<body>
<div>
    <h1>Welcome to Shared Whiteboards!</h1>

    <div>
        <h2>Login</h2>

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

<%--    <button onclick="location.href = '${pageContext.request.contextPath}/signup'">--%>
<%--        You don't have an account? SIgn up now!--%>
<%--    </button>--%>
</div>


<%--<%--%>
<%--    // Check if the user failed the login--%>
<%--    String rParam = request.getParameter("r");--%>
<%--    if (rParam != null && rParam.equals("error")) {--%>
<%--%>--%>
<%--<script>--%>
<%--    alert("Error: failed login");--%>
<%--    location.href = "${pageContext.request.contextPath}/";--%>
<%--</script>--%>
<%--<%--%>
<%--    }--%>

<%--    // Redirect user is already logged--%>
<%--    LoggedUserDTO logged_user = AccessController.getLoggedUser(request);--%>
<%--    if (logged_user != null) {--%>
<%--        ClientRedirector.redirectToPortalPage(request, response, logged_user.getRole());--%>
<%--    }--%>

<%--%>--%>

</body>
</html>
