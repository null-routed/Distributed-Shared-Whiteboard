<%--
  Webapp signup page
--%>
<%@ page contentType="text/html;charset=UTF-8" %>

<html>
<head>
    <title>Signup to Shared Whiteboards</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common.css">
</head>
<body>
<div>
    <div class="custom-header">
        <div class="text-section">
            <span> CREATE AN ACCOUNT IN SHARED WHITEBOARDS </span>
        </div>
    </div>
    <div>
        <div class="form-container">
        <form method="POST" action="${pageContext.request.contextPath}/signup">     <!-- redirect to signup servlet -->
            <div class="form-field">
                <label> Name: </label>
                <input type="text" name="name" required />
            </div>
            <div class="form-field">
                <label> Surname: </label>
                <input type="text" name="surname" required />
            </div>
            <div class="form-field">
                <label> Email address: </label>
                <input type="email" name="email" required />
            </div>
            <div class="form-field">
                <label> Choose your username: </label>
                <input type="text" name="username" required />
            </div>
            <div class="form-field">
                <label> Choose a password: </label>
                <input type="password" name="password" required />
            </div>
            <div class="form-field">
                <label> Repeat the password: </label>
                <input type="password" name="password-repeat" required />
            </div>
            <button type="submit" class="custom-submit-button"> SIGNUP </button>
        </form>
    </div>
    <div class="form-footer">
        <a onclick="location.href = '${pageContext.request.contextPath}/login'">      <!-- redirect to login page -->
            Already have an account? Sign in!
        </a>
    </div>

    <% // Signup procedure check

        // null = first time visiting page, no error is set
        if (request.getAttribute("signupError") != null) {
            // Getting 'signupError' attribute set by SignupServlet. If success, it'll be false
            boolean failedSignupProcess = (boolean) request.getAttribute("signupError");

            if (failedSignupProcess) { // failed signup procedure for some reason, access to errorMessage to know it
                String errorMessage = (String) request.getAttribute("errorMessage");
    %>
    <script>
        alert("<%= errorMessage %>");
    </script>
    <%
        } else { // signup procedure successfully completed
            // show message
            String successMessage = (String) request.getAttribute("successMessage");
    %>
            <p> suca </p>
    <%
            }
        }
    %>

</div>
</body>
</html>
