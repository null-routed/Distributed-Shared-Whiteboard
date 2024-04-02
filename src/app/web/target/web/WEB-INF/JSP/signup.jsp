<%--
  Webapp signup page
--%>
<%@ page contentType="text/html;charset=UTF-8" %>

<html>
<head>
    <title>Signup to Shared Whiteboards</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/login.css">
</head>
<body>
<div>
    <h1>Welcome to Shared Whiteboards!</h1>

    <div>
        <h2>Register to Shared Whiteboards</h2>

        <form method="POST" action="${pageContext.request.contextPath}/signup">     <!-- redirect to signup servlet -->
            <label>
                Name:
                <input type="text" name="name" placeholder="Name" required />
            </label>
            <br>
            <label>
                Surname:
                <input type="text" name="surname" placeholder="Surname" required />
            </label>
            <br>
            <label>
                Email address:
                <input type="email" name="email" placeholder="Email address" required>
            </label>
            <br>
            <label>
                Choose your username:
                <input type="text" name="username" placeholder="Username" required>
            </label>
            <label>
                Choose a password:
                <input type="password" name="password" placeholder="Password" required>
            </label>
            <br>
            <label>
                Repeat the password:
                <input type="password" name="password-repeat" placeholder="Repeat password" required>
            </label>
            <button type="submit" class="submit-button">LOGIN</button>
        </form>
    </div>

    <button onclick="location.href = '${pageContext.request.contextPath}/login'">      <!-- redirect to login page -->
        Already have an account? Sign in!
    </button>

    <% // Signup procedure check

        // null = first time visiting page
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
            <p> <%= successMessage %> </p>
    <%
            }
        }
    %>

</div>
</body>
</html>
