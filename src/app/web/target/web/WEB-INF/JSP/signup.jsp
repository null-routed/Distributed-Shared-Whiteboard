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
<%
    String signupError = (String) request.getAttribute("signupError");
%>
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
                    <% if (signupError != null && signupError.equals("email")) { %>
                    <span class="error-msg"><%= request.getAttribute("errorMessage") %></span>
                    <%}%>
                </div>
                <div class="form-field">
                    <label> Choose your username: </label>
                    <input type="text" name="username" required />
                    <% if (signupError != null && signupError.equals("username")) { %>
                    <span class="error-msg"><%= request.getAttribute("errorMessage") %></span>
                    <%}%>
                </div>
                <div class="form-field">
                    <label> Choose a password: </label>
                    <input type="password" name="password" required />
                </div>
                <div class="form-field">
                    <label> Repeat the password: </label>
                    <input type="password" name="password-repeat" required />
                </div>
                <% if (signupError != null && signupError.equals("generic")) { %>
                <span class="error-msg"><%= request.getAttribute("errorMessage") %></span>
                <%}%>
                <% if (signupError != null && signupError.equals("paramValidation")) { %>
                <span class="error-msg"><%= request.getAttribute("errorMessage") %></span>
                <%}%>
                <button type="submit" class="custom-submit-button"> SIGNUP </button>
                <% if (signupError != null && signupError.equals("none")) { %>
                <span class="success-msg"><%= request.getAttribute("successMessage") %></span>
                <%}%>
            </form>
        </div>
    </div>
    <div class="form-footer">
        <a onclick="location.href = '${pageContext.request.contextPath}/'">      <!-- redirect to login page -->
            Already have an account? Sign in!
        </a>
    </div>
</div>
</body>
</html>
