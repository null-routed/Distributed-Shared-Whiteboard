<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.servlets.UserProfileServlet" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.AdditionalUserDataDTO" %>

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
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/common.css">
</head>
<body>
    <jsp:useBean id="userProfileServlet" class="it.unipi.dsmt.jakartaee.app.servlets.UserProfileServlet" />
    <jsp:include page="/WEB-INF/jsp/common/top_bar.jsp" />
    <%
        AdditionalUserDataDTO additionalUserDataDTO = userProfileServlet.getUserData(loggedUserDTO.getUsername());
    %>
    <div id="profile-container">
        <div class="profile-field">
            <label> Name: </label>
            <input type="text" value="<%=additionalUserDataDTO.getName()%>" readonly />
        </div>
        <div class="profile-field">
            <label> Surname: </label>
            <input type="text" value="<%=additionalUserDataDTO.getSurname()%>" readonly />
        </div>
        <div class="profile-field">
            <label> Email: </label>
            <input type="text" value="<%=additionalUserDataDTO.getEmail()%>" readonly />
        </div>
        <div class="profile-field">
            <label> Username: </label>
            <input type="text" value="<%=additionalUserDataDTO.getUsername()%>" readonly />
        </div>
    </div>
</body>
</html>
