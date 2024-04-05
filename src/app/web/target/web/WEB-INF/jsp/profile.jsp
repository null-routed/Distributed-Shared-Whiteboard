<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>
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
        AdditionalUserDataDTO userData = (AdditionalUserDataDTO) request.getAttribute("userData");
    %>
    <div id="profile-container">
        <div class="profile-field">
            <label> Name: </label>
            <label>
                <input type="text" value="<%=userData.getName()%>" readonly />
            </label>
        </div>
        <div class="profile-field">
            <label> Surname: </label>
            <label>
                <input type="text" value="<%=userData.getSurname()%>" readonly />
            </label>
        </div>
        <div class="profile-field">
            <label> Email: </label>
            <label>
                <input type="text" value="<%=userData.getEmail()%>" readonly />
            </label>
        </div>
        <div class="profile-field">
            <label> Username: </label>
            <label>
                <input type="text" value="<%=userData.getUsername()%>" readonly />
            </label>
        </div>
    </div>
</body>
</html>
