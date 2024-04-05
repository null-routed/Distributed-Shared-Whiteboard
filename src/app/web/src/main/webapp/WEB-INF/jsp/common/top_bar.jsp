<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>

<link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/top_bar.css">

<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null)
        return;
%>

<div class="topnav">
    <div id="left-container">
        <button onclick="location.href = '${pageContext.request.contextPath}/homepage'">
            Back
        </button>
        <button class="custom-generic-button" onclick="location.href = '${pageContext.request.contextPath}/homepage'">
            Homepage
        </button>
        <button class="custom-generic-button" onclick="location.href = '${pageContext.request.contextPath}/profile?userId=<%= loggedUserDTO.getId()%>'">
            Profile
        </button>
    </div>
    <div id="right-container">
        <button class="custom-generic-button" onclick="location.href = '${pageContext.request.contextPath}/logout'">
            Logout
        </button>
    </div>
</div>