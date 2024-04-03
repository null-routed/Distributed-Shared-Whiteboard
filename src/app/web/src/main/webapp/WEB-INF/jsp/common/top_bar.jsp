<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>

<link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/top_bar.css">

<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null)
        return;
%>

<div class="topnav">
    <div id="logo">
        <img src="${pageContext.request.contextPath}/assets/images/logo.svg" alt="Logo"/>
    </div>
    <div id="left-container">
        <button onclick="location.href = '${pageContext.request.contextPath}/<%= loggedUserDTO.getUsername() %>/portal'">
            Back
        </button>

        <button onclick="location.href = '${pageContext.request.contextPath}/<%= loggedUserDTO.getUsername() %>/profile'">
            Profile
        </button>
    </div>
    <div id="flex-container"></div>
    <div id="right-container">
        <button onclick="location.href = '${pageContext.request.contextPath}/logout'">
            Logout
        </button>
    </div>
</div>