<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>

<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null) {
        return;
    }

    int dashboardId =  Integer.parseInt(request.getParameter("id"));

    String dashboardName;
    if(request.getAttribute("dashboard_name") != null){
        dashboardName = request.getAttribute("dashboard_name").toString();
    }
    else{
        return;
    }
%>
<head>
    <title>Dashboard</title>
    <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/assets/css/dashboard.css">
    <script src="${pageContext.request.contextPath}/assets/javascript/dashboard.js"></script>
</head>
<body onload="connect('<%= loggedUserDTO.getUsername() %>', <%= dashboardId %>)" onunload="disconnect()">
<div id="dashboard-page">
    <jsp:include page="/WEB-INF/JSP/common/top_bar.jsp" />

    <div id="dashboard-container">
        <div id="online-user-list-container">
            <header class="online-user-list-header">
                <b>Online Users:</b>
            </header>
            <div class="online-user-list-content">
                <ul id="online-user-list">
                    <li>No Online Users</li>
                </ul>
            </div>
        </div>

        <div class="dashboard">
            <header class="dashboard-header">
                <b><%=dashboardName%> Dashboard</b>
            </header>
            <main id="dashboard-frontend"> </main>

            <div class="dashboard-submit-area">
                <!-- <canvas type="text" id="dashboard-submit-area-input" placeholder="Enter your message..."/> -->
                <button type="submit" class="dashboard-submit-area-button" onclick="publishDraw()">
                    publish
                </button>
            </div>
        </div>
    </div>
</div>

</body>
</html>
