<%@ page import="it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.utility.AccessController" %>
<%@ page import="it.unipi.dsmt.jakartaee.app.dto.DashboardDTO" %>
<%@ page import="java.util.List" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null) {
        return;
    }

    List<DashboardDTO> dashboards = (List<DashboardDTO>) request.getAttribute("dashboards");

    String isSharedView = "";

    if(request.getAttribute("shared") != null){
        isSharedView = request.getAttribute("starred").toString();
        System.out.println(isSharedView);
    }
%>
<head>
    <title>Homepage</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/homepage.css">
</head>
<body>
<div id="homepage-container">
    <jsp:include page="/WEB-INF/JSP/common/top_bar.jsp" />

    <br>
    <h1>Your Dashboards</h1>
    <br>
    <div id="dashboard-selection-container">
        <div id="dashboard-selection">
            <div class="dashboard-search">
                <form class="search-form" action="${pageContext.request.contextPath}/homepage" method="get">
                    <label class="search-input-label" for="search-input">Search Dashboards:</label>
                    <input type="text" id="search-input" name="search_input" placeholder="Type the dashboard name ...">
                    <button type="submit" class="homepage-button">Search</button>
                </form>
            </div>
            <div id="dashboard-type">
                <form class="search-form" action="${pageContext.request.contextPath}/homepage" method="get">
                    <%
                        if(isSharedView.equals("true")){
                    %>
                    <button type="submit" name="shared" class="platform-button" value="false">
                        See Other Dashboards
                    </button>
                    <%
                    }
                    else{
                    %>
                    <button type="submit" name="starred" class="platform-button" value="true">
                        See Shared Dashboards
                    </button>
                    <%
                        }
                    %>
                </form>
            </div>
        </div>
        <hr class="hr-style">
        <div id="dashboards">
            <script>
                const dashboardsDiv = document.getElementById("dashboards");
                dashboardsDiv.innerHTML = "";
            </script>
            <%
                int counter = 0;
                for (DashboardDTO dashboard : dashboards) {
                    if(counter == 0){
            %>
            <div class="course-row-buttons">
                <%
                    }
                %>
                <button type="button" id="<%= dashboard.getName() %>" class="selected-courses"
                        onclick="location.href = '${pageContext.request.contextPath}/student/course?id=<%= dashboard.getId() %>'">
                    <%= dashboard.toString() %>
                </button>
                <%
                    if(counter == 2 || dashboards.indexOf(dashboard) == dashboards.size() - 1){
                        counter = 0;
                %>
            </div>
            <%
                    }
                    else{
                        counter++;
                    }
                }
            %>
        </div>
    </div>
</div>
</body>
</html>
