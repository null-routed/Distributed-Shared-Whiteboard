package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.DashboardDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.DashboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.List;

@WebServlet(name = "HomepageServlet", value = "/homepage")
public class HomepageServlet extends HttpServlet {

    @EJB
    private DashboardEJB dashboardEJB;

    /**
     * function invoked by get and post request to handle them
     * in order to retrieve and load the data of the page
     * @param request HttpServletRequest object
     * @param response HttpServletRequest object
     * @throws ServletException if forwarding fails
     * @throws IOException if forwarding fails
     */
    private void handleRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        List<DashboardDTO> dashboards;

        System.out.println("Homepage Servlet");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null) {
            return;
        }
        String shared =  request.getParameter("shared");
        String searchInput = request.getParameter("search_input");

        if (searchInput != null) {
            request.setAttribute("search_input", searchInput);
            dashboards = dashboardEJB.searchDashboards(searchInput);
        }
        else if ((shared != null) && (shared.equals("true"))) {
            dashboards = dashboardEJB.getSharedDashboards(loggedUserDTO.getId());
            request.setAttribute("shared", shared);
        }
        else {
            dashboards = dashboardEJB.getAllDashboards();
        }
        request.setAttribute("dashboards", dashboards);

        String contextPath = request.getContextPath();
        String relativePath = "/WEB-INF/jsp/homepage.jsp";
        String fullPath = contextPath + relativePath;

        System.out.println("Full Path: " + fullPath);

        RequestDispatcher requestDispatcher = request.getRequestDispatcher(relativePath);
        requestDispatcher.forward(request, response);
    }

    /**
     * Redefinition of the doGet, through the handleRequest invocation
     * @param request HttpServletRequest object
     * @param response HttpServletRequest object
     * @throws ServletException if forwarding fails
     * @throws IOException if forwarding fails
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        handleRequest(request, response);
    }
}
