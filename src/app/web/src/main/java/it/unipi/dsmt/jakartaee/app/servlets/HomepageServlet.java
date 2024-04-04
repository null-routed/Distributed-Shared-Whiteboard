package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
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
    private WhiteboardEJB whiteboardEJB;

    /**
     * function invoked by get and post request to handle them
     * in order to retrieve and load the data of the page
     * @param request HttpServletRequest object
     * @param response HttpServletRequest object
     * @throws ServletException if forwarding fails
     * @throws IOException if forwarding fails
     */
    private void handleRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        List<MinimalWhiteboardDTO> whiteboards;

        System.out.println("Homepage Servlet");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null) {
            return;
        }
        String shared =  request.getParameter("shared");
        String searchInput = request.getParameter("search_input");

        if (searchInput != null) {
            request.setAttribute("search_input", searchInput);
            whiteboards = whiteboardEJB.searchDashboards(searchInput);
        }
        else if ((shared != null) && (shared.equals("true"))) {
            whiteboards = whiteboardEJB.getSharedDashboards(loggedUserDTO.getId());
            request.setAttribute("shared", shared);
        }
        else {
            whiteboards = whiteboardEJB.getAllDashboards(loggedUserDTO.getId());
        }
        request.setAttribute("whiteboards", whiteboards);

        String relativePath = "/WEB-INF/jsp/homepage.jsp";

        RequestDispatcher requestDispatcher = request.getRequestDispatcher(relativePath);
        requestDispatcher.forward(request, response);
    }

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null) {
            return;
        }
        // Retrieve parameters from the request
        String whiteboardName = request.getParameter("whiteboardName");
        String whiteboardDescription = request.getParameter("whiteboardDescription");
        String readOnlyParam = request.getParameter("readOnly");

        // Convert readOnlyParam to boolean
        boolean isReadOnly = "on".equals(readOnlyParam);

        WhiteboardCreationDTO newWhiteboard = new WhiteboardCreationDTO(whiteboardName, whiteboardDescription,
                isReadOnly);

        try {
            if(!whiteboardEJB.addWhiteboard(loggedUserDTO.getId(), newWhiteboard)) {
                System.out.println("whiteboard insertion failed");
                return;
            }
        } catch (RuntimeException e) {
            e.printStackTrace();
        }

        response.sendRedirect(request.getContextPath() + "/homepage"); // Redirect to the homepage

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
