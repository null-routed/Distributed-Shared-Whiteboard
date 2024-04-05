package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;
import java.util.List;


/**
 * Servlet handling requests for whiteboards.
 */
@WebServlet(name = "WhiteboardServlet", value = "/whiteboard")
public class WhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    /**
     * Handle a POST request by executing the login procedure.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if redirection fails
     */
    @Override
    protected void doGet (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        System.out.println("@WhiteboardServlet: called doGet() method");

        // Getting the ID of the chosen whiteboard via GET parameter
        int whiteboardID = Integer.parseInt(request.getParameter("whiteboardID"));

        // Getting the selected whiteboard data and participants through EJB
        MinimalWhiteboardDTO selectedWhiteboard = whiteboardEJB.getWhiteboardByID(whiteboardID);
        List<String> whiteboardParticipants = whiteboardEJB.getParticipantUsernames(whiteboardID);

        request.setAttribute("whiteboardData", selectedWhiteboard);
        request.setAttribute("whiteboardParticipants", whiteboardParticipants);

        String targetPage = "/WEB-INF/jsp/whiteboard.jsp";
        request.getRequestDispatcher(targetPage).forward(request, response);
    }
}
