package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoginInformationsDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.ClientRedirector;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;
import java.util.List;
import java.util.Objects;


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

        int whiteboardID = Integer.parseInt(request.getParameter("whiteboardID"));

        // get participants through EJB
        List<String> whiteboardParticipants = whiteboardEJB.getParticipantUsernames(whiteboardID);

        request.setAttribute("whiteboardParticipants", whiteboardParticipants);

        request.getRequestDispatcher("/whiteboard.jsp").forward(request, response);
    }
}
