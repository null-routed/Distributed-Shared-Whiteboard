package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.AddParticipantStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import javax.swing.plaf.metal.OceanTheme;
import java.io.IOException;
import java.util.Enumeration;
import java.util.List;
import java.util.Optional;


/**
 * Servlet handling requests for whiteboards.
 */
@WebServlet(name = "WhiteboardServlet", value = "/whiteboard")
public class WhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    /**
     * Handle a GET request by forwarding to the specified whiteboard.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if redirection fails
     */
    @Override
    protected void doGet (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        System.out.println("@WhiteboardServlet: called doGet() method");

        String whiteboardIDParam = request.getParameter("whiteboardID");

        if (whiteboardIDParam == null || whiteboardIDParam.isEmpty()) {
            System.out.println("@WhiteboardServlet, doGet(): ID parameter missing");
            return;
        }

        int whiteboardID;
        try {
            whiteboardID = Integer.parseInt(whiteboardIDParam);
        } catch (NumberFormatException e) {
            System.out.println("@WhiteboardServlet, doGet(): invalid ID parameter");
            return;
        }

        MinimalWhiteboardDTO selectedWhiteboard = whiteboardEJB.getWhiteboardByID(whiteboardID);
        List<String> whiteboardParticipants = whiteboardEJB.getParticipantUsernames(whiteboardID);

        request.getSession().setAttribute("whiteboardData", selectedWhiteboard);
        request.getSession().setAttribute("whiteboardParticipants", whiteboardParticipants);

        String targetPage = "/WEB-INF/jsp/whiteboard.jsp";
        request.getRequestDispatcher(targetPage).forward(request, response);
    }
}
