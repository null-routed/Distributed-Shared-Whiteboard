package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.AddParticipantStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

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

        // request.setAttribute("whiteboardData", selectedWhiteboard);
        // request.setAttribute("whiteboardParticipants", whiteboardParticipants);

        request.getSession().setAttribute("whiteboardData", selectedWhiteboard);
        request.getSession().setAttribute("whiteboardParticipants", whiteboardParticipants);

        String targetPage = "/WEB-INF/jsp/whiteboard.jsp";
        request.getRequestDispatcher(targetPage).forward(request, response);
    }

    public static void printAttributeNames(HttpServletRequest request) {
        Enumeration<String> attributeNames = request.getAttributeNames();
        while (attributeNames.hasMoreElements()) {
            String attributeName = attributeNames.nextElement();
            System.out.println("Attribute Name: " + attributeName);
        }
    }

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        // Extracting parameters from the request
        String whiteboardID = Optional.ofNullable(request.getParameter("whiteboardID")).orElse("");
        String newParticipantUsername = Optional.ofNullable(request.getParameter("username")).orElse("");

        System.out.println("@WhiteboardServlet: called doPost() method, params=" + whiteboardID + ", " + newParticipantUsername);

        String targetPage = "/WEB-INF/jsp/whiteboard.jsp";

        boolean userIsAlreadyParticipant = whiteboardEJB.isParticipant(newParticipantUsername, whiteboardID);

        if (userIsAlreadyParticipant) {
            request.setAttribute("errorMessage", newParticipantUsername + " is already participating to this whiteboard.");
            request.getRequestDispatcher(targetPage).forward(request, response);
        }

        AddParticipantStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);
        switch (insertOutcome) {
            case SUCCESS:
                request.setAttribute("successMessage", newParticipantUsername + " has been added to this whiteboard!");
                request.setAttribute("errorMessage", null);
                break;
            case UNREGISTERED_USER:
                request.setAttribute("errorMessage", "The username you have provided doesn't seem to belong to any user. Try again.");
                request.setAttribute("successMessage", null);
                break;
            case OTHER_ERROR:
                request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
                request.setAttribute("successMessage", null);
                break;
        }

        printAttributeNames(request);

        request.getRequestDispatcher(targetPage).forward(request, response);
    }
}
