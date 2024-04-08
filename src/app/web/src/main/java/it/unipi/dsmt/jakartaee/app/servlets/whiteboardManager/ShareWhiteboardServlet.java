package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.enums.AddParticipantStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.Optional;


@WebServlet(name = "ShareWhiteboardServlet", value = "/share_whiteboard")
public class ShareWhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {

        // Extracting parameters from the request
        String whiteboardID = Optional.ofNullable(request.getParameter("whiteboardID")).orElse("");
        String newParticipantUsername = Optional.ofNullable(request.getParameter("username")).orElse("");

        System.out.println("@WhiteboardServlet: called doPost() method, params=" + whiteboardID + ", " + newParticipantUsername);

        String targetPage = "/WEB-INF/jsp/whiteboard.jsp";
        request.setAttribute("redirectAfterAddOperation", true);   // needed for the modal display after an add operation

        AddParticipantStatus alreadyParticipantCheck = whiteboardEJB.isParticipant(newParticipantUsername, whiteboardID);

        if (alreadyParticipantCheck == AddParticipantStatus.ALREADY_PARTICIPATING) {
            request.setAttribute("errorMessage", newParticipantUsername + " is already participating to this whiteboard.");
            request.getRequestDispatcher(targetPage).forward(request, response);
            return;
        } else if (alreadyParticipantCheck == AddParticipantStatus.OTHER_ERROR) {
            request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
            request.getRequestDispatcher(targetPage).forward(request, response);
            return;
        }

        // TODO: transaction, if erlang fails the also SQL has to be rolled back
        AddParticipantStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);
        switch (insertOutcome) {
            case SQL_SUCCESS:
                //Erlang communication -> Proceed with Erlang synchronization only if the operation was ok in MySQL DB
                try {
                    boolean erlangOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                            "insert",
                            whiteboardID,
                            newParticipantUsername,
                            false
                    );
                    if(erlangOperationOutcome) {
                        request.setAttribute("newlyInsertedParticipant", newParticipantUsername);
                        request.setAttribute("successMessage", newParticipantUsername + " has been added to this whiteboard!");
                        request.getRequestDispatcher(targetPage).forward(request, response);    // redirect to whiteboard
                    } else {
                        request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
                        request.getRequestDispatcher(targetPage).forward(request, response);
                    }
                } catch (RuntimeException e) {
                    throw new RuntimeException(e);
                }
                break;
            case UNREGISTERED_USER:
                request.setAttribute("errorMessage", "The username you have provided doesn't seem to belong to any user. Try again.");
                request.getRequestDispatcher(targetPage).forward(request, response);
                break;
            case OTHER_ERROR:
                request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
                request.getRequestDispatcher(targetPage).forward(request, response);
                break;
        }
    }
}
