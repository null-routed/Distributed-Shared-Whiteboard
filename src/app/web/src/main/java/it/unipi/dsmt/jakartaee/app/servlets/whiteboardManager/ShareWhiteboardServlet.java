package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.enums.AddParticipantStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.UserTransaction;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Optional;


@WebServlet(name = "ShareWhiteboardServlet", value = "/share_whiteboard")
public class ShareWhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Resource
    private UserTransaction userTransaction;

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        response.setContentType("application/json");

        // Extracting parameters from the request
        String whiteboardID = Optional.ofNullable(request.getParameter("whiteboardID")).orElse("");
        String newParticipantUsername = Optional.ofNullable(request.getParameter("username")).orElse("");

        System.out.println("@WhiteboardServlet: called doPost() method, params=" + whiteboardID + ", " + newParticipantUsername);

        AddParticipantStatus alreadyParticipantCheck = whiteboardEJB.isParticipant(newParticipantUsername, whiteboardID);

        JsonObject jsonResponse;
        if (alreadyParticipantCheck == AddParticipantStatus.ALREADY_PARTICIPATING) {
            jsonResponse = Json.createObjectBuilder()
                    .add("success", false)
                    .add("message", newParticipantUsername + " is already participating to this whiteboard.")
                    .build();
        } else if (alreadyParticipantCheck == AddParticipantStatus.OTHER_ERROR) {
            jsonResponse = Json.createObjectBuilder()
                    .add("success", false)
                    .add("message", "An error occurred. Try again or try in a few minutes.")
                    .build();
        } else {
            AddParticipantStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);

            switch (insertOutcome) {
                case SQL_SUCCESS:
                    jsonResponse = Json.createObjectBuilder()
                            .add("success", true)
                            .add("message", newParticipantUsername + " has been added to this whiteboard!")
                            .build();
                    break;
                case UNREGISTERED_USER:
                    jsonResponse = Json.createObjectBuilder()
                            .add("success", false)
                            .add("message", "The username you have provided doesn't seem to belong to any user. Try again.")
                            .build();
                    break;
                case OTHER_ERROR:
                    jsonResponse = Json.createObjectBuilder()
                            .add("success", false)
                            .add("message", "An error occurred. Try again or try in a few minutes.")
                            .build();
                    break;
                default:
                    jsonResponse = Json.createObjectBuilder()
                            .add("success", false)
                            .add("message", "An error occurred. Try again or try in a few minutes.")
                            .build();
            }
        }

        PrintWriter out = response.getWriter();
        out.print(jsonResponse);
        out.flush();
    }

//    @Override
//    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
//
//        // Extracting parameters from the request
//        String whiteboardID = Optional.ofNullable(request.getParameter("whiteboardID")).orElse("");
//        String newParticipantUsername = Optional.ofNullable(request.getParameter("username")).orElse("");
//
//        System.out.println("@WhiteboardServlet: called doPost() method, params=" + whiteboardID + ", " + newParticipantUsername);
//
//        String targetPage = "/WEB-INF/jsp/whiteboard.jsp";
//        request.setAttribute("redirectAfterAddOperation", true);   // needed for the modal display after an add operation
//
//        AddParticipantStatus alreadyParticipantCheck = whiteboardEJB.isParticipant(newParticipantUsername, whiteboardID);
//
//        if (alreadyParticipantCheck == AddParticipantStatus.ALREADY_PARTICIPATING) {
//            request.setAttribute("errorMessage", newParticipantUsername + " is already participating to this whiteboard.");
//            request.getRequestDispatcher(targetPage).forward(request, response);
//            return;
//        } else if (alreadyParticipantCheck == AddParticipantStatus.OTHER_ERROR) {
//            request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
//            request.getRequestDispatcher(targetPage).forward(request, response);
//            return;
//        }
//
//        // Erlang + MySQL operations transaction
//        try {
//            userTransaction.begin();
//
//            AddParticipantStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);
//
//            switch (insertOutcome) {
//                case SQL_SUCCESS:
//                    boolean erlangShareOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
//                            "insert",
//                            whiteboardID,
//                            newParticipantUsername,
//                            false
//                    );
//
//                    if (erlangShareOperationOutcome) {
//                        userTransaction.commit();
//                        request.setAttribute("newlyInsertedParticipant", newParticipantUsername);
//                        request.setAttribute("successMessage", newParticipantUsername + " has been added to this whiteboard!");
//                        request.getRequestDispatcher(targetPage).forward(request, response);    // redirect to whiteboard
//                    } else {
//                        userTransaction.rollback();
//                        request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
//                        request.getRequestDispatcher(targetPage).forward(request, response);
//                    }
//                    break;
//                case UNREGISTERED_USER:
//                    request.setAttribute("errorMessage", "The username you have provided doesn't seem to belong to any user. Try again.");
//                    request.getRequestDispatcher(targetPage).forward(request, response);
//                    break;
//                case OTHER_ERROR:
//                    request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
//                    request.getRequestDispatcher(targetPage).forward(request, response);
//                    break;
//            }
//        } catch (Exception e) {
//            try {
//                userTransaction.rollback();
//            } catch (Exception ex) {
//                // throw new RuntimeException(ex);
//                request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
//                request.getRequestDispatcher(targetPage).forward(request, response);
//            }
//            // throw new RuntimeException(e);
//            request.setAttribute("errorMessage", "An error occurred. Try again or try in a few minutes.");
//            request.getRequestDispatcher(targetPage).forward(request, response);
//        }
//    }
}
