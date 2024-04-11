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
//
//    @Override
//    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
//        response.setContentType("application/json");
//
//        // Extracting parameters from the request
//        String whiteboardID = Optional.ofNullable(request.getParameter("whiteboardID")).orElse("");
//        String newParticipantUsername = Optional.ofNullable(request.getParameter("username")).orElse("");
//
//        System.out.println("@WhiteboardServlet: called doPost() method, params=" + whiteboardID + ", " + newParticipantUsername);
//
//        JsonObject jsonResponse = Json.createObjectBuilder().build();
//
//        AddParticipantStatus alreadyParticipantCheck = whiteboardEJB.isParticipant(newParticipantUsername, whiteboardID);
//
//        if (alreadyParticipantCheck == AddParticipantStatus.ALREADY_PARTICIPATING) {
//            jsonResponse = Json.createObjectBuilder()
//                    .add("success", false)
//                    .add("message", newParticipantUsername + " is already participating to this whiteboard.")
//                    .build();
//        } else if (alreadyParticipantCheck == AddParticipantStatus.OTHER_ERROR) {
//            jsonResponse = Json.createObjectBuilder()
//                    .add("success", false)
//                    .add("message", "An error occurred. Try again or try in a few minutes.")
//                    .build();
//        } else {
//            // Erlang + MySQL operations transaction
//            try {
//                userTransaction.begin();
//
//                AddParticipantStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);
//
//                switch (insertOutcome) {
//                    case SQL_SUCCESS:
//                        boolean erlangShareOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
//                                "insert",
//                                whiteboardID,
//                                newParticipantUsername,
//                                false
//                        );
//
//                        if (erlangShareOperationOutcome) {
//                            userTransaction.commit();
//                            jsonResponse = Json.createObjectBuilder()
//                                    .add("success", true)
//                                    .add("message", newParticipantUsername + " has been added to this whiteboard!")
//                                    .build();
//                            // request.setAttribute("newlyInsertedParticipant", newParticipantUsername);
//                        } else {
//                            userTransaction.rollback();
//                            jsonResponse = Json.createObjectBuilder()
//                                    .add("success", false)
//                                    .add("message", "An error occurred. Try again or try in a few minutes.")
//                                    .build();
//                        }
//                        break;
//                    case UNREGISTERED_USER:
//                        jsonResponse = Json.createObjectBuilder()
//                                .add("success", false)
//                                .add("message", "The username you have provided doesn't seem to belong to any user. Try again.")
//                                .build();
//                        break;
//                    case OTHER_ERROR:
//                        jsonResponse = Json.createObjectBuilder()
//                                .add("success", false)
//                                .add("message", "An error occurred. Try again or try in a few minutes.")
//                                .build();
//                        break;
//                }
//            } catch (Exception e) {
//                try {
//                    userTransaction.rollback();
//                } catch (Exception ex) {
//                    // Handle rollback exception
//                }
//                jsonResponse = Json.createObjectBuilder()
//                        .add("success", false)
//                        .add("message", "An error occurred. Try again or try in a few minutes.")
//                        .build();
//            }
//        }
//
//        PrintWriter out = response.getWriter();
//        out.print(jsonResponse);
//        out.flush();
//    }


    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        response.setContentType("application/json");

        // Extracting parameters from the request
        String whiteboardID = Optional.ofNullable(request.getParameter("whiteboardID")).orElse("");
        String newParticipantUsername = Optional.ofNullable(request.getParameter("username")).orElse("");

        System.out.println("@WhiteboardServlet: called doPost() method, params=" + whiteboardID + ", " + newParticipantUsername);

        JsonObject jsonResponse = Json.createObjectBuilder().build();

        AddParticipantStatus alreadyParticipantCheck = whiteboardEJB.isParticipant(newParticipantUsername, whiteboardID);

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
        }

        AddParticipantStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);

        switch (insertOutcome) {
            case SQL_SUCCESS:
                jsonResponse = Json.createObjectBuilder()
                        .add("success", true)
                        .add("message", newParticipantUsername + " has been added to this whiteboard!")
                        .build();
                request.setAttribute("newlyInsertedUsername", newParticipantUsername);
                response.setStatus(HttpServletResponse.SC_OK);
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
        }

        PrintWriter out = response.getWriter();
        out.print(jsonResponse);
        out.flush();
    }
}
