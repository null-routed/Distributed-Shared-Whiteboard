package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.servlets.WebSocketServerEndpoint;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.UserTransaction;
import jakarta.websocket.Session;

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
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("application/json");

        // Extracting parameters from the request
        String whiteboardID = Optional.ofNullable(request.getParameter("whiteboardID")).orElse("");
        String newParticipantUsername = Optional.ofNullable(request.getParameter("newParticipantUsername")).orElse("");
        String whiteboardOwner = Optional.ofNullable(request.getParameter("whiteboardOwner")).orElse("");
        String whiteboardName = Optional.ofNullable(request.getParameter("whiteboardName")).orElse("");

        System.out.println("@WhiteboardServlet: called doPost() method, params=" + whiteboardID + ", " + newParticipantUsername);

        JsonObject jsonResponse = Json.createObjectBuilder().build();

        ParticipantOperationStatus alreadyParticipantCheck = whiteboardEJB.isParticipant(newParticipantUsername, whiteboardID);

        if (alreadyParticipantCheck == ParticipantOperationStatus.ALREADY_PARTICIPATING) {
            jsonResponse = Json.createObjectBuilder()
                    .add("success", false)
                    .add("message", newParticipantUsername + " is already participating to this whiteboard.")
                    .build();
        } else if (alreadyParticipantCheck == ParticipantOperationStatus.OTHER_ERROR) {
            jsonResponse = Json.createObjectBuilder()
                    .add("success", false)
                    .add("message", "An error occurred. Try again or try in a few minutes.")
                    .build();
        } else {
            // Erlang + MySQL operations transaction
            try {
                userTransaction.begin();

                ParticipantOperationStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);

                switch (insertOutcome) {
                    case SQL_SUCCESS:
                        boolean erlangShareOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                                "share",
                                whiteboardID,
                                newParticipantUsername,
                                0
                        );

                        if (erlangShareOperationOutcome) {
                            userTransaction.commit();
                            jsonResponse = Json.createObjectBuilder()
                                    .add("success", true)
                                    .add("message", newParticipantUsername + " has been added to this whiteboard!")
                                    .build();
                        } else {
                            userTransaction.rollback();
                            jsonResponse = Json.createObjectBuilder()
                                    .add("success", false)
                                    .add("message", "An error occurred. Try again or try in a few minutes.")
                                    .build();
                        }
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
            } catch (Exception e) {
                try {
                    userTransaction.rollback();
                } catch (Exception ex) {
                    jsonResponse = Json.createObjectBuilder()
                            .add("success", false)
                            .add("message", "An error occurred. Try again or try in a few minutes.")
                            .build();
                }
                jsonResponse = Json.createObjectBuilder()
                        .add("success", false)
                        .add("message", "An error occurred. Try again or try in a few minutes.")
                        .build();
            }
        }

        // call to WebSockedEndpoint@sendUserMessage()
        JsonObject JSONMessage = Json.createObjectBuilder()
                .add("whiteboardName", whiteboardName)
                .add("whiteboardOwner", whiteboardOwner)
                .add("command", "share")
                .build();

        System.out.println("@ShareWhiteboardServlet: JSONMessage = " + JSONMessage);

        WebSocketServerEndpoint.sendMessageToUser(newParticipantUsername, JSONMessage);

        response.setStatus(HttpServletResponse.SC_OK);      // Always send success, JSP behavior is determined by the JSON response content
        PrintWriter out = response.getWriter();
        out.print(jsonResponse);
        out.flush();
    }

    /* ------ TEST METHOD FOR AJAX CALL ------ */
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
//        }
//
//        AddParticipantStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);
//
//        switch (insertOutcome) {
//            case SQL_SUCCESS:
//                jsonResponse = Json.createObjectBuilder()
//                        .add("success", true)
//                        .add("message", newParticipantUsername + " has been added to this whiteboard!")
//                        .build();
//                response.setStatus(HttpServletResponse.SC_OK);
//                break;
//            case UNREGISTERED_USER:
//                jsonResponse = Json.createObjectBuilder()
//                        .add("success", false)
//                        .add("message", "The username you have provided doesn't seem to belong to any user. Try again.")
//                        .build();
//                break;
//            case OTHER_ERROR:
//                jsonResponse = Json.createObjectBuilder()
//                        .add("success", false)
//                        .add("message", "An error occurred. Try again or try in a few minutes.")
//                        .build();
//                break;
//        }
//
//        PrintWriter out = response.getWriter();
//        out.print(jsonResponse);
//        out.flush();
//    }
}
