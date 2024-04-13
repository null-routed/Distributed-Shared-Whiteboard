package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
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

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Optional;


@WebServlet(name = "RemoveWhiteboardParticipantServlet", value = "/remove_participant")
public class RemoveWhiteboardParticipantServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @EJB
    private UserEJB userEJB;

    @Resource
    private UserTransaction userTransaction;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("application/json");

        String whiteboardID = Optional.ofNullable(request.getParameter("whiteboardID")).orElse("");
        String usernameToBeRemoved = Optional.ofNullable(request.getParameter("userToRemove")).orElse("");
        String whiteboardOwner = Optional.ofNullable(request.getParameter("whiteboardOwner")).orElse("");
        String whiteboardName = Optional.ofNullable(request.getParameter("whiteboardName")).orElse("");


        System.out.println("@RemoveWhiteboardParticipantServlet: called doPost() method, params=" + whiteboardID + ", " + usernameToBeRemoved);

        JsonObject jsonResponse = Json.createObjectBuilder().build();

        // Erlang + MySQL operations transaction
        try{
            userTransaction.begin();

            String userIDToBeRemoved = userEJB.getUserIdByUsername(usernameToBeRemoved);

            ParticipantOperationStatus mySQLParticipantRemovalOutcome = whiteboardEJB.removeParticipant(userIDToBeRemoved, whiteboardID);

            switch (mySQLParticipantRemovalOutcome) {
                case SQL_SUCCESS:
//                    boolean erlangParticipantRemovalOutcome = RPC.sendErlangWhiteboardUpdateRPC(
//                            "delete",
//                            whiteboardID,
//                            userIDToBeRemoved,
//                            null
//                    );

                    boolean erlangParticipantRemovalOutcome = true;

                    if (erlangParticipantRemovalOutcome) {
                        userTransaction.commit();
                        jsonResponse = Json.createObjectBuilder()
                                .add("success", true)
                                .add("message", usernameToBeRemoved + " has been removed from this whiteboard.")
                                .build();
                    } else {
                        userTransaction.rollback();
                        jsonResponse = Json.createObjectBuilder()
                                .add("success", false)
                                .add("message", "An error occurred. Try again or try in a few minutes.")
                                .build();
                    }
                    break;
                case OTHER_ERROR:
                    userTransaction.rollback();
                    jsonResponse = Json.createObjectBuilder()
                            .add("success", false)
                            .add("message", "An error occurred. Try again or try in a few minutes.")
                            .build();
                    break;
            }
        } catch (Exception e) {
            try {
                userTransaction.rollback();     // rollback if any exception occurs
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

        // call to WebSockedEndpoint@sendUserMessage()
        JsonObject JSONMessage = Json.createObjectBuilder()
                .add("whiteboardName", whiteboardName)
                .add("whiteboardOwner", whiteboardOwner)
                .add("command", "remove")
                .build();

        WebSocketServerEndpoint.sendMessageToUser(usernameToBeRemoved, JSONMessage);

        response.setStatus(HttpServletResponse.SC_OK);      // Always send success, JSP behavior is determined by the JSON response content
        PrintWriter out = response.getWriter();
        out.print(jsonResponse);
        out.flush();
    }
}
