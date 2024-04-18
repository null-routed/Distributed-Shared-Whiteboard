package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.servlets.WebSocketServerEndpoint;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.UserTransaction;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


@WebServlet (name = "DeleteWhiteboardServlet", value = "/delete_whiteboard")
public class DeleteWhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Resource
    private UserTransaction userTransaction;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("@DeleteWhiteboardServlet: called doPost() method");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null)
            return;

        String whiteboardIdToDelete = request.getParameter("whiteboardIdToDelete");

        if (whiteboardIdToDelete != null) {

            String currentUser = loggedUserDTO.getUsername();
            MinimalWhiteboardDTO whiteboardDTO = whiteboardEJB.getWhiteboardByID(Integer.parseInt(whiteboardIdToDelete));

            //boolean clientIsOwner = whiteboardEJB.isWhiteboardOwner(loggedUserDTO.getId(), whiteboardIdToDelete);

            if (/*clientIsOwner*/ Objects.equals(currentUser, whiteboardDTO.getOwner())) {// Erlang + MySQL operations transaction if the requester is the whiteboard owner

                try {
                    userTransaction.begin();

                    List<String> participantsUsername = whiteboardEJB.getParticipantUsernames(whiteboardDTO.getId());
                    boolean mySQLDeleteOperationOutcome = whiteboardEJB.deleteWhiteboard(whiteboardIdToDelete);

                    if (mySQLDeleteOperationOutcome) {
                        boolean erlangDeleteOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                                "delete",
                                whiteboardIdToDelete,
                                "",       // not needed Erlang side
                                0           // not needed Erlang side
                        );

                        if (erlangDeleteOperationOutcome) {
                            userTransaction.commit();
                            sendUserRemovedMessage(currentUser, whiteboardDTO, participantsUsername);
                            response.sendRedirect(request.getContextPath() + "/homepage");
                        } else {
                            userTransaction.rollback();// rollback if Erlang operation fails
                            response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                        }
                    } else {
                        userTransaction.rollback();         // rollback if MySQL operation fails
                        response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                    }
                } catch (Exception e) {
                    try {
                        userTransaction.rollback();     // rollback if any exception occurs
                    } catch (Exception ex) {
                        throw new RuntimeException(ex);
                    }
                    throw new RuntimeException(e);
                }

                return;     // Return to prevent further processing, the op has concluded for the owner of the whiteboard
            }

            // If here, the user requesting a DELETE is not the owner -> remove him from the whiteboard participants
            try{
                userTransaction.begin();

                ParticipantOperationStatus mySQLParticipantRemovalOutcome = whiteboardEJB.removeParticipant(loggedUserDTO.getId(), whiteboardIdToDelete);

                if (mySQLParticipantRemovalOutcome == ParticipantOperationStatus.SQL_SUCCESS) {
                    boolean erlangParticipantRemovalOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                            "delete",
                            whiteboardIdToDelete,
                            loggedUserDTO.getUsername(),        // user to be removed from the whiteboard participants list
                            0            // not needed Erlang side
                    );

                    if (erlangParticipantRemovalOutcome) {
                        userTransaction.commit();
                        sendUserRemovedMessage(currentUser, whiteboardDTO, null);
                        response.sendRedirect(request.getContextPath() + "/homepage");
                    } else {
                        userTransaction.rollback();     // rollback if Erlang operation failed
                        response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                    }
                } else {
                    userTransaction.rollback();     // rollback if MySQL operation failed
                    response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                }
            } catch (Exception e) {
                try {
                    userTransaction.rollback();     // rollback if any exception occurs
                } catch (Exception ex) {
                    // throw new RuntimeException(ex);
                    response.sendRedirect(request.getContextPath() + "/homepage?insertionFailed=true");
                }
                // throw new RuntimeException(e);
                response.sendRedirect(request.getContextPath() + "/homepage?insertionFailed=true");

            }
        }
    }
    private void sendUserRemovedMessage(String currentUser, MinimalWhiteboardDTO whiteboardDTO, List<String> participantsUsername) {
        JsonObjectBuilder jsonObjectBuilder = Json.createObjectBuilder()
                .add("whiteboardID", whiteboardDTO.getId())
                .add("whiteboardName", whiteboardDTO.getName())
                .add("command", "delete");

        if(!Objects.equals(currentUser, whiteboardDTO.getOwner())) {
            jsonObjectBuilder.add("whiteboardOwner", currentUser);
            JsonObject JSONMessage = jsonObjectBuilder.build();
            WebSocketServerEndpoint.sendMessageToUser(whiteboardDTO.getOwner(), JSONMessage);
        } else {
            jsonObjectBuilder.add("whiteboardOwner", whiteboardDTO.getOwner());
            JsonObject JSONMessage = jsonObjectBuilder.build();
            for(String username : participantsUsername) {
                if(!Objects.equals(username, whiteboardDTO.getOwner()))
                    WebSocketServerEndpoint.sendMessageToUser(username, JSONMessage);
            }
        }
    }
}
