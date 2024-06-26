package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.servlets.WebSocketServerEndpoint;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.json.JsonObjectBuilder;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.UserTransaction;
import jakarta.ejb.EJB;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.annotation.Resource;
import java.io.IOException;
import java.util.List;


/**
 * Servlet for handling requests to delete a whiteboard.
 */
@WebServlet(name = "DeleteWhiteboardServlet", value = "/delete_whiteboard")
public class DeleteWhiteboardServlet extends BaseWhiteboardServlet {

    @EJB
    private UserEJB userEJB;
    @Resource
    private UserTransaction userTransaction;

    /**
     * Handles POST requests to delete a whiteboard.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if an I/O error occurs during request processing
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null) return;

        String whiteboardIdToDelete = getParameter(request, "whiteboardID");
        if (whiteboardIdToDelete.isEmpty()) {
            sendJsonResponse(response, false, "Missing parameters.");
            return;
        }

        response.setContentType("application/json");
        String currentUser = loggedUserDTO.getUsername();
        String userParam = getParameter(request, "username");
        String userToRemove = !userParam.isEmpty() ? userParam : currentUser;

        MinimalWhiteboardDTO whiteboardDTO = whiteboardEJB.getWhiteboardByID(Integer.parseInt(whiteboardIdToDelete));
        if ((isNotOwner(whiteboardDTO, currentUser) && userToRemove.equals(currentUser)) || (!isNotOwner(whiteboardDTO, currentUser) && !userToRemove.equals(currentUser)))
            removeParticipant(response, whiteboardDTO, userToRemove, currentUser);
        else if (!isNotOwner(whiteboardDTO, currentUser))
            deleteWhiteboard(response, whiteboardDTO, currentUser);
        else
            sendJsonResponse(response, false, "Unauthorized access.");
    }

    /**
     * Deletes the whiteboard.
     * @param response HttpServletResponse instance
     * @param whiteboardDTO MinimalWhiteboardDTO instance representing the whiteboard to delete
     * @param currentUser the current user
     * @throws IOException if an I/O error occurs during response sending
     */
    private void deleteWhiteboard(HttpServletResponse response, MinimalWhiteboardDTO whiteboardDTO, String currentUser) throws IOException {
        try {
            userTransaction.begin();
            List<String> participantsUsername = whiteboardEJB.getParticipantUsernames(whiteboardDTO.getId());
            if (!whiteboardEJB.deleteWhiteboard(String.valueOf(whiteboardDTO.getId())) ||
                    !RPC.sendErlangWhiteboardUpdateRPC("delete", String.valueOf(whiteboardDTO.getId()), "", 0)) {
                userTransaction.rollback();
                sendJsonResponse(response, false, "Failed to delete whiteboard.");
                return;
            }
            userTransaction.commit();
            sendUserRemovedMessage(currentUser, whiteboardDTO, null, participantsUsername);
            sendJsonResponse(response, true, "Whiteboard deleted successfully.");
        } catch (Exception e) {
            handleTransactionError(response, e);
        }
    }

    /**
     * Removes a participant from the whiteboard.
     * @param response HttpServletResponse instance
     * @param whiteboardDTO MinimalWhiteboardDTO instance representing the whiteboard
     * @param usernameToBeRemoved the username of the participant to be removed
     * @param currentUser the current user
     * @throws IOException if an I/O error occurs during response sending
     */
    private void removeParticipant(HttpServletResponse response, MinimalWhiteboardDTO whiteboardDTO, String usernameToBeRemoved, String currentUser) throws IOException {
        try {
            userTransaction.begin();
            List<String> participantsUsername = whiteboardEJB.getParticipantUsernames(whiteboardDTO.getId());
            if (!participantsUsername.contains(usernameToBeRemoved)) {
                sendJsonResponse(response, false, "User not participating.");
                return;
            }
            if (whiteboardEJB.removeParticipant(userEJB.getUserIdByUsername(usernameToBeRemoved), String.valueOf(whiteboardDTO.getId())) != ParticipantOperationStatus.SQL_SUCCESS ||
                    !RPC.sendErlangWhiteboardUpdateRPC("removeParticipant", String.valueOf(whiteboardDTO.getId()), usernameToBeRemoved, 0)) {
                userTransaction.rollback();
                sendJsonResponse(response, false, "Failed to remove participant.");
                return;
            }
            userTransaction.commit();
            sendUserRemovedMessage(currentUser, whiteboardDTO, usernameToBeRemoved, participantsUsername);
            String message = usernameToBeRemoved.equals(currentUser) ?
                    "Whiteboard " + whiteboardDTO.getName() + " left successfully." : "Participant removed successfully.";
            sendJsonResponse(response, true, message);
        } catch (Exception e) {
            handleTransactionError(response, e);
        }
    }

    /**
     * Sends a message to notify users of a removed user.
     * @param currentUser the current user
     * @param whiteboardDTO MinimalWhiteboardDTO instance representing the whiteboard
     * @param removedUser the username of the removed user, null if the whiteboard was deleted
     * @param participantsUsername list of usernames of participants
     */
    private void sendUserRemovedMessage(String currentUser, MinimalWhiteboardDTO whiteboardDTO, String removedUser, List<String> participantsUsername) {
        if(participantsUsername == null) return;
        JsonObjectBuilder builder = Json.createObjectBuilder()
                .add("whiteboardID", whiteboardDTO.getId())
                .add("whiteboardName", whiteboardDTO.getName())
                .add("senderUser", currentUser)
                .add("command", "remove");
        if (removedUser == null) {
            builder.addNull("targetUser");
        } else
            builder.add("targetUser", removedUser);

        JsonObject message = builder.build();
        for (String username : participantsUsername) {
            if (!username.equals(currentUser))
                WebSocketServerEndpoint.sendMessageToUser(username, message);
        }
    }

    /**
     * Handles transaction errors.
     * @param response HttpServletResponse instance
     * @param e the Exception thrown
     * @throws IOException if an I/O error occurs during response sending
     */
    private void handleTransactionError(HttpServletResponse response, Exception e) throws IOException {
        try {
            userTransaction.rollback();
        } catch (Exception ex) {
            System.out.println("Error rolling back transaction: " + ex.getMessage());
        }
        sendJsonResponse(response, false, "An error occurred: " + e.getMessage());
    }
}
