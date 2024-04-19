package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.servlets.WebSocketServerEndpoint;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.servlet.ServletException;
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

@WebServlet(name = "DeleteWhiteboardServlet", value = "/delete_whiteboard")
public class DeleteWhiteboardServlet extends BaseWhiteboardServlet {

    @EJB
    private UserEJB userEJB;
    @Resource
    private UserTransaction userTransaction;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
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
        if ((isNotOwner(whiteboardDTO, currentUser) && userToRemove.equals(currentUser)) || (!isNotOwner(whiteboardDTO, currentUser) && !userToRemove.equals(currentUser))) {
            removeParticipant(response, whiteboardDTO, userToRemove, currentUser);
        } else if (!isNotOwner(whiteboardDTO, currentUser)) {
            deleteWhiteboard(response, whiteboardDTO, currentUser);
        } else {
            sendJsonResponse(response, false, "Unauthorized access.");
        }
    }

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
            sendUserRemovedMessage(currentUser, whiteboardDTO, participantsUsername);
            sendJsonResponse(response, true, "Whiteboard deleted successfully.");
        } catch (Exception e) {
            handleTransactionError(response, e);
        }
    }

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
            sendUserRemovedMessage(currentUser, whiteboardDTO, participantsUsername);
            String message = usernameToBeRemoved.equals(currentUser) ?
                    "Whiteboard " + whiteboardDTO.getName() + " left successfully." : "Participant removed successfully.";
            sendJsonResponse(response, true, message);
        } catch (Exception e) {
            handleTransactionError(response, e);
        }
    }

    private void sendUserRemovedMessage(String currentUser, MinimalWhiteboardDTO whiteboardDTO, List<String> participantsUsername) {
        if(participantsUsername == null) return;
        JsonObject message = Json.createObjectBuilder()
                .add("whiteboardID", whiteboardDTO.getId())
                .add("whiteboardName", whiteboardDTO.getName())
                .add("whiteboardOwner", whiteboardDTO.getOwner())
                .add("senderUser", currentUser)
                .add("command", "remove")
                .build();
        for (String username : participantsUsername) {
            if (!username.equals(currentUser))
                WebSocketServerEndpoint.sendMessageToUser(username, message);
        }
    }

    private void handleTransactionError(HttpServletResponse response, Exception e) throws IOException {
        try {
            userTransaction.rollback();
        } catch (Exception ex) {
            System.out.println("Error rolling back transaction: " + ex.getMessage());
        }
        sendJsonResponse(response, false, "An error occurred: " + e.getMessage());
    }


}
