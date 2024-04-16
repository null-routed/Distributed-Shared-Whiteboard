package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;
import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;

import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.servlets.WebSocketServerEndpoint;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.Base64;

@WebServlet(name = "ShareWhiteboardServlet", value = "/share_whiteboard")
public class ShareWhiteboardServlet extends BaseWhiteboardServlet {

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {

        LoggedUserDTO user = AccessController.getLoggedUserWithRedirect(request, response);

        response.setContentType("application/json");

        String whiteboardID = getParameter(request, "whiteboardID");
        String newParticipantUsername = getParameter(request, "newParticipantUsername");

        if(whiteboardID.isEmpty() || newParticipantUsername.isEmpty() || user == null) {
            sendResponse(response, createJsonResponse(false, "missing parameters"), HttpServletResponse.SC_BAD_REQUEST);
            return;
        }

        MinimalWhiteboardDTO whiteboardDTO = whiteboardEJB.getWhiteboardByID(Integer.parseInt(whiteboardID));

        if(checkOwnership(whiteboardDTO, user.getUsername())) {
            sendResponse(response, createJsonResponse(false, "Forbidden"), HttpServletResponse.SC_FORBIDDEN);
            return;
        }

        System.out.println("@WhiteboardServlet: called doPost() method, params=" + whiteboardID + ", " + newParticipantUsername);

        JsonObject jsonResponse = handleRequest(whiteboardDTO, newParticipantUsername);

        sendResponse(response, jsonResponse);
    }

    private JsonObject handleRequest(MinimalWhiteboardDTO whiteboardDTO, String newParticipantUsername) {
        ParticipantOperationStatus status = whiteboardEJB.isParticipant(
                newParticipantUsername, String.valueOf(whiteboardDTO.getId()));

        switch (status) {
            case ALREADY_PARTICIPATING:
            case OTHER_ERROR:
                return createJsonResponse(false, status == ParticipantOperationStatus.ALREADY_PARTICIPATING ?
                        newParticipantUsername + " is already participating to this whiteboard." :
                        "An error occurred. Try again or try in a few minutes.");
            default:
                return processTransaction(String.valueOf(whiteboardDTO.getId()), newParticipantUsername);
        }
    }

    private JsonObject processTransaction(String whiteboardID, String newParticipantUsername) {
        try {
            userTransaction.begin();
            ParticipantOperationStatus insertOutcome = whiteboardEJB.addParticipant(newParticipantUsername, whiteboardID);
            MinimalWhiteboardDTO whiteboardDTO = whiteboardEJB.getWhiteboardByID(Integer.parseInt(whiteboardID));

            if (insertOutcome == ParticipantOperationStatus.SQL_SUCCESS) {
                return handleSuccess(whiteboardDTO, newParticipantUsername);
            } else {
                userTransaction.rollback();
                return createJsonResponse(false, getErrorMessage(insertOutcome));
            }
        } catch (Exception e) {
            rollbackTransaction();
            return createJsonResponse(false, "An error occurred during transaction. Try again.");
        }
    }

    private JsonObject handleSuccess(MinimalWhiteboardDTO whiteboardDTO, String newParticipantUsername) throws Exception {
        boolean erlangShareOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                "insert", String.valueOf(whiteboardDTO.getId()),
                newParticipantUsername, whiteboardDTO.isReadOnly() ? 0 : 1);

        if (erlangShareOperationOutcome) {
            userTransaction.commit();
            byte[] snapshotData = whiteboardEJB.getSnapshotByWhiteboardID(String.valueOf(whiteboardDTO.getId()));
            sendWebSocketMessage(newParticipantUsername, whiteboardDTO, snapshotData);
            return createJsonResponse(true, newParticipantUsername + " has been added to this whiteboard!");
        } else {
            userTransaction.rollback();
            return createJsonResponse(false, "Erlang communication failed. Try again.");
        }
    }

    private void rollbackTransaction() {
        try {
            userTransaction.rollback();
        } catch (Exception ex) {
            System.err.println("Error rolling back transaction: " + ex.getMessage());
        }
    }

    private String getErrorMessage(ParticipantOperationStatus status) {
        switch (status) {
            case UNREGISTERED_USER:
                return "The username you have provided doesn't seem to belong to any user. Try again.";
            case OTHER_ERROR:
            default:
                return "An error occurred. Try again or try in a few minutes.";
        }
    }

    private void sendWebSocketMessage(String username, MinimalWhiteboardDTO whiteboardDTO, byte[] snapshotData) {
        String base64EncodedSnapshot = Base64.getEncoder().encodeToString(snapshotData);
        JsonObject message = Json.createObjectBuilder()
                .add("whiteboardName", whiteboardDTO.getName())
                .add("whiteboardOwner", whiteboardDTO.getOwner())
                .add("whiteboardSnapshot", base64EncodedSnapshot)
                .add("whiteboardID", whiteboardDTO.getId())
                .add("whiteboardDescription", whiteboardDTO.getDescription())
                .add("whiteboardReadOnly", whiteboardDTO.isReadOnly())
                .add("command", "share")
                .build();

        WebSocketServerEndpoint.sendMessageToUser(username, message);
        System.out.println("@ShareWhiteboardServlet: WebSocket message sent to " + username + ": " + message);
    }
}
