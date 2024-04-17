package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.servlets.WebSocketServerEndpoint;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.UserTransaction;

import java.io.IOException;

@WebServlet(name = "RemoveWhiteboardParticipantServlet", value = "/remove_participant")
public class RemoveWhiteboardParticipantServlet extends BaseWhiteboardServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @EJB
    private UserEJB userEJB;

    @Resource
    private UserTransaction userTransaction;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        LoggedUserDTO user = AccessController.getLoggedUserWithRedirect(request, response);

        String whiteboardID = getParameter(request, "whiteboardID");
        String usernameToBeRemoved = getParameter(request, "userToRemove");

        if (whiteboardID.isEmpty() || usernameToBeRemoved.isEmpty() || user == null) {
            sendResponse(
                    response,
                    createJsonResponse(false, "missing parameters")
            );
            return;
        }

        MinimalWhiteboardDTO whiteboardDTO = whiteboardEJB.getWhiteboardByID(Integer.parseInt(whiteboardID));

        if(isNotOwner(whiteboardDTO, user.getUsername())) {
            sendResponse(response, createJsonResponse(false, "Forbidden"));
            return;
        }

        removeParticipant(response, whiteboardDTO, usernameToBeRemoved);
    }

    private void removeParticipant(
            HttpServletResponse response,
            MinimalWhiteboardDTO whiteboardDTO,
            String usernameToBeRemoved) throws IOException {
        try {
            userTransaction.begin();
            if (removeParticipantFromWhiteboard(usernameToBeRemoved, String.valueOf(whiteboardDTO.getId()))) {
                userTransaction.commit();
                sendUserRemovedMessage(usernameToBeRemoved, whiteboardDTO);
                sendResponse(
                        response,
                        createJsonResponse(true, usernameToBeRemoved + " has been removed from this whiteboard.")
                );
            } else {
                userTransaction.rollback();
                sendResponse(
                        response,
                        createJsonResponse(
                                false,
                                "An error occurred. Try again or try in a few minutes."
                        ));
            }

        } catch (Exception e) {
            try {
                userTransaction.rollback();
            } catch (Exception ex) {
                System.out.println("Error rolling back transaction: " + ex.getMessage());
            }
            sendResponse(response, createJsonResponse(
                    false,
                    e.toString()));
        }
    }

    private boolean removeParticipantFromWhiteboard(String usernameToBeRemoved, String whiteboardID) {
        ParticipantOperationStatus status = whiteboardEJB.isParticipant(usernameToBeRemoved, whiteboardID);
        if(ParticipantOperationStatus.NOT_PARTICIPATING.equals(status)) return false;

        String userId = userEJB.getUserIdByUsername(usernameToBeRemoved);
        return RPC.sendErlangWhiteboardUpdateRPC("removeParticipant", whiteboardID, usernameToBeRemoved, 0)
                && whiteboardEJB.removeParticipant(userId, whiteboardID).equals(ParticipantOperationStatus.SQL_SUCCESS);
    }

    private void sendUserRemovedMessage(String usernameToBeRemoved, MinimalWhiteboardDTO whiteboardDTO) {
        JsonObject JSONMessage = Json.createObjectBuilder()
                .add("whiteboardID", whiteboardDTO.getId())
                .add("whiteboardOwner", whiteboardDTO.getOwner())
                .add("whiteboardName", whiteboardDTO.getName())
                .add("command", "remove")
                .build();
        
        WebSocketServerEndpoint.sendMessageToUser(usernameToBeRemoved, JSONMessage);
    }
}
