package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.servlets.WebSocketServerEndpoint;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.ClientRedirector;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.servlet.ServletException;
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
public class DeleteWhiteboardServlet extends BaseWhiteboardServlet {

    @EJB
    private UserEJB userEJB;
    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Resource
    private UserTransaction userTransaction;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        System.out.println("@DeleteWhiteboardServlet: called doPost() method");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null)
            return;

        String whiteboardIdToDelete = getParameter(request,"whiteboardID");
        String userToRemove = getParameter(request, "username");
        if(whiteboardIdToDelete.isEmpty()){
            sendResponse(response, createJsonResponse(false, "Missing params."));
            return;
        }

        response.setContentType("application/json");
        if(userToRemove.isEmpty()) userToRemove = loggedUserDTO.getUsername();
        String currentUser = loggedUserDTO.getUsername();

        MinimalWhiteboardDTO whiteboardDTO = whiteboardEJB.getWhiteboardByID(Integer.parseInt(whiteboardIdToDelete));
        if((isNotOwner(whiteboardDTO, currentUser) && userToRemove.equals(currentUser)) || (!isNotOwner(whiteboardDTO, currentUser) && !userToRemove.equals(currentUser))){
            removeParticipant(request, response, loggedUserDTO, whiteboardDTO, userToRemove);
        } else if (!isNotOwner(whiteboardDTO, currentUser)){
            deleteWhiteboard(currentUser, request, response, whiteboardDTO);
        } else {
            sendResponse(response,
                    createJsonResponse(
                            false,
                            "Unauthorized."
                    ));
        }
    }

    private void deleteWhiteboard(String ownerUsername,
                                  HttpServletRequest request,
                                  HttpServletResponse response,
                                  MinimalWhiteboardDTO whiteboardDTO) throws IOException, ServletException {
        try {
            userTransaction.begin();
            boolean mySQLDeleteSuccessful = whiteboardEJB.deleteWhiteboard(String.valueOf(whiteboardDTO.getId()));
            if(!mySQLDeleteSuccessful) {
                userTransaction.rollback();
                sendResponse(response,
                        createJsonResponse(
                                false,
                                "An error occurred. Try again or try in a few minutes."
                        ));
                return;
            }

            boolean erlangDeleteSuccessful = RPC.sendErlangWhiteboardUpdateRPC(
                    "delete", String.valueOf(whiteboardDTO.getId()), "", 0);
            if(!erlangDeleteSuccessful) {
                userTransaction.rollback();
                sendResponse(
                        response,
                        createJsonResponse(
                                false,
                                "An error occurred. Try again or try in a few minutes."
                        ));
                return;
            }

            userTransaction.commit();
            List<String> participantsUsername = whiteboardEJB.getParticipantUsernames(whiteboardDTO.getId());
            sendUserRemovedMessage(ownerUsername, whiteboardDTO, participantsUsername);
            sendResponse(response,createJsonResponse(true,"Whiteboard " + whiteboardDTO.getName()+ " deleted successfully."));
        } catch (Exception e) {
            handleTransactionError(request, response);
        }
    }

    private void removeParticipant(
            HttpServletRequest request,
            HttpServletResponse response,
            LoggedUserDTO loggedUser,
            MinimalWhiteboardDTO whiteboardDTO,
            String usernameToBeRemoved) throws IOException, ServletException {
        try {
            userTransaction.begin();
            List<String> participantsUsername = whiteboardEJB.getParticipantUsernames(whiteboardDTO.getId());
            // write a code that checks if the usernameToBeRemoved is in participantsUsername
            if(!participantsUsername.contains(usernameToBeRemoved)) {
                sendResponse(response, createJsonResponse(false, "User not participating."));
            }

            String userId = userEJB.getUserIdByUsername(usernameToBeRemoved);
            ParticipantOperationStatus participantStatus =
                    whiteboardEJB.removeParticipant(userId, String.valueOf(whiteboardDTO.getId()));
            if(participantStatus != ParticipantOperationStatus.SQL_SUCCESS) {
                userTransaction.rollback();
                sendResponse(
                        response,
                        createJsonResponse(
                                false,
                                "An error occurred. Try again or try in a few minutes."
                        ));
                return;
            }
            boolean erlangUpdateSuccessful =
                    RPC.sendErlangWhiteboardUpdateRPC(
                            "removeParticipant",
                            String.valueOf(whiteboardDTO.getId()),
                            usernameToBeRemoved,
                            0);

            if (erlangUpdateSuccessful) {
                userTransaction.commit();
                sendUserRemovedMessage(loggedUser.getUsername(), whiteboardDTO, participantsUsername);
                if(usernameToBeRemoved.equals(loggedUser.getUsername())){
                    sendResponse(response,
                            createJsonResponse(true, "You left this whiteboard"));
                } else {
                    sendResponse(response,
                            createJsonResponse(true, "User " + usernameToBeRemoved + " has been removed"));
                }

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
            handleTransactionError(request, response);
        }
    }

    private void sendUserRemovedMessage(String currentUser, MinimalWhiteboardDTO whiteboardDTO, List<String> participantsUsername) {
        JsonObjectBuilder jsonObjectBuilder = Json.createObjectBuilder()
                .add("whiteboardID", whiteboardDTO.getId())
                .add("whiteboardName", whiteboardDTO.getName())
                .add("whiteboardOwner", whiteboardDTO.getOwner())
                .add("senderUser", currentUser)
                .add("command", "remove");

        JsonObject JSONMessage = jsonObjectBuilder.build();
        for(String username : participantsUsername) {
            if(!currentUser.equals(username))
                WebSocketServerEndpoint.sendMessageToUser(username, JSONMessage);
        }
    }

    private void handleTransactionError(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        try {
            userTransaction.rollback();
        } catch (Exception ex) {
            System.out.println("Error rolling back transaction: " + ex.getMessage());
        }
        sendResponse(
                response,
                createJsonResponse(
                        false,
                        "An error occurred. Try again or try in a few minutes."
                ));
    }
}
