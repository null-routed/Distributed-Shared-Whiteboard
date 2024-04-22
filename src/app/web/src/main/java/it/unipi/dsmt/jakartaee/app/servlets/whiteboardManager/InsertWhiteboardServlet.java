package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.*;

import java.io.IOException;

@WebServlet(name = "InsertWhiteboardServlet", value = "/insert_whiteboard")
public class InsertWhiteboardServlet extends BaseWhiteboardServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Resource
    private UserTransaction userTransaction;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null) return;

        response.setContentType("application/json");
        String whiteboardName = getParameter(request, "whiteboardName");
        if (whiteboardName.isEmpty()) {
            sendJsonResponse(response, false, "Missing parameters.");
            return;
        }

        String whiteboardDescription = getParameter(request, "whiteboardDescription");
        boolean isReadOnly = "true".equals(getParameter(request, "readOnly"));
        WhiteboardCreationDTO newWhiteboard = new WhiteboardCreationDTO(whiteboardName, whiteboardDescription, isReadOnly);

        try {
            userTransaction.begin();
            int whiteboardID = whiteboardEJB.addWhiteboard(loggedUserDTO.getId(), newWhiteboard);
            if (whiteboardID != -1) {
                boolean operationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                        "insert", Integer.toString(whiteboardID), loggedUserDTO.getUsername(), 1
                );
                if (operationOutcome) {
                    userTransaction.commit();
                    sendJsonResponse(response, true, String.valueOf(whiteboardID));
                    return;
                }
            }
            userTransaction.rollback();
            sendJsonResponse(response, false, "Failed to insert whiteboard.");
        } catch (Exception e) {
            try {
                userTransaction.rollback();
            } catch (Exception ex) {
                // Log error or handle secondary failure
            }
            sendJsonResponse(response, false, "Failed to insert whiteboard.");
        }
    }
}
