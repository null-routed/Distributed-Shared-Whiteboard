package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.UserTransaction;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Optional;


/**
 * Abstract base servlet for handling common whiteboard-related operations.
 */
public abstract class BaseWhiteboardServlet extends HttpServlet {

    @EJB
    protected WhiteboardEJB whiteboardEJB;

    @Resource
    protected UserTransaction userTransaction;

    /**
     * Creates a JSON response object with a success status and message.
     * @param success boolean indicating success or failure
     * @param message the message to include in the response
     * @return JsonObject representing the response
     */
    protected JsonObject createJsonResponse(boolean success, String message) {
        return Json.createObjectBuilder()
                .add("success", success)
                .add("message", message)
                .build();
    }

    /**
     * Sends a JSON response to the client.
     * @param response HttpServletResponse instance
     * @param jsonResponse JsonObject representing the response
     * @throws IOException if an I/O error occurs during response writing
     */
    protected void sendResponse(HttpServletResponse response, JsonObject jsonResponse) throws IOException {
        response.setStatus(HttpServletResponse.SC_OK);
        try (PrintWriter out = response.getWriter()) {
            out.print(jsonResponse);
            out.flush();
        }
    }

    /**
     * Retrieves a request parameter by name.
     * @param request HttpServletRequest instance
     * @param paramName the name of the parameter to retrieve
     * @return String the value of the parameter, or an empty string if not found
     */
    protected String getParameter(HttpServletRequest request, String paramName) {
        return Optional.ofNullable(request.getParameter(paramName)).orElse("");
    }

    /**
     * Checks if the user is not the owner of the whiteboard.
     * @param whiteboardDTO the whiteboard data transfer object
     * @param username the username to check
     * @return boolean true if the user is not the owner, false otherwise
     */
    protected boolean isNotOwner(MinimalWhiteboardDTO whiteboardDTO, String username) {
        return !whiteboardDTO.getOwner().equals(username);
    }

    /**
     * Creates and sends a JSON response to the client.
     * @param response HttpServletResponse instance
     * @param success boolean indicating success or failure
     * @param message the message to include in the response
     * @throws IOException if an I/O error occurs during response writing
     */
    protected void sendJsonResponse(HttpServletResponse response, boolean success, String message) throws IOException {
        sendResponse(response, createJsonResponse(success, message));
    }
}
