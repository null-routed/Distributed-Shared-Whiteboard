package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.utility.JWT;
import jakarta.json.JsonObject;
import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;


/**
 * WebSocket server endpoint for handling WebSocket connections.
 */
@ServerEndpoint("/websocket")
public class WebSocketServerEndpoint {

    private static final Map<String, Session> sessionMap = new ConcurrentHashMap<>();   // Map to store sessions associated with identifiers (usernames)

    /**
     * Called when a new WebSocket connection is opened.
     * Associates the session with the provided JWT username.
     * @param session The WebSocket session
     * @param jwt The JWT token containing the username
     */
    @OnOpen
    public void onOpen(Session session, @PathParam("jwt") String jwt) {
        String username = Objects.requireNonNull(JWT.parseToken(jwt)).getSubject();
        sessionMap.put(username, session);          // associating the session to the JWT opening it
    }

    /**
     * Sends a message to a specific user.
     * @param username The username of the recipient
     * @param message The message to be sent
     */
    public static void sendMessageToUser(String username, JsonObject message) {
        Session targetUserSession = sessionMap.get(username);

        try {
            if (targetUserSession.isOpen()) {
                try {
                    targetUserSession.getBasicRemote().sendObject(message);
                } catch (Exception e) {
                    System.err.println("Error sending message to '" + username + "': " + e.getMessage());
                }
            }
        } catch (NullPointerException e) {
            System.err.println("@WebSocketEndpoint: '" + username + "' is offline at the moment or the websocket has been closed.");
        }
    }
}
