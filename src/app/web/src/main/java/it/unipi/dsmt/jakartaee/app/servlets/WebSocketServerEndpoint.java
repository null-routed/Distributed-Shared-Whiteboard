package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.utility.JWT;
import jakarta.json.JsonObject;
import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;


@ServerEndpoint("/websocket")
public class WebSocketServerEndpoint {

    // Map to store sessions associated with identifiers (usernames)
    private static final Map<String, Session> sessionMap = new ConcurrentHashMap<>();

    @OnOpen
    public void onOpen(Session session, @PathParam("jwt") String jwt) {
        String username = Objects.requireNonNull(JWT.parseToken(jwt)).getSubject();
        sessionMap.put(username, session);          // associating the session to the JWT opening it
    }

    // Method to send a message to a specific user
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
