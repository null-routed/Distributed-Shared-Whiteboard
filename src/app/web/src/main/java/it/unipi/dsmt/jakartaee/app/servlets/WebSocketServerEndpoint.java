package it.unipi.dsmt.jakartaee.app.servlets;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;

import java.io.IOException;
import java.io.StringReader;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@ServerEndpoint("/websocket")
public class WebSocketServerEndpoint {

    // Map to store sessions associated with identifiers (e.g., usernames)
    // private static final Map<String, Session> sessionMap = new ConcurrentHashMap<>();

    @OnOpen
    public void onOpen(Session session, @PathParam("username") String username) {
        session.getUserProperties().put("username", username);
        System.out.println("WebSocket connection opened: " + session.getUserProperties().get("username"));
    }

    @OnMessage
    public void onMessage(String message, Session session) {
        System.out.println("Message received from client " + session.getUserProperties().get("username") + ": " + message);

        // Parse JSON message
        JsonObject jsonMessage = Json.createReader(new StringReader(message)).readObject();
        String whiteboardName = jsonMessage.getString("whiteboardName");
        String username = jsonMessage.getString("username");
        String command = jsonMessage.getString("command");

        String sender = (String) session.getUserProperties().get("username");

        // Create a JSON object containing whiteboardId, sender
        JsonObjectBuilder jsonBuilder = Json.createObjectBuilder()
                .add("whiteboardName", whiteboardName)
                .add("sender", sender)
                .add("command", command);

        JsonObject jsonToSend = jsonBuilder.build();
        sendMessageToUser(username, jsonToSend, session);
    }

    @OnClose
    public void onClose(Session session) {
        // Handle closed connections
    }

    @OnError
    public void onError(Session session, Throwable throwable) {
        // Handle errors
    }

    // Method to extract username from session (customize based on your application's logic)
    private String extractUsernameFromSession(Session session) {
        return (String) session.getUserProperties().get("username");
    }

    // Method to send a message to a specific user
    private void sendMessageToUser(String username, JsonObject message, Session clientSession) {
        // Get all active sessions
        Set<Session> sessions = clientSession.getOpenSessions();

        // Iterate through sessions to find the one associated with the username
        for (Session session : sessions) {
            String sessionUsername = (String) session.getUserProperties().get("username");
            if (sessionUsername != null && sessionUsername.equals(username)) {
                if (session.isOpen()) {
                    try {
                        session.getBasicRemote().sendObject(message);
                        System.out.println("sending message: " + message + "to " + username);
                    } catch (Exception e) {
                        System.out.println("Error sending message to user: " + e.getMessage());
                    }
                    return; // Exit loop once the session is found and message is sent
                }
            }
        }
        // If the session is not found or closed
        System.out.println("User session not found or closed: " + username);
    }
}
