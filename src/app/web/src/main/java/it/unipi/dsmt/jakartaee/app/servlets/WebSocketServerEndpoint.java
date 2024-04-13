package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.utility.JWT;
import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;

import java.io.IOException;
import java.io.StringReader;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@ServerEndpoint("/websocket")
public class WebSocketServerEndpoint {

    // Map to store sessions associated with identifiers (e.g., usernames)
    private static final Map<String, Session> sessionMap = new ConcurrentHashMap<>();

    @OnOpen
    public void onOpen(Session session, @PathParam("jwt") String jwt) {
        System.out.println("@WebSocketServerEndpoint: called onOpen() method, jwt = " + jwt);
        String username = Objects.requireNonNull(JWT.parseToken(jwt)).getSubject();
        System.out.println("@WebSocketServerEndpoint: who has opened the socket: " + username);
        sessionMap.put(username, session);          // associating the session to the JWT opening it
    }

//    @OnMessage
//    public void onMessage(String message, Session session) {
//        System.out.println("Message received from client " + session.getUserProperties().get("username") + ": " + message);
//
//        // Parse JSON message
//        JsonObject jsonMessage = Json.createReader(new StringReader(message)).readObject();
//        String whiteboardName = jsonMessage.getString("whiteboardName");
//        String username = jsonMessage.getString("username");
//        String command = jsonMessage.getString("command");
//
//        String sender = (String) session.getUserProperties().get("username");
//
//        // Create a JSON object containing whiteboardId, sender
//        JsonObjectBuilder jsonBuilder = Json.createObjectBuilder()
//                .add("whiteboardName", whiteboardName)
//                .add("sender", sender)
//                .add("command", command);
//
//        JsonObject jsonToSend = jsonBuilder.build();
//        sendMessageToUser(username, jsonToSend);
//    }

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
    public static void sendMessageToUser(String username, JsonObject message) {
        System.out.println("@WebSocketServerEndpoint: called sendMessageToUser() method");
        System.out.println("@WebSocketServerEndpoint: jwt = " + username);

        Session targetUserSession = sessionMap.get(username);
        System.out.println("@WebSocketServerEndpoint: found target user session");
        if (targetUserSession.isOpen()) {
            System.out.println("@WebSocketServerEndpoint: target user session is open");
            try {
                targetUserSession.getBasicRemote().sendObject(message);
                System.out.println("sending message: " + message + "to " + username);
            } catch (Exception e) {
                System.out.println("Error sending message to user: " + e.getMessage());
            }
            return; // Exit loop once the session is found and message is sent
        }

        // If the session is not found or closed
        System.out.println("User session not found or closed: " + username);
    }
}
