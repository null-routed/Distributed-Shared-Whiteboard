import { addMessage } from "./whiteboard-ui.js";
import {
  addNewWhiteboardToDOM,
  removeWhiteboardFromDOM,
} from "./homepage-ui.js";
let socket;

// Function to establish WebSocket connection if not already present
export function establishWebSocketConnection() {
  // Check if WebSocket connection already exists
  if (!socket || socket.readyState !== WebSocket.OPEN) {
    // Create new WebSocket connection only if not already present or if not in OPEN state
    const jwt = document.cookie
      .split("; ")
      .find((row) => row.startsWith("jwt="))
      .split("=")[1];
    socket = new WebSocket(`ws://localhost:8080/web/websocket?jwt=${jwt}`);

    // Function to handle WebSocket message event
    socket.onmessage = function (event) {
      handleReceivedMessage(event.data);
    };

    // Function to handle WebSocket close event
    socket.onclose = function (event) {
      setTimeout(establishWebSocketConnection, 2000); // Adjust delay as needed
    };

    // Function to handle WebSocket error event
    socket.onerror = function (event) {
      console.log("WebSocket error: " + event.data);
    };
  } else {
    console.log("WebSocket connection already exists.");
  }
}

function handleReceivedMessage(message) {
  // Parse the JSON message
  let parsedMessage;
  try {
    parsedMessage = JSON.parse(message);
  } catch (error) {
    console.error("Error parsing JSON message:", error);
    return; // Exit function if unable to parse JSON
  }

  // Check if the message contains the expected properties
  if (
    !parsedMessage ||
    !parsedMessage.whiteboardName ||
    !parsedMessage.whiteboardOwner ||
    !parsedMessage.command
  ) {
    console.error("Received message does not contain expected properties.");
    return; // Exit function if message format is invalid
  }
  const whiteboardName = parsedMessage.whiteboardName;
  const whiteboardOwner = parsedMessage.whiteboardOwner;
  const command = parsedMessage.command;
  const sender = parsedMessage.senderUser;
  //const currentUser = document.getElementById("self-username").value;
  let toastMessage = null;

  if (command === "share") {
    toastMessage = `${whiteboardOwner} has shared the whiteboard ${whiteboardName} with you`;
    // Check if the current page URL matches a certain pattern
    if (window.location.href.includes("/homepage"))
      // Call the functions only when on the homepage
      addNewWhiteboardToDOM(
        parsedMessage.whiteboardID,
        whiteboardName,
        whiteboardOwner,
        parsedMessage.whiteboardDescription,
      );
  } else if (command === "remove") {
    if (whiteboardOwner === sender) {
      toastMessage = `${whiteboardOwner} removed you from the whiteboard ${whiteboardName}`;
      if (window.location.href.includes("/homepage"))
        removeWhiteboardFromDOM(parsedMessage.whiteboardID);
    } else {
      toastMessage = `${sender} has left the whiteboard ${whiteboardName}`;
      $(`#${sender}-container`).remove();       // Removing the user from the participants list
    }
  }
  // show toast notification
  addMessage(toastMessage); // Call the addMessage function to display the toast
}
