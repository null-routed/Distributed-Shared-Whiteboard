import {addMessage} from "./whiteboard-ui.js";

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

    console.log("My JWT: " + jwt)

    // Function to handle WebSocket open event
    socket.onopen = function (event) {
      console.log("Notification webSocket connection established.");
    };

    // Function to handle WebSocket message event
    socket.onmessage = function (event) {
      console.log("Message received from server: " + event.data);
      handleReceivedMessage(event.data);
    };

    // Function to handle WebSocket close event
    socket.onclose = function (event) {
      console.log("WebSocket connection closed. Reconnecting...");
      // Automatically attempt to reconnect after a short delay
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

    // Extract relevant data from the parsed message
    const whiteboardId = parsedMessage.whiteboardName;
    const whiteboardOwner = parsedMessage.whiteboardOwner;
    const command = parsedMessage.command;

    // show toast notification
    const toastMessage = (command === "share") ?
        `${whiteboardOwner} has shared the whiteboard ${whiteboardId} with you` :
        `${whiteboardOwner} removed you from the whiteboard ${whiteboardId}`;

    addMessage(toastMessage); // Call the addMessage function to display the toast

    // Reload the page when the toast is clicked
    const toastElement = document.getElementById('toast-container').lastElementChild; // Assuming 'toastContainer' is the id of your toast container
    toastElement.addEventListener("click", function () {
      window.location.reload();
    });
    toastElement.addEventListener("hidden.bs.toast", function () {
      window.location.reload();
    });
  }
