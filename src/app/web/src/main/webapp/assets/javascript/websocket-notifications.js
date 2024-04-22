import {
  addToast,
  removeParticipantFromDOM,
  addParticipantToDOM} from "./whiteboard-ui.js";
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
    return; 
  }

  // Validate the necessary properties
  if (!parsedMessage || !parsedMessage.whiteboardName || !parsedMessage.targetUser || !parsedMessage.command) {
    console.error("Received message does not contain expected properties:", parsedMessage);
    return; 
  }

  const { whiteboardName, targetUser, command, senderUser: sender, whiteboardID, whiteboardDescription } = parsedMessage;
  const selfUsername = $("#self-username").val();
  const isHomepage = window.location.href.includes("/homepage");
  const currWhiteboardID = parseInt(new URLSearchParams(window.location.search).get('whiteboardID'));
  // Process commands
  switch (command) {
    case "share":
      if (targetUser === selfUsername || !targetUser) {
        addToast(`${sender} has shared the whiteboard ${whiteboardName} with you`);
        if (isHomepage) {
          addNewWhiteboardToDOM(whiteboardID, whiteboardName, whiteboardDescription);
        }
      } else {
        addToast(`${sender} has shared the whiteboard ${whiteboardName} with ${targetUser}`);
        if(whiteboardID === currWhiteboardID){
          addParticipantToDOM(targetUser);
        }
      }
      break;
    case "remove":
      if (targetUser === selfUsername) {
        addToast(`${sender} removed you from the whiteboard ${whiteboardName}`);
        if (isHomepage) {
          removeWhiteboardFromDOM(whiteboardID);
        }
      } else {
        addToast(`${targetUser} has left the whiteboard ${whiteboardName}`);
        if(whiteboardID === currWhiteboardID){
          removeParticipantFromDOM(targetUser);
        }
      }
      break;
    default:
      console.error("Unsupported command:", command);
  }
}

