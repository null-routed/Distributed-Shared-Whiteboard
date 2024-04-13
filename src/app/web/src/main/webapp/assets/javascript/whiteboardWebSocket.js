// Function to establish WebSocket connection if not already present
let socket;
function establishWebSocketConnection(username) {
  // Check if WebSocket connection already exists
  if (!socket || socket.readyState !== WebSocket.OPEN) {
    // Create new WebSocket connection only if not already present or if not in OPEN state
    socket = new WebSocket(
      `ws://localhost:8080/web/websocket?username=${username}`
    );

    // Function to handle WebSocket open event
    socket.onopen = function (event) {
      console.log("WebSocket connection established.");
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

// Function to send a message to the WebSocket server
function sendMessageToWebSocket(whiteboardName, username, command) {
  if (socket && socket.readyState === WebSocket.OPEN) {
    const messageData = {
      whiteboardName: whiteboardName,
      username: username,
      command: command,
    };
    const jsonMessage = JSON.stringify(messageData);
    socket.send(jsonMessage);
    console.log("Message sent to server: " + messageData);
  } else {
    console.log("WebSocket connection is not open. Message not sent.");
  }
}

// Custom function to handle the received message
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
    !parsedMessage.sender ||
    !parsedMessage.command
  ) {
    console.error("Received message does not contain expected properties.");
    return; // Exit function if message format is invalid
  }

  // Extract relevant data from the parsed message
  const whiteboardId = parsedMessage.whiteboardName;
  const sender = parsedMessage.sender;
  const command = parsedMessage.command;

  // Example: spawn a custom modal
  let modalMessage = null;
  if (command === "share")
    modalMessage = `${sender} has shared the whiteboard ${whiteboardId} with you`;
  else
    modalMessage = `${sender} removed you from the whiteboard ${whiteboardId}`;
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
      !parsedMessage.sender ||
      !parsedMessage.command
    ) {
      console.error("Received message does not contain expected properties.");
      return; // Exit function if message format is invalid
    }

    // Extract relevant data from the parsed message
    const whiteboardId = parsedMessage.whiteboardName;
    const sender = parsedMessage.sender;
    const command = parsedMessage.command;

    // Example: spawn a custom modal
    let modalMessage = null;
    if (command === "share")
      modalMessage = `${sender} has shared the whiteboard ${whiteboardId} with you`;
    else
      modalMessage = `${sender} removed you from the whiteboard ${whiteboardId}`;
    const modal = document.createElement("div");
    modal.innerHTML = modalMessage;
    modal.style.position = "fixed";
    modal.style.top = "50%";
    modal.style.left = "50%";
    modal.style.transform = "translate(-50%, -50%)";
    modal.style.backgroundColor = "white";
    modal.style.padding = "20px";
    modal.style.border = "1px solid black";
    modal.style.zIndex = "9999";
    document.body.appendChild(modal);

    // Add an event listener to the modal for reloading the page when clicked
    modal.addEventListener("click", function () {
      window.location.reload();
    });
  }
}
