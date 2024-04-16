// Function to establish WebSocket connection if not already present
let socket;

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

    // Example: spawn a custom modal
    let modalMessage = null;
    if (command === "share")
      modalMessage = `${whiteboardOwner} has shared the whiteboard ${whiteboardId} with you`;
    else
      modalMessage = `${whiteboardOwner} removed you from the whiteboard ${whiteboardId}`;
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
