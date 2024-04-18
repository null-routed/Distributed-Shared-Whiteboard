import { addMessage } from "./whiteboard-ui.js";
import { confirmDelete } from "./homepage.js";
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
  const whiteboardName = parsedMessage.whiteboardName;
  const whiteboardOwner = parsedMessage.whiteboardOwner;
  const command = parsedMessage.command;
  const currentUser = document.getElementById("self-username").value;
  let toastMessage = null;

  if (command === "share") {
    toastMessage=`${whiteboardOwner} has shared the whiteboard ${whiteboardName} with you`;
    // Check if the current page URL matches a certain pattern
    if (window.location.href.includes("/homepage"))
      // Call the functions only when on the homepage
      addNewWhiteboardToDOM(parsedMessage.whiteboardID, whiteboardName, whiteboardOwner,
          parsedMessage.whiteboardDescription, parsedMessage.whiteboardReadOnly);
  } else if(command === "remove") {
    if(whiteboardOwner !== currentUser)
      toastMessage = `${whiteboardOwner} has left the whiteboard ${whiteboardName}`;
    else {
      toastMessage = `${whiteboardOwner} removed you from the whiteboard ${whiteboardName}`;
      if (window.location.href.includes("/homepage"))
        removeWhiteboardFromDOM(parsedMessage.whiteboardID);
    }
  }
  else {
    if(whiteboardOwner !== currentUser) {
      toastMessage = `${whiteboardOwner} removed you from the whiteboard ${whiteboardName}`;
      if (window.location.href.includes("/homepage"))
        removeWhiteboardFromDOM(parsedMessage.whiteboardID);
    }
    else
      toastMessage = `${whiteboardOwner} has left the whiteboard ${whiteboardName}`;
  }
  // show toast notification
  addMessage(toastMessage); // Call the addMessage function to display the toast
}

// Define a function to add the new whiteboard HTML to the DOM
function addNewWhiteboardToDOM(id, name, owner, description, readOnly) {
  const whiteboardsContainer = document.querySelector('#whiteboard-container');
  const colDiv = document.createElement('div');
  colDiv.className = 'col-sm-6 col-md-4 col-lg-3 mb-4';
  colDiv.id = `whiteboard_${id}`;

  colDiv.innerHTML = `
        <div class="card">
            <div class="card-img-top-wrapper">
                <a href="${pageContext}/whiteboard?whiteboardID=${id}">
                    <img class="card-img-top" alt="${name}" src="${pageContext}/snapshot_manager?whiteboardID=${id}">
                </a>
                <button type="button" class="delete-whiteboard-button btn btn-danger btn-sm" data-wb-id="${id}">
                    <i class="bi bi-x"></i>
                </button>
            </div>
        </div>`;

  whiteboardsContainer.appendChild(colDiv);
  const deleteButton = colDiv.querySelector('.delete-whiteboard-button');
  deleteButton.addEventListener('click', (event) => {
    confirmDelete(event.target.getAttribute('data-wb-id'));
  });
}

// Define a function to remove the whiteboard HTML from the DOM
function removeWhiteboardFromDOM(whiteboardId) {
  // Find the whiteboard element by its ID
  const whiteboardElement = document.getElementById(`whiteboard_${whiteboardId}`);
  if (whiteboardElement) {
    // Find the parent div with the class grid-item-whiteboard
    const parentDiv = whiteboardElement.closest('.grid-item-whiteboard');
    if (parentDiv) {
      // Remove the parent div from the DOM
      parentDiv.parentNode.removeChild(parentDiv);
    }
  }
}
