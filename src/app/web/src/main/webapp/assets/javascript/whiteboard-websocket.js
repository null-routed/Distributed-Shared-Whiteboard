import {
  cursorPosition,
  updateUserCursor,
  usersMap,
  addStroke,
  deleteStroke,
  updateStroke,
  tempIdMap,
  removeUserCursor,
} from "./canvas-setup.js";
import { addToast } from "./whiteboard-ui.js";
import { getRandomColor } from "./utils.js";

let ws, username;
let receivedList = false;
const jwt = document.cookie
  .split("; ")
  .find((row) => row.startsWith("jwt="))
  .split("=")[1];
const urlParams = new URLSearchParams(window.location.search);
const whiteboardID = urlParams.get("whiteboardID");

export const setupWebSocket = () => {
  ws = new WebSocket(`ws://127.0.0.1:8888/ws/${whiteboardID}?jwt=` + jwt);
  username = document.getElementById("self-username").value;
  ws.onopen = () => {
    setInterval(() => {
      sendSelfCursor();
    }, 50);
  };

  ws.onerror = () => {
    alert(
      "An error occurred while establishing the connection with the whiteboard. Make sure you have the permissions to access this whiteboard and refresh the page."
    );
    setTimeout(() => {
      window.location.href = contextPath + "/homepage";
    }, 0);
  };

  ws.onclose = (event) => {
    alert("WebSocket closed due to: " + event.reason);
    setTimeout(() => {
      window.location.href = contextPath + "/homepage";
    }, 0);
  };

  ws.onmessage = (event) => {
    handleWebsocketMessage(event);
  };
};

export const sendDeleteStroke = (strokeId) => {
  ws.send(
    JSON.stringify({
      action: "deleteStroke",
      strokeId: strokeId,
    })
  );
};

export const sendNewStroke = (path) => {
  ws.send(
    JSON.stringify({
      action: "addStroke",
      data: JSON.stringify(path.toObject()),
      tempId: path.strokeId,
    })
  );
};

export const sendUndo = () => {
  ws.send(
    JSON.stringify({
      action: "undo",
    })
  );
};

export const sendRedo = () => {
  ws.send(
    JSON.stringify({
      action: "redo",
    })
  );
};

const sendSelfCursor = () => {
  ws.send(
    JSON.stringify({
      action: "updateUserCursor",
      data: {
        x: cursorPosition.x,
        y: cursorPosition.y,
      },
    })
  );
};

const handleWebsocketMessage = (event) => {
  const message = JSON.parse(event.data);
  switch (message.action) {
    case "addStroke":
      if (
        message.tempId !== undefined &&
        tempIdMap[message.tempId] !== undefined
      ) {
        updateStroke(message.tempId, message.strokeId);
      } else {
        addStroke(message.data, message.strokeId);
      }
      break;
    case "deleteStroke":
      deleteStroke(message.strokeId);
      break;
    case "updateUserList":
      if (receivedList) {
        handleUserList(message.users, true);
      } else {
        handleUserList(message.users, false);
        receivedList = true;
      }
      break;
    case "updateUserCursor":
      updateUserCursor(message.data, message.username);
      break;
    default:
      console.log("Unknown message action:", message.action);
  }
};

const handleUserList = (users, showMessage) => {
  for (const user of users) {
    if (!usersMap[user]) {
      usersMap[user] = { color: getRandomColor() };
      if (showMessage) addToast(`${user} joined.`);
    }
  }

  for (const user in usersMap) {
    if (!users.includes(user)) {
      removeUserCursor(user);
      delete usersMap[user];
      addToast(`${user} disconnected.`);
    }
  }
};
