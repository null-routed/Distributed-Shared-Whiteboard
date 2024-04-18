import { setupListeners } from "./whiteboard-ui.js";
import { setupWebSocket } from "./whiteboard-websocket.js";
import { setupCanvas } from "./canvas-setup.js";
import { establishWebSocketConnection } from "./websocket-notifications.js";
import { sendAJAXSnapshot } from "./whiteboard_ajax.js";

document.addEventListener("DOMContentLoaded", () => {
  setupListeners();
  setupCanvas();
  establishWebSocketConnection();     // notifications
  setupWebSocket();
  setInterval(sendAJAXSnapshot, 5000);
});

