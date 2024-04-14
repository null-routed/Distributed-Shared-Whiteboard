import { setupListeners } from "./whiteboard-ui.js";
import { setupWebSocket } from "./whiteboard-websocket.js";
import { setupCanvas } from "./canvas-setup.js";
import { establishWebSocketConnection } from "./websocket-notifications.js";

document.addEventListener("DOMContentLoaded", () => {
  setupListeners();
  setupCanvas();
  establishWebSocketConnection();     // notifications
  setupWebSocket();         // sketching
});
