import { setupListeners } from "./whiteboard-ui.js";
import { setupWebSocket } from "./websocket.js";
import { setupCanvas } from "./canvas-setup.js";

document.addEventListener("DOMContentLoaded", () => {
  setupListeners();
  setupCanvas();
  setupWebSocket();
});
