import { establishWebSocketConnection } from "./websocket-notifications.js";

document.addEventListener("DOMContentLoaded", () => {
    establishWebSocketConnection();
});