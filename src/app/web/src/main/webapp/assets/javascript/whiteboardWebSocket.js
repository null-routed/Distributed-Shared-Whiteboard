// Function to establish WebSocket connection if not already present
let socket;
function establishWebSocketConnection() {
    // Check if WebSocket connection already exists
    if (!socket || socket.readyState !== WebSocket.OPEN) {
        // Create new WebSocket connection only if not already present or if not in OPEN state
        socket = new WebSocket(`ws://localhost:8080/web/websocket`);

        // Function to handle WebSocket open event
        socket.onopen = function(event) {
            console.log("WebSocket connection established.");
        };

        // Function to handle WebSocket message event
        socket.onmessage = function(event) {
            console.log("Message received from server: " + event.data);
            // Handle the received message here...
        };

        // Function to handle WebSocket close event
        socket.onclose = function(event) {
            console.log("WebSocket connection closed.");
        };

        // Function to handle WebSocket error event
        socket.onerror = function(event) {
            console.log("WebSocket error: " + event.data);
        };
    } else {
        console.log("WebSocket connection already exists.");
    }
}

// Call establishWebSocketConnection() when the page loads
window.addEventListener('load', function() {
    establishWebSocketConnection();
});