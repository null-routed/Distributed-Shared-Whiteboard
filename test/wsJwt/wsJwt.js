import jwt from "jsonwebtoken";
import WebSocket from "ws";

const payload = { username: "exampleUser" };
const secret = "yourRandomSecretKey";
const token = jwt.sign(payload, secret, { expiresIn: "1h" });

const cookie = `jwt=${token}; Path=/; HttpOnly`;

const wsUrl = "ws://localhost:8080/ws/test";

const ws = new WebSocket(wsUrl, {
  headers: {
    Cookie: cookie,
  },
});

ws.on("open", function open() {
  console.log("WebSocket connection established");
  ws.send("Hello server");
});

ws.on("message", function incoming(data) {
  console.log("Received message:", data);
});

ws.on("close", function (code, reason) {
  console.log(`WebSocket closed with code: ${code}, reason: ${reason}`);
});

ws.on("error", function (error) {
  console.error("WebSocket encountered an error:", error);
});
