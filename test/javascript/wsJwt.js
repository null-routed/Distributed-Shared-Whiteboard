import jwt from "jsonwebtoken";
import WebSocket from "ws";

const createWebSocketConnection = (username, disconnectAfter = null) => {
  const payload = { username };
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
    console.log(`${username} WebSocket connection established`);
  });

  ws.on("message", function incoming(data) {
    let readableText = new TextDecoder().decode(data);
    console.log(`Received message for ${username}:`, readableText);
  });

  ws.on("close", function (code, reason) {
    console.log(
      `${username} WebSocket closed with code: ${code}, reason: ${reason}`
    );
  });

  ws.on("error", function (error) {
    console.error(`${username} WebSocket encountered an error:`, error);
  });

  if (disconnectAfter) {
    setTimeout(() => {
      ws.close(1000, "Closing after timeout");
    }, disconnectAfter);
  }

  return ws;
};

const user1Ws = createWebSocketConnection("exampleUser", 5000);
setTimeout(() => {
  const user2Ws = createWebSocketConnection("exampleUser1", 2000);
}, 5);
