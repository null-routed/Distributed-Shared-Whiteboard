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
    ws.send(
      JSON.stringify({
        action: "addStroke",
        data: {
          points: [
            [1, 2],
            [2, 3],
          ],
          thickness: 3,
          color: "#FFFFFF",
        },
        tempId: "temp-id-1",
      })
    );
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
