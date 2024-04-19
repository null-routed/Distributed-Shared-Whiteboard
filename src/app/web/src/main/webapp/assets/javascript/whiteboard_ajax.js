import { addMessage } from "./whiteboard-ui.js";

export const removeParticipant = (userToRemove) => {
  const whiteboardID = new URLSearchParams(window.location.search).get("whiteboardID");

  fetch(contextPath + "/delete_whiteboard", {
    method: "POST",
    headers: { "Content-type": "application/x-www-form-urlencoded" },
    body: `whiteboardID=${whiteboardID}&username=${userToRemove}`
  })
      .then(response => response.json())
      .then(jsonResponse => {
        if (jsonResponse.success === true) {
          const removedParticipantDiv = document.getElementById(`${userToRemove}-container`);
          removedParticipantDiv.remove();
        }
        addMessage(jsonResponse.message);
      });
};

export const shareWhiteboard = (whiteboardId) => {
  const targetUsername = document.getElementById("new-participant-username").value;

  fetch(contextPath + "/share_whiteboard", {
    method: "POST",
    headers: { "Content-type": "application/x-www-form-urlencoded" },
    body: `whiteboardID=${whiteboardId}&newParticipantUsername=${targetUsername}`
  })
      .then(response => response.json())
      .then(jsonResponse => {
        if (jsonResponse.success === true) {
          const participantsListDiv = document.getElementById("participants");
          const participantContainer = document.createElement("div");
          participantContainer.className = "d-flex justify-content-between align-items-center mb-2";
          participantContainer.id = `${targetUsername}-container`;

          const participantName = document.createElement("p");
          participantName.className = "mb-0";
          participantName.textContent = targetUsername;
          participantContainer.appendChild(participantName);

          const removeButton = document.createElement("button");
          removeButton.type = "button";
          removeButton.className = "btn btn-danger btn-sm remove-participant-btn";
          removeButton.setAttribute("data-participant", targetUsername);
          removeButton.innerHTML = '<i class="bi bi-x"></i>';
          removeButton.addEventListener("click", () => {
            removeParticipant(targetUsername);
          });

          participantContainer.appendChild(removeButton);
          participantsListDiv.appendChild(participantContainer);
        }

        addMessage(jsonResponse.message);
      });
};

export function sendAJAXSnapshot() {
  const compressionFactor = 0.7;
  const currentWhiteboardCanvas = document.getElementById("whiteboard");
  const imageDataURL = currentWhiteboardCanvas.toDataURL("image/png", compressionFactor);
  const whiteboardId = document.getElementById("whiteboard-id").value;

  fetch(contextPath + "/snapshot_manager", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ whiteboardID: whiteboardId, snapshot: imageDataURL })
  });
}
