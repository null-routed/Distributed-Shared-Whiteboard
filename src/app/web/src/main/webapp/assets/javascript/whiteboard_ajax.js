import { addMessage } from "./whiteboard-ui.js";

export const removeParticipant = (userToRemove) => {
  let whiteboardID = new URLSearchParams(window.location.search).get("whiteboardID");

  let xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function () {
    if (xhttp.readyState === 4 && xhttp.status === 200) {
      const jsonResponse = JSON.parse(xhttp.responseText);

      if (jsonResponse.success === true) {
        let removedParticipantDiv = document.getElementById(
          `${userToRemove}-container`
        );
        removedParticipantDiv.remove();
      }
      addMessage(jsonResponse.message);
    }
  };

  xhttp.open("POST", contextPath + "/remove_participant", true);
  xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

  xhttp.send(
    "whiteboardID=" +
      whiteboardID +
      "&userToRemove=" +
      userToRemove
  );
};

export const shareWhiteboard = (
  whiteboardId,
) => {
  let targetUsername = document.getElementById(
    "new-participant-username"
  ).value;

  let xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = () => {
    if (xhttp.readyState === 4 && xhttp.status === 200) {
      const jsonResponse = JSON.parse(xhttp.responseText);

      if (jsonResponse.success === true) {
        let participantsListDiv = document.getElementById("participants");

        let participantContainer = document.createElement("div");
        participantContainer.className =
          "d-flex justify-content-between align-items-center mb-2";
        participantContainer.id = `${targetUsername}-container`;

        let participantName = document.createElement("p");
        participantName.className = "mb-0";
        participantName.textContent = targetUsername;
        participantContainer.appendChild(participantName);

        let removeButton = document.createElement("button");
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
    }
  };

  xhttp.open("POST", contextPath + "/share_whiteboard", true);
  xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

  xhttp.send(
    "whiteboardID=" +
      whiteboardId +
      "&" +
      "newParticipantUsername=" +
      targetUsername
  );
};
export function sendAJAXSnapshot () {
  const compressionFactor = 0.7;
  let currentWhiteboardCanvas = document.getElementById("whiteboard");
  let imageDataURL = currentWhiteboardCanvas.toDataURL("image/png", compressionFactor);
  const whiteboardId = document.getElementById("whiteboard-id").value;


  let xhr = new XMLHttpRequest();
  xhr.open("POST", contextPath + "/snapshot_manager", true);
  xhr.setRequestHeader("Content-Type", "application/json");

  xhr.send(JSON.stringify({whiteboardID: whiteboardId, snapshot: imageDataURL}));
}


