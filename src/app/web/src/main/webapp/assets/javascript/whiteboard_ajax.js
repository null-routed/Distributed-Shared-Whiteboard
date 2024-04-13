import { addMessage } from "./whiteboard-ui.js";

export const removeParticipant = (userToRemove) => {
  let whiteboardID = document.getElementById("whiteboardID").value;
  let whiteboardOwner = document.getElementById("self-username").value;
  let whiteboardName = document.getElementById("whiteboard-name").value;

  let xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function () {
    if (this.readyState === 4 && this.status === 200) {
      const jsonResponse = JSON.parse(this.responseText);

      if (jsonResponse.success === true) {
        let removedParticipantDiv = document.getElementById(
          `${username}-container`
        );
        removedParticipantDiv.remove();
      }
      addMessage(jsonResponse.message);
    }
  };

  xhttp.open("GET", contextPath + "/remove_participant", true);
  xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

  xhttp.send(
    "whiteboardID=" +
      whiteboardID +
      "&userToRemove=" +
      userToRemove +
      "&whiteboardOwner=" +
      whiteboardOwner +
      "&whiteboardName=" +
      whiteboardName
  );
};

export const shareWhiteboard = (
  whiteboardId,
  whiteboardName,
  whiteboardOwner
) => {
  let targetUsername = document.getElementById(
    "new-participant-username"
  ).value;

  let xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = () => {
    if (this.readyState === 4 && this.status === 200) {
      const jsonResponse = JSON.parse(this.responseText);

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

  xhttp.open("GET", contextPath + "/share_whiteboard", true);
  xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

  xhttp.send(
    "whiteboardID=" +
      whiteboardId +
      "&" +
      "newParticipantUsername=" +
      targetUsername +
      "&" +
      "whiteboardOwner=" +
      whiteboardOwner +
      "&whiteboardName=" +
      whiteboardName
  );
};
