import { establishWebSocketConnection } from "./websocket-notifications.js";
import {addMessage} from "./whiteboard-ui.js";

document.addEventListener("DOMContentLoaded", () => {
  establishWebSocketConnection();
  setupListeners();
});

function showErrorModal() {
    let errorModal = document.getElementById("error-modal");
    if(errorModal != null) {
      errorModal = new bootstrap.Modal(errorModal);
      errorModal.show();
    }
}

function setupListeners () {
  let insert_modal = new bootstrap.Modal(document.getElementById("insert-whiteboard-modal"));
  let newWhiteboardButton = document.getElementById("new-whiteboard");
    newWhiteboardButton.addEventListener("click", () => {
        insert_modal.show();
    });
  let deleteWhiteboardButtons = document.getElementsByClassName("delete-whiteboard-button");
  for (let button of deleteWhiteboardButtons) {
    button.addEventListener("click",  () => {
      confirmDelete(button.getAttribute("data-wb-id"))
    });
  }

  let allView = document.getElementById("all-view");
  if(allView) {
    allView.addEventListener("click", () => {
      window.location.href = pageContext + "/homepage";
    });
  }

  let sharedView = document.getElementById("shared-view");
  if(sharedView) {
    sharedView.addEventListener("click", () => {
      console.log(pageContext+ "?shared=true");
      window.location.href = pageContext + "/homepage?shared=true";
    });
  }
}

export function confirmDelete (whiteboardId) {
  let deleteConfirmation = confirm(
      "Are you sure you want to delete this whiteboard? \n" +
      "If you are not the owner you will leave the participation."
  )

  if (deleteConfirmation) {
    fetch(pageContext + "/delete_whiteboard", {
      method: "POST",
      headers: { "Content-type": "application/x-www-form-urlencoded" },
      body: `whiteboardID=${whiteboardId}`
    })
        .then(response => response.json())
        .then(jsonResponse => {
          if (jsonResponse.success === true) {
            document.getElementById(`whiteboard_${whiteboardId}`).remove();
            addMessage(jsonResponse.message);
          } else {
            showErrorModal();
          }
        });
  }
}
