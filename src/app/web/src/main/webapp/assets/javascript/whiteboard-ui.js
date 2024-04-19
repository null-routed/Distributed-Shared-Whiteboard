import { penLogic, rubberLogic } from "./canvas-setup.js";
import { sendRedo, sendUndo } from "./whiteboard-websocket.js";
import { removeParticipant, shareWhiteboard } from "./whiteboard_ajax.js";

let penButton, rubberButton, redoButton, undoButton, toastContainer;

export const setupListeners = () => {
  penButton = document.getElementById("pen-button");
  rubberButton = document.getElementById("rubber-button");
  redoButton = document.getElementById("redo-button");
  undoButton = document.getElementById("undo-button");
  //toastContainer = document.getElementById("toast-container");

  penButton.addEventListener("click", function () {
    penLogic();
    penButton.classList.add("active");
    rubberButton.classList.remove("active");
  });

  rubberButton.addEventListener("click", () => {
    rubberLogic();
    penButton.classList.remove("active");
    rubberButton.classList.add("active");
  });

  undoButton.addEventListener("click", () => {
    sendUndo();
  });

  redoButton.addEventListener("click", () => {
    sendRedo();
  });

  document.querySelectorAll(".remove-participant-btn").forEach((button) => {
    button.addEventListener("click", () => {
      removeParticipant(button.getAttribute("data-participant"));
    });
  });

  document
    .getElementById("share-button-modal")
    .addEventListener("click", () => {
      shareWhiteboard(
        new URLSearchParams(window.location.search).get("whiteboardID")
      );
    });
};

export const addMessage = (message) => {
  toastContainer = document.getElementById("toast-container");
  const toastId = `toast-${Date.now()}`;
  const toastHtml = `
        <div class="toast" role="alert" aria-live="assertive" aria-atomic="true" id="${toastId}">
            <div class="toast-header">
                <strong class="me-auto">Notification</strong>
                <small>Now</small>
                <button type="button" class="btn-close" data-bs-dismiss="toast" aria-label="Close"></button>
            </div>
            <div class="toast-body">
                ${message}
            </div>
        </div>
    `;

  toastContainer.insertAdjacentHTML("beforeend", toastHtml);
  const toastElement = document.getElementById(toastId);
  const toast = new bootstrap.Toast(toastElement, {
    delay: 2500,
  });

  toast.show();

  toastElement.addEventListener("hidden.bs.toast", () => {
    toastElement.remove();
  });
};
