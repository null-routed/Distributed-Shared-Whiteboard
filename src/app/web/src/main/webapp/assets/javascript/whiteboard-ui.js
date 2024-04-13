import { penLogic, rubberLogic } from "./canvas-setup.js";
import { sendRedo, sendUndo } from "./whiteboard-websocket.js";

let penButton, rubberButton, redoButton, undoButton, toastContainer;

export const setupListeners = () => {
  penButton = document.getElementById("pen-button");
  rubberButton = document.getElementById("rubber-button");
  redoButton = document.getElementById("redo-button");
  undoButton = document.getElementById("undo-button");
  toastContainer = document.getElementById("toast-container");

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
};

export const addMessage = (user, action) => {
  const toastId = `toast-${Date.now()}`;
  const toastHtml = `
        <div class="toast" role="alert" aria-live="assertive" aria-atomic="true" id="${toastId}">
            <div class="toast-header">
                <strong class="me-auto">Notification</strong>
                <small>Now</small>
                <button type="button" class="btn-close" data-bs-dismiss="toast" aria-label="Close"></button>
            </div>
            <div class="toast-body">
                ${user} ${action}.
            </div>
        </div>
    `;

  toastContainer.insertAdjacentHTML("beforeend", toastHtml);
  const toastElement = document.getElementById(toastId);
  const toast = new bootstrap.Toast(toastElement, {
    delay: 2000,
  });

  toast.show();

  toastElement.addEventListener("hidden.bs.toast", () => {
    toastElement.remove();
  });
};
