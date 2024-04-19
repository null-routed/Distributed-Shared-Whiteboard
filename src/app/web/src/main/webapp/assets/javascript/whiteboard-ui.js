import { penLogic, rubberLogic } from "./canvas-setup.js";
import { sendRedo, sendUndo } from "./whiteboard-websocket.js";
import { removeParticipant, shareWhiteboard } from "./whiteboard_ajax.js";

let $penButton, $rubberButton, $redoButton, $undoButton, $toastContainer;

export const setupListeners = () => {
  $penButton = $("#pen-button");
  $rubberButton = $("#rubber-button");
  $redoButton = $("#redo-button");
  $undoButton = $("#undo-button");
  //$toastContainer = $("#toast-container");

  $penButton.click(function () {
    penLogic();
    $(this).addClass("active");
    $rubberButton.removeClass("active");
  });

  $rubberButton.click(() => {
    rubberLogic();
    $penButton.removeClass("active");
    $(this).addClass("active");
  });

  $undoButton.click(() => {
    sendUndo();
  });

  $redoButton.click(() => {
    sendRedo();
  });

  $(".remove-participant-btn").click(function () {
    removeParticipant($(this).data("participant"));
  });

  $("#share-button-modal").click(() => {
    shareWhiteboard(
        new URLSearchParams(window.location.search).get("whiteboardID")
    );
  });
};

export const addMessage = (message) => {
  $toastContainer = $("#toast-container");
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

  $toastContainer.append(toastHtml);
  const $toastElement = $(`#${toastId}`);
  const toast = new bootstrap.Toast($toastElement, {
    delay: 2500,
  });

  toast.show();

  $toastElement.on('hidden.bs.toast', function () {
    $(this).remove();
  });
};
