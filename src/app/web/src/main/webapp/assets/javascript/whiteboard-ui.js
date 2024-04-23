import { penLogic, rubberLogic } from "./canvas-setup.js";
import { sendRedo, sendUndo } from "./whiteboard-websocket.js";
import { removeParticipant, shareWhiteboard } from "./whiteboard_ajax.js";
import { chooseColor } from "./canvas-setup.js";

let $penButton, $rubberButton, $redoButton, $undoButton, $toastContainer;
let selectedColor = "black";
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

  $('[data-bs-toggle="popover"]').popover({
    html: true,
    title: 'Color Palette',
    content: function() {
      return `
                <div class="color-palette-container">
                    <div class="color-box black ${selectedColor === 'black' ? 'selected-color' : ''}"></div>
                    <div class="color-box red ${selectedColor === 'red' ? 'selected-color' : ''}"></div>
                    <div class="color-box blue ${selectedColor === 'blue' ? 'selected-color' : ''}"></div>
                    <div class="color-box green ${selectedColor === 'green' ? 'selected-color' : ''}"></div>
                    <div class="color-box purple ${selectedColor === 'purple' ? 'selected-color' : ''}"></div>
                </div>`;
    }
  });

  $(document).on('click', '.color-box', (event) => {
    let target = $(event.target);
    target.addClass('selected-color');
    $(".color-box").not(target).removeClass('selected-color');
    let colorClass = target.attr('class').split(' ')[1];
    selectedColor = colorClass;
    chooseColor(colorClass);
  });

};

export const addToast = (message) => {
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

export const removeParticipantFromDOM = (participant) => {
  $(`#${participant}-container`).remove();
}

export const addParticipantToDOM = (participant) => {
  const participantHtml = `
    <div class="d-flex justify-content-between align-items-center mb-2" id="${participant}-container">
        <p class="mb-0">${participant}</p>                      
    </div>
  `;

  $("#participants").append(participantHtml);
};
