import { establishWebSocketConnection } from "./websocket-notifications.js";
import { addToast } from "./whiteboard-ui.js";
import { addNewWhiteboardToDOM } from "./homepage-ui.js";
document.addEventListener("DOMContentLoaded", () => {
  establishWebSocketConnection();
  setupListeners();
});

function showErrorModal() {
  let errorModal = $("#error-modal");
  if (errorModal != null) {
    errorModal = new bootstrap.Modal(errorModal);
    errorModal.show();
  }
}

function setupListeners() {
  let deleteWhiteboardButtons = $(".delete-whiteboard-button");
  deleteWhiteboardButtons.each(function () {
    $(this).click(() => {
      confirmDelete($(this).attr("data-wb-id"));
    });
  });

  let allView = $("#all-view");
  if (allView) {
    allView.click(() => {
      window.location.href = pageContext + "/homepage";
    });
  }

  let sharedView = $("#shared-view");
  if (sharedView) {
    sharedView.click(() => {
      console.log(pageContext + "?shared=true");
      window.location.href = pageContext + "/homepage?shared=true";
    });
  }

  $('[data-toggle="popover"]').popover({
    trigger: "hover",
    container: "body",
  });

  $("#insert-new-wb-submit").click(submitWhiteboardForm);
}

export function confirmDelete(whiteboardId) {
  let deleteConfirmation = confirm(
    "Are you sure you want to delete this whiteboard? \n" +
      "If you are not the owner you will leave the participation."
  );

  if (deleteConfirmation) {
    fetch(pageContext + "/delete_whiteboard", {
      method: "POST",
      headers: { "Content-type": "application/x-www-form-urlencoded" },
      body: `whiteboardID=${whiteboardId}`,
    })
      .then((response) => response.json())
      .then((jsonResponse) => {
        if (jsonResponse.success === true) {
          document.getElementById(`whiteboard_${whiteboardId}`).remove();
          addToast(jsonResponse.message);
        } else {
          showErrorModal();
        }
      });
  }
}

function submitWhiteboardForm() {
  $("#insert-whiteboard-modal").modal("hide");

  const whiteboardName = $("#whiteboardName").val();
  const whiteboardDescription = $("#whiteboardDescription").val();
  const readOnly = $("#readOnly").prop("checked");

  // Construct the form data
  const formData = new URLSearchParams();
  formData.append("whiteboardName", whiteboardName);
  formData.append("whiteboardDescription", whiteboardDescription);
  formData.append("readOnly", readOnly);

  // Send the data using fetch
  fetch(`${pageContext}/insert_whiteboard`, {
    method: "POST",
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body: formData,
  })
    .then((response) => response.json())
    .then((data) => {
      if (!data.success) {
        showErrorModal();
      } else {
        addNewWhiteboardToDOM(
          data.message,
          whiteboardName,
          whiteboardDescription
        );
      }
    })
    .catch((error) => {
      console.error("Error:", error);
    });
}
