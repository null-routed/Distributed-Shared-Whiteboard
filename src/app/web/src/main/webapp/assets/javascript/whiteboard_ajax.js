import { addToast } from "./whiteboard-ui.js";

export const removeParticipant = (userToRemove) => {
  const whiteboardID = new URLSearchParams(window.location.search).get("whiteboardID");

  $.post(contextPath + "/delete_whiteboard", {
    whiteboardID: whiteboardID,
    username: userToRemove
  }).done((jsonResponse) => {
    if (jsonResponse.success === true) {
      $(`#${userToRemove}-container`).remove();
    }
    addToast(jsonResponse.message);
  });
};

export const shareWhiteboard = (whiteboardId) => {
  const targetUsername = $("#new-participant-username").val();

  $.post(contextPath + "/share_whiteboard", {
    whiteboardID: whiteboardId,
    newParticipantUsername: targetUsername
  }).done((jsonResponse) => {
    if (jsonResponse.success === true) {
      const participantContainer = $('<div/>', {
        'class': 'd-flex justify-content-between align-items-center mb-2',
        'id': `${targetUsername}-container`
      });

      const participantName = $('<p/>', {
        'class': 'mb-0',
        'text': targetUsername
      }).appendTo(participantContainer);

      const removeButton = $('<button/>', {
        'type': 'button',
        'class': 'btn btn-danger btn-sm remove-participant-btn',
        'html': '<i class="bi bi-x"></i>',
        'data-participant': targetUsername
      }).on("click", function() {
        removeParticipant(targetUsername);
      }).appendTo(participantContainer);

      $('#participants').append(participantContainer);
    }
    addToast(jsonResponse.message);
  });
};

export function sendAJAXSnapshot() {
  const compressionFactor = 0.7;
  const imageDataURL = $('#whiteboard').get(0).toDataURL("image/png", compressionFactor);
  const whiteboardId = $('#whiteboard-id').val();

  $.ajax({
    url: contextPath + "/snapshot_manager",
    method: "POST",
    contentType: "application/json",
    data: JSON.stringify({
      whiteboardID: whiteboardId,
      snapshot: imageDataURL
    })
  });
}
