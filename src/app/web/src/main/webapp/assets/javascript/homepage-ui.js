import { confirmDelete } from "./homepage.js";

export const addNewWhiteboardToDOM = (id, name, description) => {
  const whiteboardsContainer = document.getElementById("whiteboard-container");
  const colDiv = document.createElement("div");
  colDiv.className = "col-sm-6 col-md-4 col-lg-3 mb-4";
  colDiv.id = `whiteboard_${id}`;

  colDiv.innerHTML = `
      <div class="card">
          <div class="card-img-top-wrapper">
              <a href="${pageContext}/whiteboard?whiteboardID=${id}">
                  <img class="card-img-top" alt="${name}"
                      data-bs-toggle="popover" 
                      title="${name}"
                      data-bs-content="${description}"
                      data-bs-trigger="hover"
                      data-bs-placement="right"    
                      src="${pageContext}/snapshot_manager?whiteboardID=${id}">
              </a>
              <button type="button" class="delete-whiteboard-button btn btn-danger btn-sm" data-wb-id="${id}">
                  <i class="bi bi-x"></i>
              </button>
          </div>
      </div>`;

  whiteboardsContainer.appendChild(colDiv);

  const imgElement = colDiv.querySelector('.card-img-top');
  new bootstrap.Popover(imgElement, {
    trigger: 'hover',
    container: 'body',
    html: true,
    content: description || 'No description available'
  });

  const $colDiv = $(colDiv);
  $colDiv.find('.delete-whiteboard-button').on('click', function(event) {
    confirmDelete($(this).data('wb-id'));
  });
};

export const removeWhiteboardFromDOM = (whiteboardId) => {
  const whiteboardElement = $(`#whiteboard_${whiteboardId}`);
  if (whiteboardElement) {
    whiteboardElement.remove();
  }
};
