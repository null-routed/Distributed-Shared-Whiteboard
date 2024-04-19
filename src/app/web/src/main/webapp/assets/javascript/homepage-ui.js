import { confirmDelete } from "./homepage.js";

export const addNewWhiteboardToDOM = (id, name, owner, description) => {
  const whiteboardsContainer = $("#whiteboard-container");
  const colDiv = document.createElement("div");
  colDiv.className = "col-sm-6 col-md-4 col-lg-3 mb-4";
  colDiv.id = `whiteboard_${id}`;

  colDiv.innerHTML = `
        <div class="card">
            <div class="card-img-top-wrapper">
                <a href="${pageContext}/whiteboard?whiteboardID=${id}">
                    <img class="card-img-top" alt="${name}" 
                        data-toggle="popover" 
                        title="${name}"
                        data-content="${description.replace(/"/g, '&quot;')}" /
                        data-trigger="hover"
                        data-placement="right"    
                        src="${pageContext}/snapshot_manager?whiteboardID=${id}" 
                    >
                </a>
                <button type="button" class="delete-whiteboard-button btn btn-danger btn-sm" data-wb-id="${id}">
                    <i class="bi bi-x"></i>
                </button>
            </div>
        </div>`;

  whiteboardsContainer.append(colDiv);

  // Convert newly created colDiv to jQuery object for using jQuery methods
  const $colDiv = $(colDiv);

  // Initialize popover on the new image element
  $colDiv.find('.card-img-top').popover({
    html: true, // If you are using HTML in your descriptions or titles
    trigger: "hover",
    container: "body",
    sanitize: false // If you're sure your content is safe, you can disable sanitization
  });

  // Attach event listener for delete button
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
