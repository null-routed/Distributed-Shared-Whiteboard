/* ------ ERROR DISPLAYING MODAL ------ */
let error_X_span = document.getElementsByClassName("close")[0];

error_X_span.onclick = function () {
    let error_modal = document.getElementById("error-display-modal");
    error_modal.style.display = "none";
}

window.onclick = function(event) {
    let error_modal = document.getElementById("error-display-modal");
    if (event.target === error_modal) {
        insert_modal.style.display = "none";
    }
}

/* ------ INSERT NEW WHITEBOARD MODAL ------ */
let insert_modal = document.getElementById("insert-whiteboard-modal");
let insert_btn = document.getElementById("add-button");
let insert_X_span = document.getElementsByClassName("close")[1];

// When the user clicks on the button, open the modal
insert_btn.onclick = function() {
    insert_modal.style.display = "block";
}

// When the user clicks on <span> (x), close the modal
insert_X_span.onclick = function() {
    insert_modal.style.display = "none";
}

// When the user clicks anywhere outside the modal, close it
window.onclick = function(event) {
    if (event.target === insert_modal) {
        insert_modal.style.display = "none";
    }
}

function confirmDelete(whiteboardId) {
    if (confirm("Are you sure you want to delete this whiteboard? \n" +
        "If you are not the owner you will leave the participation")) {
        // submit the form
        document.getElementById("whiteboardIdToDelete").value = whiteboardId;
        document.getElementById("deleteWhiteboardForm").submit();
    }
}