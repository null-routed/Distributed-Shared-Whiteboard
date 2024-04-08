// Get the modal
let modal = document.getElementById("myModal");

// Get the button that opens the modal
let btn = document.getElementById("addButton");

// Get the <span> element that closes the modal
let span = document.getElementsByClassName("close")[0];

// When the user clicks on the button, open the modal
btn.onclick = function() {
    modal.style.display = "block";
}

// When the user clicks on <span> (x), close the modal
span.onclick = function() {
    modal.style.display = "none";
}

// When the user clicks anywhere outside the modal, close it
window.onclick = function(event) {
    if (event.target === modal) {
        modal.style.display = "none";
    }
}

function confirmDelete(whiteboardId) {
    if (confirm("Are you sure you want to delete this whiteboard? \n" +
        "If you are not the owner you will leave the participation")) {
        // submit the form
        document.getElementById("whiteboardIdToDelete").value = whiteboardId;
        document.getElementById("deleteWhiteboardForm").submit();
        //deleteWhiteboard(whiteboardId)
    }
}

/*
// Function to send an AJAX request for whiteboard deletion
function deleteWhiteboard(whiteboardId) {
    // AJAX request to send whiteboardId to Java backend for Erlang RPC call
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "${pageContext.request.contextPath}/homepage", true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function() {
        if (xhr.readyState === XMLHttpRequest.DONE) {
            if (xhr.status === 200) {
                // Success
                alert("Whiteboard deleted successfully.");
                // Reload the page or update UI as needed
            } else {
                // Error handling
                alert("Failed to delete whiteboard. Please try again later.");
            }
        }
    };
    var data = JSON.stringify({ "whiteboardId": whiteboardId });
    xhr.send(data);
}
*/
