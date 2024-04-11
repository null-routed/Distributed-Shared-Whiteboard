// EventListener for the participant list modal
document.addEventListener("DOMContentLoaded", function() {
    let modal = document.getElementById("participants-modal");
    let button = document.getElementById("participants-button");
    let closeButton = document.getElementsByClassName("close")[0];

    button.onclick = function () {
        modal.style.display = "inline-block";
    }

    closeButton.onclick = function () {
        modal.style.display = "none";
    }

    window.onclick = function (event) {
        if (event.target === modal) {
            modal.style.display = "none";
            clearMessages();
        }
    }
});

// EventListener for the share functionality
document.addEventListener("DOMContentLoaded", function() {
    // Open the modal when the share button is clicked
    document.getElementById("share-button").addEventListener("click", function() {
        document.getElementById("share-modal").style.display = "block";
    });

    // Close the modal when the close button is clicked
    document.querySelector("#share-modal .close").addEventListener("click", function() {
        document.getElementById("share-modal").style.display = "none";
        clearMessages();
    });

    // Close the modal when clicking outside of it
    window.addEventListener("click", function(event) {
        if (event.target === document.getElementById("share-modal")) {
            document.getElementById("share-modal").style.display = "none";
            clearMessages();
        }
    });
});

function clearMessages() {
    const errorMessageElement = document.querySelector(".error-msg");
    const successMessageElement = document.querySelector(".success-msg");

    if (errorMessageElement)
        errorMessageElement.remove();

    if (successMessageElement)
        successMessageElement.remove();
}
