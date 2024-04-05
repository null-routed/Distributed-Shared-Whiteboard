document.addEventListener("DOMContentLoaded", function() {
    console.log("DOM loaded");
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
        if (event.target == modal) {
            modal.style.display = "none";
        }
    }
});