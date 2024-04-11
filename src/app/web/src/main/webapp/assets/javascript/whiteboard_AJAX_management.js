// ------ PARTICIPANT REMOVAL VIA AJAX ------
function removeParticipantAJAX (username) {
    let whiteboardID = document.getElementById("whiteboardID").value;

    console.log("params: " + username + ", " + whiteboardID);

    let xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function () {
        if (this.readyState === 4 && this.status === 200) {
            // console.log("AJAX response: " + this.responseText)

            const jsonResponse = JSON.parse(this.responseText);

            if (jsonResponse.success === true) {      // removing participant from the list
                let removedParticipantDiv = document.getElementById(username);
                removedParticipantDiv.remove();
            }

            // displaying error or success message
            let participantsListDiv = document.getElementsByClassName("participants-list")[0];
            let outcomeMessageDiv = document.createElement("div");
            outcomeMessageDiv.style.marginTop = "10px";
            if (jsonResponse.success)
                outcomeMessageDiv.setAttribute("class", "success-msg");
            else
                outcomeMessageDiv.setAttribute("class", "error-msg");
            outcomeMessageDiv.textContent = jsonResponse.message;
            participantsListDiv.append(outcomeMessageDiv);

            // redirection to homepage + notification system handled by Erlang + websockets
        }
    }

    xhttp.open("POST", contextPath + "/remove_participant", true);
    xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

    xhttp.send("whiteboardID=" + whiteboardID + "&" + "username=" + username);
}


// ------ SHARING FUNCTIONALITY VIA AJAX ------
document.addEventListener("DOMContentLoaded", function () {
    document.getElementById("share-button-modal").addEventListener("click", function (event) {
        event.preventDefault();

        let whiteboardID = document.getElementById("whiteboardID").value;
        let username = document.getElementById("username").value;

        let xhttp = new XMLHttpRequest();
        xhttp.onreadystatechange = function () {
            if (this.readyState === 4 && this.status === 200) {
                // console.log("AJAX response: " + this.responseText)

                const jsonResponse = JSON.parse(this.responseText);

                if (jsonResponse.success === true) {        // adding new participant to list
                    let participantsListDiv = document.getElementsByClassName("participants-list")[0];

                    let newlyInsertedParticipantDiv = document.createElement("div");
                    newlyInsertedParticipantDiv.setAttribute("class", "whiteboard-participant");
                    newlyInsertedParticipantDiv.setAttribute("id", username);
                    newlyInsertedParticipantDiv.textContent = "• " + username;

                    participantsListDiv.append(newlyInsertedParticipantDiv);
                }

                // displaying error or success message
                let usernameTextBox = document.getElementsByClassName("modal-content")[1];
                let outcomeMessageDiv = document.createElement("div");
                outcomeMessageDiv.style.marginTop = "10px";
                if (jsonResponse.success)
                    outcomeMessageDiv.setAttribute("class", "success-msg");
                else
                    outcomeMessageDiv.setAttribute("class", "error-msg");
                outcomeMessageDiv.textContent = jsonResponse.message;
                usernameTextBox.append(outcomeMessageDiv);
            }
        }

        xhttp.open("POST", contextPath + "/share_whiteboard", true);
        xhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");

        xhttp.send("whiteboardID=" + whiteboardID + "&" + "username=" + username);
    });
});