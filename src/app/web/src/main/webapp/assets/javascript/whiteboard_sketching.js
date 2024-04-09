document.addEventListener("DOMContentLoaded", function() {
    window.addEventListener('load', ()=>{
        resize(); // Resizes the canvas once the window loads
    });
    let isPenToggled = true;
    let isRubberToggled = false;
    let lastPosition;
    // Drawing and erasing functionality
    let isDrawing = false;
    let isErasing = false;
    const penButton = document.getElementById("pen-button");
    const rubberButton = document.getElementById("rubber-button");
    const canvas = document.getElementById("whiteboard");
    const ctx = canvas.getContext('2d');

    // Resizes the canvas to the available size of the window.
    function resize(){
        ctx.canvas.width = window.innerWidth;
        ctx.canvas.height = window.innerHeight;
    }

    // Initial state: Pen button is selected
    penButton.classList.add("button-selected");

    penButton.addEventListener("click", function () {
        if (!isPenToggled) {
            isPenToggled = true;
            penButton.classList.add("button-selected");
            isRubberToggled = false; // Ensure rubber button is deselected
            rubberButton.classList.remove("button-selected");
            canvas.style.cursor = "url(/web/assets/images/pen.png), auto";
        }
    });

    rubberButton.addEventListener("click", function () {
        if (!isRubberToggled) {
            isRubberToggled = true;
            rubberButton.classList.add("button-selected");
            isPenToggled = false; // Ensure pen button is deselected
            penButton.classList.remove("button-selected");
            canvas.style.cursor = "url(/web/assets/images/eraser.png), auto";
        }
    });

    canvas.addEventListener("mouseenter", function() {
        if (isPenToggled) {
            canvas.style.cursor = "url(/web/assets/images/pen.png), auto";
        } else if (isRubberToggled) {
            canvas.style.cursor = "url(/web/assets/images/eraser.png), auto";
        }
    });

    canvas.addEventListener("mouseleave", function() {
        canvas.style.cursor = "default"; // Restore default cursor when leaving canvas
    });

    canvas.addEventListener("mousedown", startAction);
    canvas.addEventListener("mousemove", performAction);
    document.addEventListener("mouseup", stopAction);

    function startAction(event) {
        if (isPenToggled) { // Left mouse button for drawing
            isDrawing = true;
        } else if (isRubberToggled) { // Ctrl key for erasing
            isErasing = true;
        }
        performAction(event);
    }

    function stopAction() {
        isDrawing = false;
        isErasing = false;
    }

    function performAction(event) {
        if (isDrawing && isPenToggled) {
            draw(event);
        } else if (isErasing && isRubberToggled) {
            erase(event);
        }
    }

    function draw(event) {
        let position = getPosition(event);
        if (!lastPosition) {
            lastPosition = position;
            return;
        }
        ctx.beginPath();
        ctx.lineWidth = 5;
        ctx.lineCap = 'round';
        ctx.strokeStyle = 'black';
        ctx.moveTo(lastPosition.x, lastPosition.y);
        ctx.lineTo(position.x, position.y);
        ctx.stroke();
        lastPosition = position;
    }

    function erase(event) {
        let position = getPosition(event);
        // Define the size of the erased area
        let eraseWidth = 20;
        let eraseHeight = 20;
        // Clear a larger region centered around the cursor position
        ctx.clearRect(position.x - eraseWidth / 2, position.y - eraseHeight / 2, eraseWidth, eraseHeight);
    }

    function getPosition(event) {
        const canvasRect = canvas.getBoundingClientRect();
        return {
            x: event.clientX - canvasRect.left,
            y: event.clientY - canvasRect.top
        };
    }
});
