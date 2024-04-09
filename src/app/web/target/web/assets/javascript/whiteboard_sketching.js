document.addEventListener("DOMContentLoaded", function() {
    // wait for the content of the window element
    // to load, then performs the operations.
    // This is considered best practice.
    window.addEventListener('load', ()=>{
        resize(); // Resizes the canvas once the window loads
        document.addEventListener('mousedown', startPainting);
        document.addEventListener('mouseup', stopPainting);
        document.addEventListener('mousemove', sketch);
        window.addEventListener('resize', resize);
    });


    const canvas = document.getElementById('whiteboard');

    // Context for the canvas for 2 dimensional operations
    const ctx = canvas.getContext('2d');

    // Resizes the canvas to the available size of the window.
    function resize(){
        ctx.canvas.width = window.innerWidth;
        ctx.canvas.height = window.innerHeight;
    }


    // Stores the initial position of the cursor
    let coords = {x:0 , y:0};

    // This is the flag that we are going to use to
    // trigger drawing
    let paint = false;

    // Updates the coordinates of the cursor when
    // an event e is triggered to the coordinates where
    // the said event is triggered.
    function getPosition(event){
        var canvasRect = canvas.getBoundingClientRect();

        // Calculate the relative position
        coords.x = event.clientX - canvasRect.left;
        coords.y = event.clientY - canvasRect.top;
        //coords.x = event.clientX; // - canvas.offsetLeft;
        //coords.y = event.clientY; //- canvas.offsetTop;
    }

    // The following functions toggle the flag to start
    // and stop drawing
    function startPainting(event){
        paint = true;
        getPosition(event);
    }
    function stopPainting(){
        paint = false;
    }

    function sketch(event) {
        if (!paint) return;
        ctx.beginPath();

        ctx.lineWidth = 5;

        // Sets the end of the lines drawn
        // to a round shape.
        ctx.lineCap = 'round';

        ctx.strokeStyle = 'black';

        // The cursor to start drawing
        // moves to this coordinate
        ctx.moveTo(coords.x, coords.y);

        // The position of the cursor
        // gets updated as we move the
        // mouse around.
        getPosition(event);

        // A line is traced from start
        // coordinate to this coordinate
        ctx.lineTo(coords.x, coords.y);

        // Draws the line.
        ctx.stroke();
    }
});