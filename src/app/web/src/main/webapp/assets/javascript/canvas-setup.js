import { sendNewStroke, sendDeleteStroke } from "./whiteboard-websocket.js";
import { generateRandomId } from "./utils.js";
let currentColor = "#000000";
let currentWidth = 5;
let isMouseDown = false;
let lastErasedObject = null;
let isRubberToggled = false;
let canvas = null;

export let tempIdMap = {};
export let usersMap = {};
export let cursorPosition = { x: 0, y: 0 };

export const setupCanvas = () => {
  let hasWritePermission = document.getElementById("writePermission").value === "true";
  canvas = new fabric.Canvas("whiteboard", {
    isDrawingMode: hasWritePermission,
    perPixelTargetFind: true,
  });

  canvas.freeDrawingBrush.width = currentWidth;
  canvas.freeDrawingBrush.color = currentColor;

  const resizeCanvas = () => {
    canvas.setWidth(window.innerWidth);
    canvas.setHeight(window.innerHeight);
  };
  window.addEventListener("resize", resizeCanvas, false);
  resizeCanvas();

  canvas.on("mouse:move", (event) => {
    if(canvas.isDrawingMode) {
      canvas.freeDrawingCursor = 'url("/web/assets/images/pen.png"), auto';
    }
    if(isRubberToggled) {
      canvas.defaultCursor = 'url("/web/assets/images/eraser.png"), auto';
      canvas.hoverCursor = 'url("/web/assets/images/eraser.png"), auto';
    }
    let pointer = canvas.getPointer(event.e);
    cursorPosition.x = pointer.x;
    cursorPosition.y = pointer.y;
  });

  canvas.on("mouse:down", () => {
    isMouseDown = true;
  });

  canvas.on("mouse:up", () => {
    isMouseDown = false;
  });

  canvas.on("mouse:over", function (e) {
    if (isRubberToggled && isMouseDown) {
      const obj = e.target;
      if (obj && obj !== lastErasedObject) {
        console.log(obj);
        canvas.remove(obj);
        lastErasedObject = obj; // Avoid repeatedly removing the same object in quick succession
        sendDeleteStroke(obj.strokeId);
      }
    }
  });

  canvas.on("path:created", function (options) {
    const path = options.path;
    path.strokeId = generateRandomId();
    tempIdMap[path.strokeId] = path;
    sendNewStroke(path);
  });
};

export const addStroke = (jsonizedPath, strokeId) => {
  fabric.Path.fromObject(JSON.parse(jsonizedPath), function (path) {
    path.strokeId = strokeId;
    path.selectable = false;
    path.hasControls = false;
    path.hasBorders = false;
    canvas.add(path);
    canvas.renderAll();
  });
};

export const removeUserCursor = (username) => {
    let user = usersMap[username];
    if (!user) {
        return;
    }

    if (user.cursor && user.cursor.group) {
        canvas.remove(user.cursor.group);
        canvas.renderAll();
    }
}

export const updateUserCursor = (data, username) => {
  let user = usersMap[username];
  if (!user) {
    console.log("User not found:", username);
    return;
  }

  const { x, y } = data;

  // Check if the cursor group exists
  if (user.cursor && user.cursor.group) {
    user.cursor.group
      .set({
        left: x,
        top: y,
        visible: true,
      })
      .setCoords();
    user.cursor.group.bringToFront();
  } else {
    let circle = new fabric.Circle({
      radius: 5,
      fill: user.color,
      left: x,
      top: y,
      originX: "center",
      originY: "center",
      selectable: false,
    });
    let text = new fabric.Text(username, {
      fontSize: 14,
      left: x + 20,
      top: y,
      fill: user.color,
      selectable: false,
    });
    let cursorGroup = new fabric.Group([circle, text], {
      left: x,
      top: y,
      selectable: false,
      evented: false,
    });
    canvas.add(cursorGroup);
    cursorGroup.bringToFront(); // Ensure the new group is on top
    user.cursor = { circle: circle, text: text, group: cursorGroup };
  }

  canvas.renderAll();
};

export const deleteStroke = (strokeId) => {
  const object = canvas.getObjects().find((obj) => obj.strokeId === strokeId);
  if (object) {
    canvas.remove(object);
    canvas.renderAll();
  }
};

export const updateStroke = (oldStrokeId, newStrokeId) => {
  const object = canvas
    .getObjects()
    .find((obj) => obj.strokeId === oldStrokeId);
  if (object) {
    object.strokeId = newStrokeId;
    canvas.renderAll();
  }
};

export const penLogic = () => {
  canvas.isDrawingMode = true;
  canvas.selection = false;
  canvas.freeDrawingBrush.width = currentWidth;
  canvas.freeDrawingBrush.color = currentColor;
  isRubberToggled = false;
};

export const rubberLogic = () => {
  canvas.isDrawingMode = false;
  canvas.selection = false;
  isRubberToggled = true;

  canvas.forEachObject(function (obj) {
    obj.selectable = false;
  });
};
