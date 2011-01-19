var Interaction = {}

Interaction.selected = [];

var picking = false;
var pickHit = false;
var shiftPicking = false;



Interaction.beforePick = function() {
    picking = true;
    pickHit = false;
}

Interaction.afterPick = function() {
    if (picking && !pickHit && !shiftPicking) {
        Interaction.unselect();
    }
    picking = false;
    shiftPicking = false;
}

Interaction.picked = function(path) {
    
    pickHit = true;
    var alreadySelected = false;
    
    for (var i in Interaction.selected) {
        if (Interaction.selected[i] == path) {
            alreadySelected = true;
        } 
    }

    if (shiftPicking) {
        if (alreadySelected) {
            Interaction.deselectPath(path);
        } else  {
            Interaction.selectPath(path);
        }
    } else {
        if (alreadySelected) {
            // Do nothing
        } else {
            Interaction.unselect();
            Interaction.selectPath(path);
        }
    }
    
}

Interaction.selectPath = function(path) {
    Interaction.selected.push(path);
    SceneJS.withNode(path).set("baseColor", { r: 1.0, g: 1.0, b: 0.0 });
    console.log("selected:" + Interaction.selected);
}

Interaction.deselectPath = function(path) {
    var newSelected = [];
    for (i in Interaction.selected) {
        if (Interaction.selected[i] != path) {
            newSelected.push(Interaction.selected[i]);
        }
    }
    Interaction.selected = newSelected;
    SceneJS.withNode(path).set("baseColor", { r: 0.5, g: 1.0, b: 0.0 });
    console.log("selected:" + Interaction.selected);
}

Interaction.unselect = function() {
    for (var i in Interaction.selected) {
        SceneJS.withNode(Interaction.selected[i]).set("baseColor", { r: 0.5, g: 1.0, b: 0.0 });
    }
    Interaction.selected = [];
}

Interaction.pickable = function(path) {
     SceneJS.withNode(path).bind("picked",
                                 function(event) {
                                     Interaction.picked(path);
                                 });
}

SceneJS.withNode("the-scene").bind("post-rendered",
                                 function(event) {
                                     Interaction.afterPick()
                                 });

var canvas = document.getElementById("theCanvas");

var lastX;
var lastY;
var firstX;
var firstY;

var dragging = false;
var panning = false;
var rotating = false;
var threshhold = 5; // inside this threshold is a single click

function mouseDown(event) {
    firstX = event.clientX;
    firstY = event.clientY;
    lastX = event.clientX;
    lastY = event.clientY
    dragging = true;
}

function mouseUp(event) {
    if (!panning && !rotating) {
        /* On mouse down, we render the scene in picking mode, passing in the 
         * mouse canvas coordinates. This will cause a scene render traversal in which
         * all the "picked" listeners will fire on nodes situated above whatever
         * geometry node was picked, as those nodes are visited.
         *
         */
        if (event.shiftKey) {
            shiftPicking = true;
        }
        var coords = clickCoordsWithinElement(event);
        Interaction.beforePick();
        SceneJS.withNode("the-scene").pick(coords.x, coords.y);
    }

    dragging = false;
    rotating = false;
    panning = false;
}

function mouseMove(event) {

    if (dragging && !(rotating || panning)) {
        if (event.button == 0) {
            if ((Math.abs(event.clientX - firstX) > threshhold)
                ||
                (Math.abs(event.clientY - firstY) > threshhold)) {

                rotating = true;
            }
        }
        if (event.button == 1) {
            if ((Math.abs(event.clientX - firstX) > threshhold)
                ||
                (Math.abs(event.clientY - firstY) > threshhold)) {
                panning = true;
            }
        }
    }

    if (rotating) {
        yaw_angle += (event.clientX - lastX) * 0.5;
        pitch_angle -= (event.clientY - lastY) * -0.5;
    }

    if (panning) {
        camera_translate.y += (event.clientX - lastX) * 0.01;
        camera_translate.z -= (event.clientY - lastY) * 0.01;
    }

    lastX = event.clientX;
    lastY = event.clientY;


}

function cancelEvent(e) {
  e = e ? e : window.event;
  if(e.stopPropagation)
    e.stopPropagation();
  if(e.preventDefault)
    e.preventDefault();
  e.cancelBubble = true;
  e.cancel = true;
  e.returnValue = false;
  return false;
}

function mouseWheel(e) {
    e = e ? e : window.event;
    var raw = e.detail ? e.detail : e.wheelDelta;
    var normal = e.detail ? e.detail * -1 : e.wheelDelta / 40;
    if (camera_translate.x + normal < 40) {
        camera_translate.x += normal;
    }
    cancelEvent(e);
}

canvas.addEventListener('DOMMouseScroll', mouseWheel, false);  
canvas.addEventListener('mousewheel', mouseWheel, false);

canvas.addEventListener('mousedown', mouseDown, true);
canvas.addEventListener('mousemove', mouseMove, true);
canvas.addEventListener('mouseup', mouseUp, true);

$('#theCanvas').keypress(function(event) {
    console.log(event);
});



function clickCoordsWithinElement(event) {
    var coords = { x: 0, y: 0};
    if (!event) {
        event = window.event;
        coords.x = event.x;
        coords.y = event.y;
    } else {
        var element = event.target ;
        var totalOffsetLeft = 0;
        var totalOffsetTop = 0 ;

        while (element.offsetParent)
        {
            totalOffsetLeft += element.offsetLeft;
            totalOffsetTop += element.offsetTop;
            element = element.offsetParent;
        }
        coords.x = event.pageX - totalOffsetLeft;
        coords.y = event.pageY - totalOffsetTop;
    }
    return coords;
}