/*
 This example demonstrates how to define geometry, in this case a simple cube
 object that supports texturing.

 Lindsay S. Kay,
 lindsay.kay@xeolabs.com

 This example assumes that you have looked at a few of the other examples
 and now have an understanding of concepts such as basic SceneJS syntax,
 lighting, material, data flow etc.

 Scroll down to the SceneJS.geometry node about one third of the way downurl
 this file and I'll guide you from there.

 */
//var geomNode = 

var eye = {x: 50.0, y: 0.0, z: 0.0};
var look = {x: 0.0, y:0.0, z:0.0};
var up = {z: 1.0};
var yaw_angle = -45;
var pitch_angle = 20;
var camera_translate = {x: 0, y:0, z:0};


var scene = new SceneJS.Scene(
    {
        id: "the-scene",
        canvasId: "theCanvas",
        loggingElementId: "theLoggingDiv"
    },
    new SceneJS.LookAt(
        {
            id : "lookat",
            eye : eye,
            look : look,
            up : up
        },
        new SceneJS.Camera(
            {
                type: "camera",
                optics: {
                    type: "perspective",
                    fovy : 5.0,
                    aspect : 1.47,
                    near : 0.10,
                    far : 300.0
                }
            },
            new SceneJS.Light(
                {
                    mode:                   "dir",
                    color:                  { r: 0.5, g: 0.5, b: 0.5 },
                    diffuse:                true,
                    specular:               true,
                    dir:                    { x: -1.0, y: -1.0, z: 1.0 }
                }),
            new SceneJS.Light(
                {
                    mode:                   "dir",
                    color:                  { r: 0.7, g: 0.7, b: 0.7 },
                    diffuse:                true,
                    specular:               true,
                    dir:                    { x: 0.0, y: 1.0, z: -1.0 }
                }),
            new SceneJS.Light(
                {
                    mode:                   "dir",
                    color:                  { r: 0.8, g: 0.8, b: 0.8 },
                    diffuse:                true,
                    specular:               true,
                    dir:                    { x: -1.0, y: -1.0, z: 1.0 }
                }),
            new SceneJS.Translate(
                {
                    id : "camera-translate",
                    x : camera_translate.x,
                    y : camera_translate.y,
                    z : camera_translate.z
                },
                new SceneJS.Rotate(
                    {
                        id: "pitch",
                        angle: pitch_angle,
                        y : 1.0
                    },
                    new SceneJS.Rotate(
                        {
                            id: "yaw",
                            angle: yaw_angle,
                            z : 1.0
                        },
                        new SceneJS.Material(
                            {
                                id: "x-axis",
                                emit: 1,
                                baseColor:      { b: 1.0 },
                                specular:       0,
                            },
                            new SceneJS.Geometry(
                                {
                                    primitive: "lines",
                                    positions : [
                                        0,0,0,
                                        1000,0,0
                                    ],
                                    normals : [],
                                    uv : [],
                                    uv2 : [],
                                    indices : [0,1]
                                })),
                        new SceneJS.Material(
                            {
                                id: "y-axis",
                                emit: 1,
                                baseColor:      { g: 1.0 },
                                specular:       0,
                            },
                            new SceneJS.Geometry(
                                {
                                    primitive: "lines",
                                    positions : [
                                        0,0,0,
                                        0,1000,0
                                    ],
                                    normals : [],
                                    uv : [],
                                    uv2 : [],
                                    indices : [0,1]
                                })),
                        new SceneJS.Material(
                            {
                                id: "z-axis",
                                emit: 1,
                                baseColor:      { r: 1.0 },
                                specular:       0,
                            },
                            new SceneJS.Geometry(
                                {
                                    primitive: "lines",
                                    positions : [
                                        0,0,0,
                                        0,0,1000
                                    ],
                                    normals : [],
                                    uv : [],
                                    uv2 : [],
                                    indices : [0,1]
                                })),
                        new SceneJS.Material(
                            {
                                id: "geom",
                                emit: 0,
                                baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                specular:       0.9,
                                shine:          100.0,
                            }
                        )
                    )
                )
            )
        )
    )
);
              



window.render = function() {
    
    SceneJS.withNode("pitch").set("angle", pitch_angle);
    SceneJS.withNode("yaw").set("angle", yaw_angle);
    SceneJS.withNode("camera-translate").set("x", camera_translate.x);
    SceneJS.withNode("camera-translate").set("y", camera_translate.y);
    SceneJS.withNode("camera-translate").set("z", camera_translate.z);

    SceneJS.withNode("the-scene").render();
};

/* Render loop until error or reset
 * (which IDE does whenever you hit that run again button)
 */
var pInterval;

SceneJS.bind("error", function() {
    window.clearInterval(pInterval);
});

SceneJS.bind("reset", function() {
    window.clearInterval(pInterval);
});

pInterval = window.setInterval("window.render()", 10);







