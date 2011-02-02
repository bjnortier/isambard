function Action(label, iconPath, fn) {
    this.label = label;
    this.iconPath = iconPath;
    this.fn = fn;

    this.render = function(toolbar) {

        var imgId = "action_" + label;
        toolbar.append('<img id="' + imgId + '" src="' + this.iconPath + '"/>');
        
        // Because 'this' is the HTML element inside the function below,
        // we have to use a reference
        var fn = this.fn;
        jQuery("#" + imgId).click(function() {
            fn();
        });
    }
}

function delete_geom() {
    var selected = selectionManager.selected();
    selectionManager.deselectAll();
    for (i in selected) {
        geom_doc.removeByPath(selected[i]);
        SceneJS.withNode(selected[i]).parent().remove({node: selected[i]});
    }
}


function create_primitive(type, keys) {
    var geometryParams = {};
    for (i in keys) {
        geometryParams[keys[i]] = null;
    }
    geom_doc.add(new GeomNode({
        type: type,
        prototype: true,
        parameters: geometryParams}));
}

function create_transform(type, keys) {
    if (selectionManager.size() != 1)  {
        alert("no object selected!");
        return;
    }
    var transformParams = {};
    for (i in keys) {
        transformParams[keys[i]] = null;
    }
    
    var path = selectionManager.selected()[0];

    geom_doc.addTransformToNodeWithPath(
        path,
        new Transform({
            type: type,
            prototype: true,
            parameters: transformParams
        }));
}


function add_to_scene(path, tesselation) {
    tesselation["type"] = "geometry";
    SceneJS.withNode("geom").add("node", {type: "material",
                                          id: path,
                                          emit: 0,
                                          baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                          specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                          specular:       0.9,
                                          shine:          100.0,
                                          nodes: [tesselation]});

    
    picker.addPickable(path);
}
    
function boolean(type) {
    if ((type == 'union') || (type == 'intersect')) {
        if (selectionManager.size() <= 1)  {
            alert("must have > 2 object selected!");
            return;
        }
    } else if (type =='subtract') {
        if (selectionManager.size() != 2)  {
            alert("must have 2 object selected!");
            return;
        }
    }
    var doFn = function() {
        var selected = selectionManager.selected();
        var geometry = {type: type,
                        children: selected
                       };
        
        $.ajax({
            type: "POST",
            url: "/geom/",
            contentType: "application/json",
            data: JSON.stringify(geometry),
            success: function(nodeData){
                var path = nodeData.path;
                $.ajax({
                    type: "GET",
                    url: path,
                    success: function(tesselation) {
                        var childNodes = selected.map(function(x) {
                            var node = geom_doc.findByPath(x);
                            geom_doc.remove(node);
                            return node;
                        });
                        geometry["path"] = path;
                        var boolNode = new GeomNode(geometry, childNodes);
                        geom_doc.add(boolNode);
                        
                        selected.map(function(x) {
                            SceneJS.withNode(x).parent().remove({node: x});
                        });
                        selectionManager.deselectAll();
                        add_to_scene(path, tesselation);
                    }
                });
            }
        })};
    var undoFn = function() {
    throw Error("not implemented");
}
    var cmd = new Command(doFn, undoFn);
    command_stack.execute(cmd);
}

$(document).ready(function() {

    /*
     * Basic
     */
    new Action("delete", "images/trash.png", 
               function(parameters) { delete_geom(); }).render($("#edit"));
    $('#edit').append('<a id="action_stl" href=""><img src="images/stl.png" alt="stl"></img></a>');
    $('#action_stl').click(function() {
        alert("select one object"); 
        return false;
    });
        
    
    /*
     * Primitives
     */
    new Action("cuboid", "/images/cuboid.png", 
               function() { create_primitive("cuboid",  ["width", "depth", "height"]); }).render($("#primitives"));
    new Action("sphere", "/images/sphere.png", 
               function(parameters) { create_primitive("sphere", ["radius"]); }).render($("#primitives"));
    new Action("cylinder", "/images/cylinder.png", 
               function(parameters) { create_primitive("cylinder", ["radius", "height"]); }).render($("#primitives"));
    new Action("cone", "/images/cone.png", 
               function(parameters) { create_primitive("cone", ["bottom_radius", "top_radius", "height"]); }).render($("#primitives"));
     new Action("wedge", "/images/wedge.png", 
                function(parameters) { create_primitive("wedge", ["x1", "x2", "y", "z"]); }).render($("#primitives"));
    new Action("torus", "/images/torus.png", 
               function(parameters) { create_primitive("torus", ["r1", "r2"]); }).render($("#primitives"));

    /*
     * Booleans
     */
    new Action("union", "/images/union.png", 
               function(parameters) { boolean("union"); }).render($("#boolean"));
    new Action("subtract", "/images/diff.png", 
               function(parameters) { boolean("subtract"); }).render($("#boolean"));
    new Action("intersect", "/images/intersect.png", 
               function(parameters) { boolean("intersect"); }).render($("#boolean"));
    
    /*
     * Transformations
     */
    new Action("translate", "/images/translate.png", 
               function(parameters) { create_transform("translate", ["dx", "dy", "dz"]); }).render($("#transforms"));
    new Action("scale", "/images/scale.png", 
               function(parameters) { create_transform("scale", ["x", "y", "z", "factor"]); }).render($("#transforms"));
    new Action("rotate", "/images/rotate.png", 
               function(parameters) { create_transform("rotate", ["px", "py", "pz", "vx", "vy", "vz", "angle"]); }).render($("#transforms"));
    new Action("mirror", "/images/mirror.png", 
               function(parameters) { create_transform("mirror", ["px", "py", "pz", "vx", "vy", "vz"]); }).render($("#transforms"));


    new Action("copy_translate", "/images/copy_translate.png", 
               function(parameters) { create_transform("copy_translate", ["dx", "dy", "dz", "n"]); }).render($("#transforms"));
    new Action("copy_rotate", "/images/copy_rotate.png", 
               function(parameters) { create_transform("copy_rotate", ["px", "py", "pz", "vx", "vy", "vz", "angle", "n"]); }).render($("#transforms"));



});

