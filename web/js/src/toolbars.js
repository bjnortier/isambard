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

function undo() {
    command_stack.undo();
}

function redo() {
    command_stack.redo();
}

function delete_geom() {
    var selected = selectionManager.selected();
    selectionManager.deselectAll();
    for (var i in selected) {
        geom_doc.removeByPath(selected[i]);
    }
}


function create_primitive(type, keys) {
    var geometryParams = {};
    for (var i in keys) {
        geometryParams[keys[i]] = null;
    }
    geom_doc.add(new GeomNode({
        type: type,
        editing: true,
        parameters: geometryParams}));
}

function create_transform(type, keys) {
    if (selectionManager.size() != 1)  {
        alert("no object selected!");
        return;
    }
    var transformParams = {};
    for (var i in keys) {
        transformParams[keys[i]] = null;
    }
    
    var path = selectionManager.selected()[0];
    
    var original = geom_doc.findByPath(path);
    var replacement = original.editableCopy();
    replacement.transforms.push(new Transform({
        type: type,
        editing: true,
        parameters: transformParams
    }));
    geom_doc.replace(original, replacement);
       
}


$(document).ready(function() {

    /*
     * Basic
     */
    new Action("save", "images/save.png", 
               function(parameters) { save(); }).render($("#edit"));
    new Action("delete", "images/trash.png", 
               function(parameters) { delete_geom(); }).render($("#edit"));
    new Action("undo", "images/undo.png", 
               function(parameters) { undo(); }).render($("#edit"));
    new Action("redo", "images/redo.png", 
               function(parameters) { redo(); }).render($("#edit"));
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

    /*
     * Copy & Transform
     */
    new Action("copy_translate", "/images/copy_translate.png", 
               function(parameters) { create_transform("copy_translate", ["dx", "dy", "dz", "n"]); }).render($("#copy_transforms"));
    new Action("copy_rotate", "/images/copy_rotate.png", 
               function(parameters) { create_transform("copy_rotate", ["px", "py", "pz", "vx", "vy", "vz", "angle", "n"]); }).render($("#copy_transforms"));
    new Action("copy_mirror", "/images/copy_mirror.png", 
               function(parameters) { create_transform("copy_mirror", ["px", "py", "pz", "vx", "vy", "vz"]); }).render($("#copy_transforms"));




});

