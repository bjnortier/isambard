function Action(label, iconPath, parameters, okFn) {
    this.label = label;
    this.iconPath = iconPath;
    this.parameters = parameters;
    this.okFn = okFn;

    this.render = function(toolbar) {

        var imgId = "action_" + label;
        toolbar.append('<img id="' + imgId + '" src="' + this.iconPath + '"/>');
        
        // Because 'this' is the HTML element inside the function below,
        // we have to save a reference
        var okFn = this.okFn;
        var parameters = this.parameters;
        jQuery('#' + imgId).click(function() {
            if (parameters.length > 0) {
                open_dialog(parameters, okFn);
            } else {
                okFn();
            }
        });
    }
}

function delete_geom() {
    for (i in Interaction.selected) {
        SceneJS.withNode(Interaction.selected[i]).parent().remove({node: Interaction.selected[i]});
    }
    Interaction.unselect();
}


function create_primitive(parameters, type) {
    parameters['type'] = type;
    create_geom(parameters);
}


function create_geom(parameters) {

    $.ajax({
        type: "POST",
        url: "/geom/",
        contentType: "application/json",
        data: JSON.stringify(parameters),
        success: function(nodeData){
            var path = nodeData.path;
            $.ajax({
                type: "GET",
                url: path,
                success: function(nodeData) {
                    /* FIXME: The picking doesn't seem to work unless there is an 
                       extra node above the geometry node? */
                    nodeData['type'] = 'geometry';
                    SceneJS.withNode("geom").add("node", {type: "material",
                                                          id: path,
                                                          emit: 0,
                                                          baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                                          specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                                          specular:       0.9,
                                                          shine:          100.0,
                                                          nodes: [nodeData]});

                    Interaction.pickable(path);
                }
            });
        }
    });
}


function boolean(type) {
    if (Interaction.selected.length != 2)  {
        alert('must have 2 object Interaction.selected!');
        return;
    }
    $.ajax({
        type: "POST",
        url: "/geom/",
        contentType: "application/json",
        data: JSON.stringify({type: type,
                              a: Interaction.selected[0],
                              b: Interaction.selected[1]
                             }),
        success: function(nodeData){
            var path = nodeData.path;
            $.ajax({
                type: "GET",
                url: path,
                success: function(nodeData) {
                    SceneJS.withNode(Interaction.selected[0]).parent().remove({node: Interaction.selected[0]});
                    SceneJS.withNode(Interaction.selected[1]).parent().remove({node: Interaction.selected[1]});
                    Interaction.unselect();

                    /* FIXME: The picking doesn't seem to work unless there is an 
                       extra node above the geometry node? */
                    SceneJS.withNode("geom").add("node", {type: "material",
                                                          id: path,
                                                          emit: 0,
                                                          baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                                          specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                                          specular:       0.9,
                                                          shine:          100.0,
                                                          nodes: [nodeData]});
                    Interaction.pickable(path);

                }
            });
        }
    });
}

function transform(parameters, type) {
    if (Interaction.selected.length != 1)  {
        alert('must have 1 object selected!');
        return;
    }
    parameters['type'] = type;
    parameters['path'] = Interaction.selected[0];

    $.ajax({
        type: "POST",
        url: "/geom/",
        contentType: "application/json",
        data: JSON.stringify(parameters),
        success: function(nodeData){
            var path = nodeData.path;
            $.ajax({
                type: "GET",
                url: path,
                success: function(nodeData) {
                    SceneJS.withNode(Interaction.selected[0]).parent().remove({node: Interaction.selected[0]});
                    Interaction.unselect();

                    /* FIXME: The picking doesn't seem to work unless there is an 
                       extra node above the geometry node? */
                    SceneJS.withNode("geom").add("node", {type: "material",
                                                          id: path,
                                                          emit: 0,
                                                          baseColor:      { r: 0.5, g: 1.0, b: 0.0 },
                                                          specularColor:  { r: 0.9, g: 0.9, b: 0.9 },
                                                          specular:       0.9,
                                                          shine:          100.0,
                                                          nodes: [nodeData]});
                    Interaction.pickable(path);
                }
            });
        }
    });
}


function open_dialog(parameters, okFn) {

    $( "#dialog:ui-dialog" ).dialog( "destroy" );

    function checkFloat( o, n) {
        var result = true;
            try {
                if (o.val().length == 0) {
                    result = false;
                }
                if (isNaN(parseFloat(o.val()))) {
                    result = false;
                }
            } catch(e) {
                result = false;
	    }
        if (result == false) {
            o.addClass( "ui-state-error" );
        }
        return result;
    }

    var form = '<form><fieldset>';
    
    for (i in parameters) {
        var parameter = parameters[i];
        var field = '<label for="' + parameter.name + '">' + parameter.label + '</label><input type="text" name="' + parameter.name + '" id="dialog-' + parameter.name + '" class="text ui-widget-content ui-corner-all"/><br/>';
        form += field;
    }
    form += '</fieldset></form>';
    $('#dialog').html(form);

    $( "#dialog" ).dialog({
	autoOpen: false,
	width: 250,
	height: 300,
	modal: true,
	buttons: {
	    Ok : function() {


		var bValid = true;
		for (i in parameters) {
                    var input = $('#dialog-' + parameters[i].name);
                    input.removeClass( "ui-state-error" );
                    bValid = bValid & checkFloat(input);
                }
                
		if ( bValid ) {
                    var result =  {};
                    for (i in parameters) {
                        var parameter = parameters[i];
                        result[parameter.name] = parseFloat($('#dialog-' + parameter.name).val());
                    }
                    
                    okFn(result);
		    $( this ).dialog( "close" );
		}
	    },
	    Cancel : function() {
		$( this ).dialog( "close" );
	    }
	},
	close: function() {
	}
    });

    $("#dialog" ).dialog( "open" );
}

$(document).ready(function() {

    /*
     * Edit
     */
    new Action('delete', 'images/trash.png', 
               [],
               function(parameters) { delete_geom(); }).render($('#edit'));
    
    /*
     * Primitives
     */
    new Action('cuboid', 'images/cuboid.png', 
               [{name: "width", label: "Width (X)"},
                {name: "depth", label: "Depth (Y)"},
                {name: "height", label: "Height (Z)"}
               ],
               function(parameters) { create_primitive(parameters, "cuboid"); }).render($('#primitives'));
    new Action('sphere', 'images/sphere.png', 
               [{name: "radius", label: "Radius"}
               ],
               function(parameters) { create_primitive(parameters, "sphere"); }).render($('#primitives'));
    new Action('cylinder', 'images/cylinder.png', 
               [{name: "radius", label: "Radius (XY)"},
                {name: "height", label: "Height (Z)"}
               ],
               function(parameters) { create_primitive(parameters, "cylinder"); }).render($('#primitives'));
    new Action('cone', 'images/cone.png', 
               [{name: "bottom_radius", label: "Bottom radius (XY)"},
                {name: "top_radius", label: "Top radius (XY)"},
                {name: "height", label: "Height (Z)"}
               ],
               function(parameters) { create_primitive(parameters, "cone"); }).render($('#primitives'));
     new Action('wedge', 'images/wedge.png', 
               [{name: "x1", label: "X1"},
                {name: "x2", label: "X2"},
                {name: "y", label: "Y"},
                {name: "z", label: "Z"},
               ],
               function(parameters) { create_primitive(parameters, "wedge"); }).render($('#primitives'));
    new Action('torus', 'images/torus.png', 
               [{name: "r1", label: "Radius to tube (XY)"},
                {name: "r2", label: "Tube radius (Z)"}
               ],
               function(parameters) { create_primitive(parameters, "torus"); }).render($('#primitives'));

    /*
     * Booleans
     */
    new Action('union', 'images/union.png', 
               [],
               function(parameters) { boolean("union"); }).render($('#boolean'));
    new Action('difference', 'images/diff.png', 
               [],
               function(parameters) { boolean("diff"); }).render($('#boolean'));
    new Action('intersect', 'images/intersect.png', 
               [],
               function(parameters) { boolean("intersect"); }).render($('#boolean'));
    
    /*
     * Transformations
     */
    new Action('translate', 'images/translate.png', 
               [{name: "dx", label: "dX"},
                {name: "dy", label: "dY"},
                {name: "dz", label: "dZ"}],
               function(parameters) { transform(parameters, "translate"); }).render($('#transforms'));
    new Action('scale', 'images/scale.png', 
               [{name: "x", label: "X"},
                {name: "y", label: "Y"},
                {name: "z", label: "Z"},
                {name: "factor", label: "Factor"},],
               function(parameters) { transform(parameters, "scale"); }).render($('#transforms'));
    new Action('rotate', 'images/rotate.png', 
               [{name: "x", label: "Position X"},
                {name: "y", label: "Position Y"},
                {name: "z", label: "Position Z"},
                {name: "vx", label: "Axis X"},
                {name: "vy", label: "Axis Y"},
                {name: "vz", label: "Axis Z"},
                {name: "angle", label: "Angle (deg)"},],
               function(parameters) { transform(parameters, "rotate"); }).render($('#transforms'));


});

