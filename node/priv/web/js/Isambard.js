var command_stack = new CommandStack();
var geom_doc = new GeomDocument();

function transform_geom_command(geomNode, transform) {
    var doFn = function() {
        $.ajax({
            type: 'PUT',
            url: geomNode.path,
            contentType: 'application/json',
            data: geomNode.json(),
            success: function(nodeData){
                var path = nodeData.path;
                $.ajax({
                    type: 'GET',
                    url: path,
                    success: function(tesselation) {
                        geom_doc.remove(prototype);
                        geom_doc.add(new GeomNode({
                            type: geometry.type,
                            path: path,
                            parameters: geometry.parameters}));

                        add_to_scene(path, tesselation);

                    }
                });
            }
        });
    };
    var undoFn = function() {
        throw Error('not implemented');
    }
    return new Command(doFn, undoFn);
}

function create_geom_command(prototype, geometry) {
    
    var doFn = function() {
        $.ajax({
            type: 'POST',
            url: '/geom/',
            contentType: 'application/json',
            data: JSON.stringify(geometry),
            success: function(nodeData){
                var path = nodeData.path;
                $.ajax({
                    type: 'GET',
                    url: path,
                    success: function(tesselation) {
                        geom_doc.remove(prototype);
                        geom_doc.add(new GeomNode({
                            type: geometry.type,
                            path: path,
                            parameters: geometry.parameters}));

                        add_to_scene(path, tesselation);

                    }
                });
            }
        });
    };
    var undoFn = function() {
        throw Error('not implemented');
    }
    return new Command(doFn, undoFn);
}

function renderTransform(transform) {
    var paramsArr = [];
    for (key in transform.parameters) {
        paramsArr.push({key: key,
                        value: transform.parameters[key],
                        prototype: transform.prototype
                       });
    }

    var template = '<table><tr><td><table>{{#paramsArr}}<tr><td>{{key}}</td><td>{{^prototype}}{{value}}{{/prototype}}{{#prototype}}<input id="{{key}}" type="text">{{/prototype}}</td></tr>{{/paramsArr}}</td></tr></table>{{#prototype}}<tr><td><input id="transform-ok" type="submit" value="Ok"/><input id="transform-cancel" type="submit" value="Cancel"/></td></tr>{{/prototype}}</table>';
    var transformTable = $.mustache(template, {paramsArr: paramsArr,
                                               prototype: transform.prototype});
    return transformTable;
}

function renderNode(geomNode) {
    // Params
    var paramsArr = [];
    for (key in geomNode.parameters) {
        paramsArr.push({key: key,
                        value: geomNode.parameters[key],
                        prototype: geomNode.prototype
                       });
    }
    var template = '<table>{{#paramsArr}}<tr><td>{{key}}</td><td>{{^prototype}}{{value}}{{/prototype}}{{#prototype}}<input id="{{key}}" type="text">{{/prototype}}</td></tr>{{/paramsArr}}</table>';
    var paramsTable = $.mustache(template, {paramsArr : paramsArr});

    // Transforms
    var transforms = geomNode.transforms.map(renderTransform);
    
    // Children
    var childTables = geomNode.children.map(renderNode);
    
    var template = '<table><tr><td>{{type}}</td></tr><tr><td>{{{paramsTable}}}</td></tr>{{#prototype}}<tr><td><input id="modal-ok" type="submit" value="Ok"/><input id="modal-cancel" type="submit" value="Cancel"/></td></tr>{{/prototype}}{{#transforms}}<tr><td>{{{.}}}</tr></td>{{/transforms}}{{#children}}<tr><td>{{{.}}}</td></td>{{/children}}</table>';
    var view = {type: geomNode.type,
                prototype: geomNode.prototype,
                paramsTable: paramsTable,
                transforms: transforms,
                children: childTables
               };
    var nodeTable = $.mustache(template, view);
    return nodeTable;
}


function update_geom_doc_tree() {

    $('#geom-model-doc').html('');
    var geomNodeRenderer = function(geomNode) {

        var nodeTable = renderNode(geomNode);
        $('#geom-model-doc').append(nodeTable);
        if (geomNode.prototype) {
            $('#modal-ok').click(function() {
                if (geomNode.prototype) {
                    var parameters = {};
                    for (key in geomNode.parameters) {
                        parameters[key] = parseFloat($('#' + key).val());
                    }
                    var cmd = create_geom_command(geomNode, {type: geomNode.type,
                                                             parameters: parameters});
                    command_stack.execute(cmd);
                }
            });
            $('#modal-cancel').click(function() {
                geom_doc.remove(geomNode);
            });
        }

        // Add the transform ok/cancel event functions. There can be only a 
        // single prorotype transform on a GeomNode
        for (i in geomNode.transforms) {
            if (geomNode.transforms[i].prototype) {
                var transform = geomNode.transforms[i];
                $('#transform-ok').click(function() {
                    for (key in transform.parameters) {
                        transform.parameters[key] = parseFloat($('#' + key).val());
                    }
                    var cmd = transform_geom_command(geomNode, transform);
                    command_stack.execute(cmd);
                }); 
                $('#transform-cancel').click(function() {
                    geom_doc.removeTransformFromNodeWithPath(geomNode.path, transform);
                });
            }
        }
    };

    geom_doc.iterate(geomNodeRenderer);
};


geom_doc.addListener(function(event) {
    update_geom_doc_tree();
});

/* Debug */
var cmd = create_geom_command(null, {type: 'sphere',
                                     parameters: {radius: 1.0}});
command_stack.execute(cmd);