var command_stack = new CommandStack();
var geom_doc = new GeomDocument();

function update_geom_command(geomNode) {
    var updateChain = [geomNode];
    var parent = geomNode.parent;
    while(parent) {
        updateChain.push(parent);
        parent = parent.parent;
    }
    console.log('updateChain: ' + updateChain.map(function(node) { return node.path; }));
    
    var chainedPutFn = function() {
        var nextNode = updateChain.splice(0,1)[0];
        if (nextNode) {
            $.ajax({
                type: 'PUT',
                url: nextNode.path,
                contentType: 'application/json',
                data: nextNode.json(),
                success: function(nodeData) {
                    nextNode.prototype = false;
                    if (updateChain.length > 0) {
                        chainedPutFn();
                    } else {
                        // No more -> update the root node
                        $.ajax({
                            type: 'GET',
                            url: nextNode.path,
                            success: function(tesselation) {
                                geom_doc.update(nextNode);
                                selectionManager.deselectAll();
                                SceneJS.withNode(nextNode.path).parent().remove({node: nextNode.path});
                                add_to_scene(nextNode.path, tesselation);
                            }
                        });
                    }
                }
            });
        }
    }

    var doFn = function() {
        chainedPutFn();
    };
    var undoFn = function() {
        throw Error('not implemented');
    }
    return new Command(doFn, undoFn);
}


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
                        geom_doc.update(geomNode);
                        selectionManager.deselectAll();
                        SceneJS.withNode(geomNode.path).parent().remove({node: geomNode.path});
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

function renderTransform(geomNode, transformIndex) {
    var paramsArr = [];
    var transform = geomNode.transforms[transformIndex];
    var id = idForGeomNode(geomNode);
    for (key in transform.parameters) {
        paramsArr.push({key: key,
                        value: transform.parameters[key],
                        clazz: 'edit-transform target-' + id + '-' + transformIndex,
                        prototype: transform.prototype
                       });
    }
    
    var template = '<table><tr><td>{{type}}</td></tr><tr><td><table>{{#paramsArr}}<tr><td>{{key}}</td><td>{{^prototype}}<span class="{{clazz}}">{{value}}</span>{{/prototype}}{{#prototype}}<input id="{{key}}" type="text" value="{{value}}">{{/prototype}}</td></tr>{{/paramsArr}}</td></tr></table>{{#prototype}}<tr><td><input id="transform-ok" type="submit" value="Ok"/><input id="transform-cancel" type="submit" value="Cancel"/></td></tr>{{/prototype}}</table>';
    var transformTable = $.mustache(template, {
        type: transform.type,
        paramsArr: paramsArr,
        prototype: transform.prototype});
    return transformTable;
}

function idForGeomNode(geomNode) {
    var id = 'prototype_id';
    if (geomNode.path) {
        id = idForGeomPath(geomNode.path);
    }
    return id;
}

function idForGeomPath(path) {
    var pattern = /^\/geom\/(.*)$/;
    return path.match(pattern)[1];
}

function renderNode(geomNode) {
    // Params
    var paramsArr = [];
    if (!((geomNode.type == 'union')
          ||
          (geomNode.type == 'subtract')
          ||
          (geomNode.type == 'subtract'))) {

        var id = idForGeomNode(geomNode);
        for (key in geomNode.parameters) {

            paramsArr.push({key: key,
                            value: geomNode.parameters[key],
                            clazz: 'edit-geom target-' + id,
                            prototype: geomNode.prototype
                           });
        }
    }
    var template = '<table>{{#paramsArr}}<tr><td>{{key}}</td><td>{{^prototype}}<span class="{{clazz}}">{{value}}</span>{{/prototype}}{{#prototype}}<input id="{{key}}" type="text" value="{{value}}"/>{{/prototype}}</td></tr>{{/paramsArr}}</table>';
    var paramsTable = $.mustache(template, {paramsArr : paramsArr});

    // Transforms
    var transformRows = []
    for (var i in geomNode.transforms) {
        transformRows.push(renderTransform(geomNode, i));
    };

    
    // Children
    var childTables = geomNode.children.map(renderNode);
    
    var template = '<table id="{{id}}"><tr><td><img class="show-hide-siblings siblings-showing" src="/images/arrow_showing.png"></img>{{^prototype}}<span class="{{clazz}}">{{type}}</span>{{/prototype}}{{#prototype}}{{type}}{{/prototype}}</td></tr><tr><td>{{{paramsTable}}}</td></tr>{{#prototype}}<tr><td><input id="modal-ok" type="submit" value="Ok"/><input id="modal-cancel" type="submit" value="Cancel"/></td></tr>{{/prototype}}{{#transformRows}}<tr><td>{{{.}}}</tr></td>{{/transformRows}}{{#children}}<tr><td>{{{.}}}</td></td>{{/children}}</table>';
    var geomId = idForGeomNode(geomNode);
    var view = {type: geomNode.type,
                prototype: geomNode.prototype,
                id: geomId,
                paramsTable: paramsTable,
                transformRows: transformRows,
                clazz: 'select-geom target-' + geomId,
                children: childTables
               };
    var nodeTableContents = $.mustache(template, view);
    return nodeTableContents;
}



/* Debug */
/*var cmd = create_geom_command(null, {type: 'sphere',
                                     parameters: {radius: 1.0}});
command_stack.execute(cmd);*/


var treeView = new TreeView();

geom_doc.addListener(function(event) {
    treeView.geomDocUpdated(event);
});

selectionManager.addListener(function(event) {
    treeView.selectionUpdated(event);
});