

function update_geom_command(precursor, geomNode) {
    var ancestors = geom_doc.ancestors(geomNode);
    var ancestorCopies = ancestors.map(function(ancestor) {
        return ancestor.editableCopy();
    });

    var updateChain = [geomNode].concat(ancestors);
    var precursorChain = [precursor].concat(ancestorCopies);

    console.log('updateChain: ' + updateChain.map(function(node) { return node.path; }));
    
    var chainedPutFn = function() {
        var nextNode = updateChain.splice(0,1)[0];
        var nextPrecursor = precursorChain.splice(0,1)[0];
        if (nextNode) {
            $.ajax({
                type: 'PUT',
                url: nextNode.path,
                contentType: 'application/json',
                data: nextNode.json(),
                success: function(nodeData) {
                    if (nextNode.editing) {
                        nextNode.editing = false;
                    }
                    for (var i in nextNode.transforms) {
                        if (nextNode.transforms[i].editing) {
                            nextNode.transforms[i].editing = false;
                        }
                    }
                    if (updateChain.length > 0) {
                        chainedPutFn();
                    } else {
                        // No more -> update the root node
                        $.ajax({
                            type: 'GET',
                            url: '/tesselation/' + idForGeomPath(nodeData.path),
                            success: function(tesselation) {
                                nextNode.tesselation = tesselation;
                                selectionManager.deselectAll();
                                geom_doc.replace(nextPrecursor, nextNode);
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
                    url: '/tesselation/' + idForGeomPath(nodeData.path),
                    success: function(tesselation) {
                        var geomNode = new GeomNode({
                            type : geometry.type,
                            path : path,
                            parameters : geometry.parameters,
                            tesselation : tesselation})
                        selectionManager.deselectAll();
                        geom_doc.replace(prototype, geomNode);
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
                    url: '/tesselation/' + idForGeomPath(nodeData.path),
                    success: function(tesselation) {
                        var childNodes = selected.map(function(x) {
                            var node = geom_doc.findByPath(x);
                            geom_doc.remove(node);
                            return node;
                        });
                        geometry["path"] = path;
                        var boolNode = new GeomNode(geometry, childNodes);
                        boolNode.tesselation = tesselation;
                        selectionManager.deselectAll();
                        geom_doc.add(boolNode);
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
