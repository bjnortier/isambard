

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
    };
    var redoFn = function() {
        throw Error('not implemented');
    }

    return new Command(doFn, undoFn, redoFn);
}


function create_geom_command(prototype, geometry) {
    
    var fns = 
        (function() {
            var id;
            var geomNode;
            var doFn = function() {
                $.ajax({
                    type: 'POST',
                    url: '/geom/',
                    contentType: 'application/json',
                    data: JSON.stringify(geometry),
                    success: function(nodeData){
                        var path = nodeData.path;
                        id = idForGeomPath(nodeData.path);
                        $.ajax({
                            type: 'GET',
                            url: '/tesselation/' + id,
                            success: function(tesselation) {
                                geomNode = new GeomNode({
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
                geom_doc.remove(geomNode);
            }
            var redoFn = function() {
                geom_doc.add(geomNode);
            }

            return { doFn : doFn,
                     undoFn : undoFn,
                     redoFn : redoFn }
        })();
            
    return new Command(fns.doFn, fns.undoFn, fns.redoFn);
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
    var fns = (function() {

        var id;
        var boolNode;
        var childNodes;

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
                    id = id = idForGeomPath(nodeData.path);
                    $.ajax({
                        type: "GET",
                        url: '/tesselation/' + id,
                        success: function(tesselation) {
                            childNodes = selected.map(function(x) {
                                var node = geom_doc.findByPath(x);
                                geom_doc.remove(node);
                                return node;
                            });
                            geometry["path"] = path;
                            boolNode = new GeomNode(geometry, childNodes);
                            boolNode.tesselation = tesselation;
                            selectionManager.deselectAll();
                            geom_doc.add(boolNode);
                        }
                    });
                }
            })};
        var undoFn = function() {
            geom_doc.remove(boolNode);
            childNodes.reverse().map(function(x) {
                geom_doc.add(x);
            });
        }
        var redoFn = function() {
            childNodes.map(function(x) {
                geom_doc.remove(x);
            });
            geom_doc.add(boolNode);
        }

        return {doFn : doFn,
                undoFn : undoFn,
                redoFn : redoFn}
    })();

    var cmd = new Command(fns.doFn, fns.undoFn, fns.redoFn);
    command_stack.execute(cmd);
}
