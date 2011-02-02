
function TreeView() {

    this.domNodeLookup = {};

    this.addEvents = function(geomNode) {

        if (geomNode.prototype) {
            $('#modal-ok').click(function() {
                if (geomNode.prototype) {
                    var cmd;
                    if (geomNode.path) {
                        for (key in geomNode.parameters) {
                            geomNode.parameters[key] = parseFloat($('#' + key).val());
                        }
                        cmd = update_geom_command(geomNode);
                    } else {
                        var parameters = {};
                        for (key in geomNode.parameters) {
                            parameters[key] = parseFloat($('#' + key).val());
                        }
                        cmd = create_geom_command(geomNode, {type: geomNode.type,
                                                             parameters: parameters});
                    }
                    command_stack.execute(cmd);
                }
            });
            $('#modal-cancel').click(function() {
                if (geomNode.path) {
                    // It an existing node, remove prototype designation
                    // and update
                    geomNode.prototype = false;
                    geom_doc.update(geomNode);
                } else {
                    // It's a new node, remove it
                    geom_doc.remove(geomNode);
                }
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
                        transform.prototype = false;
                    }
                    var cmd = transform_geom_command(geomNode, transform);
                    command_stack.execute(cmd);
                }); 
                $('#transform-cancel').click(function() {
                    geom_doc.removeTransformFromNodeWithPath(geomNode.path, transform);
                });
            }
        }

        $("tr:nth-child(even)").addClass("even");
        $("tr:nth-child(odd)").addClass("odd");

        $('.select-geom').click(function(event) {
            var id;
            var pattern = /^target-(.*)$/;
            var classes = $(this).attr('class').split(' ');
            for (i in classes) {
                var match = classes[i].match(pattern);
                if (match) {
                    id = match[1];
                }
            }
            if (!id) {
                throw Error('id for editing could not be determined');
            }
            selectionManager.picked('/geom/' + id);
        });

        // Edit geom
        $('.edit-geom').dblclick(function() { 
            var id;
            var pattern = /^target-(.*)$/;
            var classes = $(this).attr('class').split(' ');
            for (i in classes) {
                var match = classes[i].match(pattern);
                if (match) {
                    id = match[1];
                }
            }
            if (!id) {
                throw Error('id for editing could not be determined');
            }
            var geomNode = geom_doc.findByPath('/geom/' + id);
            geomNode.prototype = true;
            geom_doc.update(geomNode);
        });

        // Edit transform
        $('.edit-transform').dblclick(function() { 
            var id;
            var transformIndex;
            var pattern = /^target-(.*)-(.*)$/;
            var classes = $(this).attr('class').split(' ');
            for (i in classes) {
                var match = classes[i].match(pattern);
                if (match) {
                    id = match[1];
                    transformIndex = match[2];
                }
            }
            if (!id) {
                throw Error('id for editing could not be determined');
            }
            if (!transformIndex) {
                throw Error('transformIndex for editing could not be determined');
            }
            var geomNode = geom_doc.findByPath('/geom/' + id);
            geomNode.transforms[transformIndex].prototype = true;
            geom_doc.update(geomNode);
        });

        // Show/Hide
        $('#' + idForGeomNode(geomNode) + ' .show-hide-siblings').click(function() {
                if ($(this).hasClass('siblings-showing')) {
                    $(this).attr('src', '/images/arrow_hidden.png');
                    $(this).removeClass('siblings-showing');
                    var otherRows = $(this).parent().parent().siblings();
                    otherRows.hide();
                } else {
                    $(this).attr('src', '/images/arrow_showing.png');
                    $(this).addClass('siblings-showing');
                    var otherRows = $(this).parent().parent().siblings();
                otherRows.show();
                }
                return false;
            });
        
    }

    this.geomDocUpdated = function(event) {
        if (event.add) {
            var geomNode = event.add;

            var nodeTable = renderNode(geomNode);

            this.domNodeLookup[geomNode] = nodeTable;
            $('#geom-model-doc').prepend(nodeTable);
            this.addEvents(geomNode);
        }

        if (event.remove) {
            var geomNode = event.remove;
            var id = idForGeomNode(geomNode);
            $('#' + id).remove();
            delete this.domNodeLookup[geomNode];
        }

        if (event.update) {
            var geomNode = event.update;
            var nodeTable = renderNode(geomNode);
            $('#' + idForGeomNode(geomNode)).replaceWith(nodeTable);
            this.addEvents(geomNode);
        }
    }

    this.selectionUpdated = function(event) {
        if (event.deselected) {
            var deselected = event.deselected;
            for (i in deselected) {
                var id = idForGeomPath(deselected[i]);
                $('#' + id + ' > tbody > tr:nth-child(1)').removeClass('selected');
            }
        }
        if (event.selected) {
            var selected = event.selected;
            for (i in selected) {
                var id = idForGeomPath(selected[i]);
                $('#' + id + ' > tbody > tr:nth-child(1)').addClass('selected');
            }
        }

    }
}

