var command_stack = new CommandStack();
var geom_doc = new GeomDocument();
var treeView = new TreeView();

geom_doc.addListener(function(event) {
    treeView.geomDocUpdated(event);
});

selectionManager.addListener(function(event) {
    treeView.selectionUpdated(event);
});

geom_doc.addListener(function(event) {
    sceneView.geomDocUpdated(event);
});
selectionManager.addListener(function(event) {
    sceneView.selectionUpdated(event);
});

selectionManager.addListener(function(event) {
    sceneView.selectionUpdated(event);
});
selectionManager.addListener(function(event) {
    
    if (selectionManager.size() == 1) {
        $('#action_stl').unbind('click');
        var pattern = /^\/geom\/(.*)$/;
        var id = selectionManager.selected()[0].match(pattern)[1];
        $('#action_stl').attr('href', '/stl/' + id); 
    } else {
        $('#action_stl').unbind('click');
        $('#action_stl').click(function() {
            alert("select one object"); 
            return false;
        });
    }
});

function save() {
    var docId = $.getQueryParam("docid");
    var rootPaths = geom_doc.rootNodes.filter(function(x) {
        return !x.editing;
    }).map(function(x) {
        return x.path;
    });
    console.log(rootPaths);
    $.ajax({
        type: 'PUT',
        url: '/doc/' + docId,
        contentType: 'application/json',
        data: JSON.stringify(rootPaths),
        success: function() {
            console.log('saved');
        }
    });
}

$(document).ready(function() {
    var docId = $.getQueryParam("docid");
    if (docId == undefined) {
        alert('no document defined!');
        return;
    }
    // Retrieve the serialised document
    $.ajax({
        type: 'GET',
        url: '/doc/' + docId,
        dataType: 'json',
        success: function(geomPaths) {
            geomPaths.map(function(path) {
                console.log("loading " + path);
                $.ajax({
                    type: 'GET',
                    url: path,
                    dataType: 'json',
                    success: function(geomJson) {
                        var newNode = new GeomNode(geomJson);
                        $.ajax({
                            type: 'GET',
                            url: '/tesselation/' + idForGeomPath(path),
                            success: function(tesselation) {
                                newNode.tesselation = tesselation;
                                newNode.path = path;
                                geom_doc.add(newNode);
                            }
                        });

                    }
                });
            });
        }
    });

});
