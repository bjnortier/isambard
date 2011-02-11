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
