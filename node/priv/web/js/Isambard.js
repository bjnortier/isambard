var command_stack = new CommandStack();
var geom_doc = new GeomDocument();

function create_geom_command(parameters) {
    
    var doFn = function() {
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
                        geom_doc.add(new GeomNode({
                            type: parameters.type,
                            path: path,
                            parameters: parameters}));
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
    };
    var undoFn = function() {
        throw Error("not implemented");
    }
    return new Command(doFn, undoFn);
}


function GeomDocumentRenderer() {
    this.update = function() {

        $('#geom-model-doc').html('');
        var geomNodeRenderer = function(geomNode) {
            var nodeTable = $('<table>');
            var typeRow = $('<tr>').append('<td>' + geomNode.type + '</td>');

            nodeTable.append(typeRow);


            $('#geom-model-doc').append(nodeTable);
        }
        geom_doc.iterate(geomNodeRenderer);
    }
}

var geom_doc_renderer = new GeomDocumentRenderer();

geom_doc.addListener(function(event) {
    geom_doc_renderer.update();
});

