function Transform() {
    if (!arguments[0].type) {
        throw new Error("type is not defined");
    }

    this.editing = arguments[0].editing;
    this.type = arguments[0].type;
    this.parameters = arguments[0].parameters;
}

Transform.prototype.editableCopy = function() {
    var copiedParameters = {};
    for (key in this.parameters) {
        copiedParameters[key] = this.parameters[key];
    }
    return new Transform({type : this.type,
                          parameters : copiedParameters,
                          editing : this.editing});
}

Transform.prototype.json = function() {
    return JSON.stringify({type: this.type,
                           parameters: this.parameters});
}

function GeomNode() {

    if (!arguments[0].type) {
        throw new Error("type is not defined");
    }

    this.editing = arguments[0].editing;
    this.type = arguments[0].type;
    this.path = arguments[0].path;
    this.parameters = arguments[0].parameters;
    this.transforms = arguments[0].transforms || [];
    this.tesselation = arguments[0].tesselation;
    this.children = [];

    if (arguments[1]) {
        if (!typeof(arguments[1]) == "object") {
            throw new Error("Children must be array");
        }

        for (var i in arguments[1]) {
            this.children.push(arguments[1][i]);
        }
    }
}

GeomNode.prototype.editableCopy = function() {

    var copiedParameters = {};
    for (key in this.parameters) {
        copiedParameters[key] = this.parameters[key];
    }
    var copiedTransforms = this.transforms.map(function(transform) {
        return transform.editableCopy();
    });
        
    var newNode = new GeomNode({type : this.type,
                                path : this.path,
                                parameters : copiedParameters,
                                transforms : copiedTransforms,
                                tesselation : this.tesselation,
                               }, this.children);
    return newNode;
}

// TODO: Move test for multiple prorotype transforms from doc
// into this class
    
GeomNode.prototype.json = function() {
    // No need to do somethign special with parameters if they are not 
    // defined, as JSON.stringigy simply ignores those fields
    return JSON.stringify({type: this.type,
                           parameters: this.parameters,
                           children: this.children.map(function(child) {
                               return child.path;
                           }),
                           transforms: this.transforms.map(function(tx) {
                               return JSON.parse(tx.json());
                           })});
}