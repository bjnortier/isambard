

function GeomNode() {

    if (!arguments[0].type) {
        throw new Error("type is not defined");
    }

    if (!arguments[0].path) {
        throw new Error("path is not defined");
    }


    this.type = arguments[0].type;
    this.path = arguments[0].path;
    this.parameters = arguments[0].parameters;
    this.parent = undefined;

    this.children = [];
    for (var i = 1; i < arguments.length; ++i) {
        arguments[i].parent = this;
        this.children.push(arguments[i]);
    }
    
    this.json = function() {
        // No need to do somethign special with parameters if they are not 
        // defined, as JSON.stringigy simply ignores those fields
        return JSON.stringify({type: this.type,
                               parameters: this.parameters});
    }
    
}