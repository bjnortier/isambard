
function GeomDocument() {

    this.rootNodes = [];
    
    this.add = function(node) {
        this.rootNodes = [node].concat(this.rootNodes);
        this.notify({add: node});
    }

    this.remove = function(node) {
        this.rootNodes.splice(this.rootNodes.indexOf(node),1);
        this.notify({remove: node});
    }

    this.replace = function(original, replacement) {
        this.rootNodes.splice(this.rootNodes.indexOf(original), 1, replacement);
        this.notify({replace: {original : original,
                               replacement : replacement}});
    }

    this.removeByPath = function(path) {
        var toRemove = [];
        for (var i in this.rootNodes) {
            if (this.rootNodes[i].path == path) {
                toRemove.push(this.rootNodes[i]);
            }
        }
        for (var i in toRemove) {
            this.remove(toRemove[i]);
        }
    }

    this.findByPath = function(path) {
        var recurFn = function(geomNode) {
            if (geomNode.path == path) {
                return geomNode;
            } else {
                for (var i in geomNode.children) {
                    var foundChild = recurFn(geomNode.children[i]);
                    if (foundChild) {
                        return foundChild;
                    } 
                }
                return null;
            }
        }
        for (var i in this.rootNodes) {
            var found = recurFn(this.rootNodes[i]);
            if (found) {
                return found;
            }
        }
        return null;
    }

    this.removeTransformFromNodeWithPath = function(path, transform) {
        var node = this.findByPath(path);
        if (node) {
            node.transforms.splice(node.transforms.indexOf(transform),1);
            this.notify({update: node});
        } else {
            throw(new Error('node with path "' + path + '" not found'));
        }
    }
    
    this.iterate = function(iterator) {
        for (var i in this.rootNodes) {
            iterator(this.rootNodes[i]);
        }
    }

    Observable.makeObservable(this);
}