
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

    this.update = function(node) {
        this.notify({update: node});
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
        for (var i in this.rootNodes) {
            if (this.rootNodes[i].path == path) {
                return this.rootNodes[i];
            }
        }
        return null;
    }

    this.addTransformToNodeWithPath = function(path, transform) {
        var node = this.findByPath(path);
        if (node) {
            // Only one prototype transform allowed
            if (transform.prototype) {
                for (i in node.transforms) {
                    if (node.transforms[i].prototype) {
                        throw(new Error('multiple prototype transforms not allowed'));
                    }
                }
            }

            node.transforms.push(transform);
            this.notify({updated: node});
        } else {
            throw(new Error('node eith path "' + path + '" not found'));
        }
    }

    this.removeTransformFromNodeWithPath = function(path, transform) {
        var node = this.findByPath(path);
        if (node) {
            node.transforms.splice(node.transforms.indexOf(transform),1);
            this.notify({updated: node});
        } else {
            throw(new Error('node eith path "' + path + '" not found'));
        }
    }
    
    this.iterate = function(iterator) {
        for (var i in this.rootNodes) {
            iterator(this.rootNodes[i]);
        }
    }

    Observable.makeObservable(this);
}