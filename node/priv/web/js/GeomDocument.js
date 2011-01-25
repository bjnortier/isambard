
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

    this.iterate = function(iterator) {
        for (var i in this.rootNodes) {
            iterator(this.rootNodes[i]);
        }
    }

    Observable.makeObservable(this);
}