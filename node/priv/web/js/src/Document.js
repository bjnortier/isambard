
function Document() {

    this.rootNodes = [];
    
    this.add = function(node) {
        this.rootNodes.push(node);
    }

    this.remove = function(node) {
        this.rootNodes.splice(this.rootNodes.indexOf(node),1);
    }
}