function Observable() {
}

Observable.prototype.addListener = function(listener) {
    this.listeners.push(listener);
};

Observable.prototype.removeListener = function(listener) {
    this.listeners.splice(this.listeners.indexOf(listener),1);
};

Observable.prototype.notify = function(event) {
    for (var i in this.listeners) {
        this.listeners[i](event);
    }
};


Observable.makeObservable = function(obj) {
    obj.listeners = [];
    obj.addListener = Observable.prototype.addListener;
    obj.removeListener = Observable.prototype.removeListener;
    obj.notify = Observable.prototype.notify;
}



