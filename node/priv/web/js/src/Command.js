
function Command(doFn, undoFn) {
    var _doFn = doFn;
    var _undoFn = undoFn;
    this.do = function() { _doFn(); };
    this.undo = function() { _undoFn(); };
}