
function Command(doFn, undoFn) {
    var _doFn = doFn;
    var _undoFn = undoFn;
    this.do = function() { _doFn(); };
    this.undo = function() { _undoFn(); };
}

function CommandStack() {
    var commands = [];
    var last_executed_index = -1;

    this.execute = function(command) {
        command.do();
        commands.push(command);
        last_executed_index  += 1;
    }

    this.undo = function() {
        commands[last_executed_index].undo();
        last_executed_index -= 1;
        
    };

    this.redo = function() {
        last_executed_index += 1;
        commands[last_executed_index].do();
    };

    this.canUndo = function() {
        return last_executed_index >= 0;
    };

    this.canRedo = function() {
        return last_executed_index < commands.length - 1;
    };

    
}
