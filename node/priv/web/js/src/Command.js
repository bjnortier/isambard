
function Command(doFn, undoFn, redoFn) {
    var doFn = doFn;
    var undoFn = undoFn;
    var redoFn = redoFn;

    this.do = function() { doFn(); };
    this.undo = function() { undoFn(); };
    this.redo = function() { redoFn(); };
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
        commands[last_executed_index].redo();
    };

    this.canUndo = function() {
        return last_executed_index >= 0;
    };

    this.canRedo = function() {
        return last_executed_index < commands.length - 1;
    };

    
}
