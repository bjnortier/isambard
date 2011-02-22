
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
        commands.splice(last_executed_index + 1, commands.length - (last_executed_index) - 1);
        commands.push(command);
        last_executed_index  += 1;
    }

    this.undo = function() {
        if (last_executed_index < 0) {
            throw Error('Undo past beginning');
        }
        commands[last_executed_index].undo();
        last_executed_index -= 1;
        
    };

    this.redo = function() {
        if (last_executed_index + 1 > commands.length - 1) {
            throw Error('Redo past end');
        }
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
