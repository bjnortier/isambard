
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
    var successFn;

    this.execute = function(command) {
        showSpinner();
        command.do();
        successFn = function() {
            commands.splice(last_executed_index + 1, commands.length - (last_executed_index) - 1);
            commands.push(command);
            last_executed_index  += 1;
        }
    }

    this.undo = function() {
        if (last_executed_index < 0) {
            throw Error('Undo past beginning');
        }
        showSpinner();
        commands[last_executed_index].undo();
        successFn = function() {
            last_executed_index -= 1;
        }
    }

    this.redo = function() {
        if (last_executed_index + 1 > commands.length - 1) {
            throw Error('Redo past end');
        }
        showSpinner();
        commands[last_executed_index + 1].redo();
        successFn = function() {
            last_executed_index += 1;
        }
    };

    this.canUndo = function() {
        return last_executed_index >= 0;
    };

    this.canRedo = function() {
        return last_executed_index < commands.length - 1;
    };

    var showSpinner = function() {
	if ($('#progress-container').children().size() == 0) {
            $('#progress-container').append(
		'<div id="progress"><img src="images/progress-spinner.gif" alt="in progress"/></div>');
	}
	$('#progress').show();
    }

    var hideSpinner = function() {
	$('#progress').hide();
    }

    this.inProgressSuccess = function() {
        console.log("command in progress success");
        successFn();
        hideSpinner();
    }

    this.inProgressFailure = function() {
        console.log("command in progress failure");
        hideSpinner();
    }
    
}
