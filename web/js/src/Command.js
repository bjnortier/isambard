function renderErrorMessage(error) {
    $('#messages-container').empty();
    if (error.validation) {
	// No need for a message as there is already validation feedback
	console.log(error);
    } else if (error.string) {
	$('#messages-container').append('<div class="message">' + error.string + '</div>');
    } else {
	$('#messages-container').append('<div class="message">Oops. An unknown problem occurred</div>');
    }
}


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
	if (!this.canUndo()) {
	    renderErrorMessage({string:"Nothing to undo"});
	    return;
	}
        showSpinner();
        successFn = function() {
            last_executed_index -= 1;
        }
        commands[last_executed_index].undo();
    }

    this.redo = function() {
	if (!this.canRedo()) {
	    renderErrorMessage({string:"Nothing to redo"});
	    return;
	}
        showSpinner();
        successFn = function() {
            last_executed_index += 1;
        }
        commands[last_executed_index + 1].redo();
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
	$('#messages-container').empty();
        hideSpinner();
    }

    this.inProgressFailure = function(error) {
        console.log("command in progress failure: " + JSON.stringify(error));
	renderErrorMessage(error);
        hideSpinner();
    }
    
}
