var selectedList = -1;

var shadows = []; // list of pop-up forms

var isShadowRoot = function (object) {
	for (var i = 0; i < shadows.length; i++) {
		if (object === shadows [i]) {
			return true;
		}
	}

	return false;
}

// List of buttons
var addListButton, addTaskButtons;
// Each shadow
var addListShadow, addTaskShadow;
// Additional objects
var addTaskLabel, addTaskList, addTaskDesc,
	addTaskError;

var hideShadowFunction = function (e) {
	var obj = e.target;
	if (isShadowRoot (e.target)) {
		obj.style.display = "none";
	}
}

var openAddTaskShadow = function (e) {
	var clickObj = e.target;
	while (clickObj 
	   && !clickObj.hasAttribute ("title") 
	   && !clickObj.hasAttribute ("list")) {
		clickObj = clickObj.parentElement;
	}
	
	if (!clickObj) { return; }
	
	addTaskLabel.innerHTML = clickObj.getAttribute ("title");
	addTaskList.value = clickObj.getAttribute ("list");
	
	addTaskShadow.style.display = "flex";
}

var sendRequest = function (address, content, handler) {
	var request = new XMLHttpRequest();
    request.open ("POST", address, true);
    
    request.setRequestHeader ("Content-Type", "application/json");
    request.send (JSON.stringify (content));
    
    request.onreadystatechange = function (e) {
        if (request.readyState == 4) {
        	var response = request.responseText;
            if (!response) { return; }
            
            //console.log (response);
            handler (response); 
        }
    };
}

var addTask = function (e) {
	var body = {
		"list": addTaskList.value,
		"desc": addTaskDesc.value
	}
	
	sendRequest ("/lists/add/task", body, function (response) {
		addTaskError.innerHTML = "";
		
		try {
            response = JSON.parse (response);
        } catch (exception) {
			addTaskError.innerHTML = exception;
			return;
        }
		
		if (response ['status'] === "done") {
			location.reload ();
		} else if (response ['status'] === "error") {
			var error = "";
			if (response ['cause']) {
				error += response ['cause'];
			}
			if (response ['reason']) {
				error += " " + response ['reason'];
				error = error.trim ();
			}
			if (response ['comment']) {
				error += " ~ " + response ['comment'];
				error = error.trim ();
			}
			
			addTaskError.innerHTML = error;
		}
	});
}

window.onload = function (e) {
	shadows = document.getElementsByClassName ("shadow");
	for (var i = 0; i < shadows.length; i++) {
		shadows [i].onclick = hideShadowFunction;
	}
	
	addListShadow = document.getElementById ("add-list-shadow");
	addListButton = document.getElementById ("add-list-button");
	addListButton.onclick = function (e) {
		addListShadow.style.display = "flex";
	}
	
	addTaskShadow  = document.getElementById ("add-task-shadow");
	addTaskError   = document.getElementById ("add-task-error");
	addTaskLabel   = document.getElementById ("add-task-label");
	addTaskList    = document.getElementById ("add-task-list");
	addTaskDesc    = document.getElementById ("add-task-desc");
	addTaskButtons = document.getElementsByClassName ("add-task-button");
	for (var i = 0; i < addTaskButtons.length; i++) {
		addTaskButtons [i].onclick = openAddTaskShadow;
	}
	
	document.getElementById ("add-task-button").onclick = addTask;
};