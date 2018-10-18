function escapeHtml (text) {
  var map = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#039;' };
  return text.replace (/[&<>"']/g, function (m) { return map [m]; });
}

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
var addListButton, addTaskButtons, deleteTaskButtons;
// Each shadow
var addListShadow, addTaskShadow;
// Additional objects
var addTaskLabel, addTaskList, addTaskDesc, 
	addTaskDate, addTaskTime, addTaskError,
	
	addListTitle, addListError;

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
	
	addTaskLabel.innerHTML = escapeHtml (clickObj.getAttribute ("title"));
	addTaskList.value = clickObj.getAttribute ("list");
	
	addTaskShadow.style.display = "flex";
}

var sendRequest = function (address, content, handler) {
	var request = new XMLHttpRequest ();
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

var sendAddRequest = function (address, body, errorField) {
	sendRequest (address, body, function (response) {
		errorField.innerHTML = "";
		try {
            response = JSON.parse (response);
        } catch (exception) {
			errorField.innerHTML = exception;
			console.log (exception);
			return;
        }
		
		if (response ['status'] === "error") {
			var error = "";
			if (response ['cause'])   { error += response ['cause']; }
			if (response ['reason'])  { error += " " + response ['reason']; }
			if (response ['comment']) { error += " ~ " + response ['comment']; }
			
			errorField.innerHTML = error.trim ();
			console.log (error.trim ());
		} else if (response ['status'] === "done") {
			location.reload ();
		}
	});
}

var addList = function (e) {
	var body = { "title": addListTitle.value };
	sendAddRequest ("/lists/add/list", body, addListError);
}

var addTask = function (e) {
	var body = {
		"list": addTaskList.value,
		"desc": addTaskDesc.value
	}
	
	if (addTaskDate.value) {
		body ['expireDate'] = addTaskDate.value;
		if (addTaskTime.value) {
			body ['expireTime'] = addTaskTime.value;
		}
	}
	
	sendAddRequest ("/lists/add/task", body, addTaskError);
}

var deleteTask = function (e) {
	var parent = e.target.parentNode;
	while (parent && !parent.hasAttribute ("task")) {
		parent = parent.parentNode;
	}
	
	if (!parent) { location.reload (); }
	
	var task = parent.getAttribute ("task");
	var body = {
		"task": task
	};
	
	sendRequest ("/lists/delete/task", body, function (response) {
		try {
            response = JSON.parse (response);
        } catch (exception) { 
        	console.log (exception); 
        	return; 
        }
		
		if (response ['status'] === "done") {
			location.reload ();
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
	
	addListTitle = document.getElementById ("add-list-title");
	addListError = document.getElementById ("add-list-error");
	document.getElementById ("add-list-button-final").onclick = addList;
	
	addTaskShadow  = document.getElementById ("add-task-shadow");
	addTaskError   = document.getElementById ("add-task-error");
	addTaskLabel   = document.getElementById ("add-task-label");
	addTaskList    = document.getElementById ("add-task-list");
	addTaskDesc    = document.getElementById ("add-task-desc");
	addTaskDate    = document.getElementById ("add-task-date");
	addTaskTime    = document.getElementById ("add-task-time");
	addTaskButtons = document.getElementsByClassName ("add-task-button");
	for (var i = 0; i < addTaskButtons.length; i++) { addTaskButtons [i].onclick = openAddTaskShadow; }
	
	deleteTaskButtons = document.getElementsByClassName ("delete-task-button");
	for (var i = 0; i < deleteTaskButtons.length; i++) { deleteTaskButtons [i].onclick = deleteTask; }
	
	document.getElementById ("add-task-button").onclick = addTask;
};