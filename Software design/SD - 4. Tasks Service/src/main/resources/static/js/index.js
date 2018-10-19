function escapeHtml (text) {
  var map = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#039;' };
  //return text.replace (/[&<>"']/g, function (m) { return map [m]; });
  return text;
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

var findParentWithAttributes = function (child, attributes) {
	var parent = child;
	while (parent) {
		var allAts = true;
		for (var i = 0; i < attributes.length; i++) {
			allAts =  allAts 
				   && parent.hasAttribute (attributes [i]);
		}
		
		if (allAts) { break; }
		
		parent = parent.parentNode;
	}
		
	return parent;
}

// List of buttons
var addListButton, deleteListButtons, 
	addTaskButtons, deleteTaskButtons;
// Each shadow
var addListShadow, addTaskShadow;
// Additional objects
var addTaskLabel, addTaskList, addTaskDesc, 
	addTaskDate, addTaskTime, addTaskError,
	
	addListTitle, addListError;
// Tasks objects
var tasks;

var hideShadowFunction = function (e) {
	var obj = e.target;
	if (isShadowRoot (e.target)) {
		obj.style.display = "none";
	}
}

var openAddTaskShadow = function (e) {
	var clickObj = findParentWithAttributes (e.target, ["title", "list"]);
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
		} else { console.log (response); }
	});
}

var sendAndReload = function (address, body) {
	sendRequest (address, body, function (response) {
		try {
            response = JSON.parse (response);
        } catch (exception) { 
        	console.log (exception); 
        	return; 
        }
		
		if (response ['status'] === "done") {
			location.reload ();
		} else { console.log (response); }
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

var deleteList = function (e) {
	var parent = findParentWithAttributes (e.target, ["list"]);
	if (!parent) { location.reload (); }
	
	var list = parent.getAttribute ("list");
	var body = { "list": list };
	
	sendAndReload ("/lists/delete/list", body);
}

var deleteTask = function (e) {
	var parent = findParentWithAttributes (e.target, ["task"]);
	if (!parent) { location.reload (); }
	
	var task = parent.getAttribute ("task");
	var body = { "task": task };
	
	sendAndReload ("/lists/delete/task", body);
}

var toggleTask = function (e) {
	var task = findParentWithAttributes (e.target, ["task"]);
	
	var classes = task.classList.value;
	if (classes.indexOf ("task-failed") != -1) { return; }
	
	var body = { "task": task.getAttribute ("task") };
	sendAndReload ("/lists/update/task", body);
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
	
	deleteListButtons = document.getElementsByClassName ("delete-list-button");
	for (var i = 0; i < deleteListButtons.length; i++) { deleteListButtons [i].onclick = deleteList; }
	
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
	
	tasks = document.getElementsByClassName ("task");
	for (var i = 0; i < tasks.length; i++) { tasks [i].onclick = toggleTask; }
};