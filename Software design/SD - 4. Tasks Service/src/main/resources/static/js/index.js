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

var addListButton, addTaskButtons;
var addListShadow, addTaskShadow;

var hideShadowFunction = function (e) {
	var obj = e.target;
	if (isShadowRoot (e.target)) {
		obj.style.display = "none";
	}
}

var openAddTaskShadow = function (e) {
	var buttonObj = e.target;
	addTaskShadow.style.display = "flex";
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
	
	addTaskShadow = document.getElementById ("add-task-shadow");
	addTaskButtons = document.getElementsByClassName ("add-task-button");
	for (var i = 0; i < addTaskButtons.length; i++) {
		addTaskButtons [i].onclick = openAddTaskShadow;
	}
};