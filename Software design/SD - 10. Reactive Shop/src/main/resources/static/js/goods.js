var element = function (id) {
    return document.getElementById (id);
}

window.onload = function () {
    element ("signup").onclick = function (e) {
        var login    = element ("login").value;
        var currency = element ("currency").value;
        if (login && login.length > 0 && currency) {
            var request = new XMLHttpRequest ();

            request.open ("POST", "/register-user", true);
            request.setRequestHeader ("Content-Type", "application/json");
            request.send (JSON.stringify ({
                "login": login, "currency": currency
            }));

            request.onreadystatechange = function (e) {
                if (request.readyState == 4) {
                    var response = request.responseText;
                    console.log (response);

                    if (response === "registered") {
                        location.reload ();
                    }
                }
            };
        }
    }
}