var element = function (id) {
    return document.getElementById (id);
}

window.onload = function () {
    if (element ("signup")) {
        element ("signup").onclick = function (e) {
            element ("signup").setAttribute ("disabled", "disabled");
    
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
                        element ("signup").removeAttribute ("disabled");
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

    if (element ("filter")) {
        element ("filter").onclick = function (e) {
            element ("filter").setAttribute ("disabled", "disabled");
    
            var identifier      = element ("userIdentifier").value;
            var sorting         = element ("sorting").value;
            var currency        = element ("currency").value;
            var withDescription = element ("showWithoutDesc").checked;
            var withIcon        = element ("showWithoutIcon").checked;
            var shape           = element ("shape").value;
            var color           = element ("color").value;
    
            var request = new XMLHttpRequest ();
    
            request.open ("POST", "/update-user", true);
            request.setRequestHeader ("Content-Type", "application/json");
            request.send (JSON.stringify ({
                "identifier": identifier, "currency": currency,
                "sorting": sorting, "withDescription": withDescription,
                "withIcon": withIcon, "shape": shape, "color": color
            }));

            request.onreadystatechange = function (e) {
                if (request.readyState == 4) {
                    element ("filter").removeAttribute ("disabled");
                    var response = request.responseText;
                    console.log (response);

                    if (response === "updated") {
                        location.reload ();
                    }
                }
            };
        }
    }
}