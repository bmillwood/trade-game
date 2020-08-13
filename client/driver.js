var socket;
var app = Elm.Main.init({
    node: document.getElementById('elm')
});
app.ports.sendLogin.subscribe(function(login) {
    var endpoint = login.endpoint;
    var username = login.username;
    socket = new WebSocket(endpoint);
    socket.addEventListener("open", function(event) {
        socket.send(JSON.stringify({ username: username }));
    });
    socket.addEventListener("message", function(event) {
        app.ports.receiveFromServer.send(event.data);
    });
});
