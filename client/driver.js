var socket;
var app = Elm.Main.init({
    node: document.getElementById('elm')
});
app.ports.sendToJS.subscribe(function(request) {
    switch(request.kind) {
    case 'connect':
        socket = new WebSocket(request.payload);
        socket.addEventListener("open", function(event) {
            app.ports.receiveFromJS.send({ kind: 'server-status', payload: 'connected' });
        });
        socket.addEventListener("close", function(event) {
            app.ports.receiveFromJS.send({ kind: 'server-status', payload: 'disconnected' });
        });
        socket.addEventListener("message", function(event) {
            app.ports.receiveFromJS.send({ kind: 'from-server', payload: event.data });
        });
        break;
    case 'send':
        socket.send(JSON.stringify(request.payload));
        break;
    }
});
