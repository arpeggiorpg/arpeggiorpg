function DomUtils_select(id) {
    console.log("selecting", id);
    window.requestAnimationFrame(function (_) {
        document.getElementById(id).select();
    });
}

function DomUtils_initializePorts(app) {
    app.ports.select.subscribe(DomUtils_select);
}
