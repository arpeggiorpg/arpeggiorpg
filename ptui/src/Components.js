
function afterView(f) {
  return function() {
    let args = arguments;
    window.requestAnimationFrame(function(_) {
        f.apply(this, args);
    });
  }
}

function Components_initializePorts(app) {
  app.ports.renderHello.subscribe(afterView(components.renderHello));
  app.ports.unloadHello.subscribe(afterView(components.unloadHello));
}
