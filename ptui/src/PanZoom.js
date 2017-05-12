

function init(state, el) {
  console.log("[initializePanZoom]", el);
  window.requestAnimationFrame(function(_) {
    console.log("[initializePanZoom:animation]");
    state[el] = svgPanZoom(el);
  });
}

function update(state, el) {
  console.log("[updateBoundingBox]", el);
  state[el].updateBBox();
}

function PanZoom_initializePorts(app) {
  var state = {};
  app.ports.initializePanZoom.subscribe(function(s) {init(state, s)});
  app.ports.updateBoundingBox.subscribe(function(s) {update(state, s)});
}
