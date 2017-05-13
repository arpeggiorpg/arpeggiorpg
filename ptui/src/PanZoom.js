

function init(app, state, el) {
  console.log("[initializePanZoom]", el);
  window.requestAnimationFrame(function(_) {
    console.log("[initializePanZoom:animation]");
    state.managedElements[el] = svgPanZoom(
      el,
      { dblClickZoomEnabled: false
      , resize: true
      , center: true
      , fit: true
      , customEventsHandler: state.eventsHandler
      , zoomScaleSensitivity: 0.5
      , beforePan: function() {
          app.ports.panning.send(null);
      }
      });
    state.managedElements[el].zoomOut();
    state.managedElements[el].zoomOut();
    state.managedElements[el].zoomOut();
  });
}

function update(state, el) {
  console.log("[updateBoundingBox]", el);
  window.requestAnimationFrame(function(_) {
    console.log("[updateBoundingBax:animate]", el);
    state.managedElements[el].updateBBox();
    state.managedElements[el].resize();
    state.managedElements[el].center();
    state.managedElements[el].fit();
    state.managedElements[el].zoomOut();
    state.managedElements[el].zoomOut();
    state.managedElements[el].zoomOut();
  });
  
}

function get_svgpanzoom_hammerjs_touch_event_handler() {
  // This code was largely copied from the SVG-pan-zoom mobile.html example:
  // https://github.com/ariutta/svg-pan-zoom/blob/master/demo/mobile.html
  return {
      haltEventListeners: ['touchstart', 'touchend', 'touchmove', 'touchleave', 'touchcancel']
    , init: function(options) {
        var instance = options.instance
          , initialScale = 1
          , pannedX = 0
          , pannedY = 0
        // Init Hammer
        // Listen only for pointer and touch events
        this.hammer = Hammer(options.svgElement, {
          inputClass: Hammer.SUPPORT_POINTER_EVENTS ? Hammer.PointerEventInput : Hammer.TouchInput
        })
        // Enable pinch
        this.hammer.get('pinch').set({enable: true})
        // Handle pan
        this.hammer.on('panstart panmove', function(ev){
          // On pan start reset panned variables
          if (ev.type === 'panstart') {
            pannedX = 0;
            pannedY = 0;
          }
          // Pan only the difference
          instance.panBy({x: ev.deltaX - pannedX, y: ev.deltaY - pannedY})
          pannedX = ev.deltaX
          pannedY = ev.deltaY
        })
        // Handle pinch
        this.hammer.on('pinchstart pinchmove', function(ev){
          // On pinch start remember initial zoom
          if (ev.type === 'pinchstart') {
            initialScale = instance.getZoom()
            instance.zoom(initialScale * ev.scale)
          }
          instance.zoom(initialScale * ev.scale)
        })
        // Prevent moving the page on some devices when panning over SVG
        options.svgElement.addEventListener('touchmove', function(e){ e.preventDefault(); });
      }
    , destroy: function(){ this.hammer.destroy() }
    }
}


function PanZoom_initializePorts(app) {
  var eventsHandler = get_svgpanzoom_hammerjs_touch_event_handler();
      
  var state = {managedElements: {}, eventsHandler: eventsHandler};
  app.ports.initializePanZoom.subscribe(function(s) {init(app, state, s)});
  app.ports.updateBoundingBox.subscribe(function(s) {update(state, s)});
}
