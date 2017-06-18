/// A component which handles svg-pan-zoom and Hammer.js integration to pan and zoom.

import * as React from 'react';
import * as M from './Model';

interface SVGPanZoomProps {
  children: JSX.Element;
}
interface SVGPanZoomState {
  spz_element: SvgPanZoom.Instance | undefined;
  hammer?: HammerManager;
}

export class SVGPanZoom extends React.Component<React.SVGProps<SVGSVGElement>, SVGPanZoomState> {

  constructor(props: SVGPanZoomProps) {
    super(props);
    this.state = { spz_element: undefined };
  }

  panzoomEvents() {
    const self = this;
    return {
      haltEventListeners: ['touchstart', 'touchend', 'touchmove', 'touchleave', 'touchcancel'],
      init: (options: SvgPanZoom.CustomEventOptions) => {
        let initialScale = 1;
        let pannedX = 0;
        let pannedY = 0;
        // Init Hammer
        // Listen only for pointer and touch events
        const hammer = new Hammer(options.svgElement, {
          // `Hammer as any` is here because @types/hammerjs doesn't declare SUPPORT_POINTER_EVENTS
          inputClass: (Hammer as any).SUPPORT_POINTER_EVENTS
            ? Hammer.PointerEventInput : Hammer.TouchInput,
        });
        self.setState({ hammer });
        // Enable pinch
        hammer.get('pinch').set({ enable: true });
        // Handle pan
        hammer.on('panstart panmove', ev => {
          // On pan start reset panned variables
          if (ev.type === 'panstart') {
            pannedX = 0;
            pannedY = 0;
          }
          // Pan only the difference
          options.instance.panBy({ x: ev.deltaX - pannedX, y: ev.deltaY - pannedY });
          pannedX = ev.deltaX;
          pannedY = ev.deltaY;
        });
        // Handle pinch
        hammer.on('pinchstart pinchmove', ev => {
          // On pinch start remember initial zoom
          if (ev.type === 'pinchstart') {
            initialScale = options.instance.getZoom();
            options.instance.zoom(initialScale * ev.scale);
          }
          options.instance.zoom(initialScale * ev.scale);
        });
        // Prevent moving the page on some devices when panning over SVG
        options.svgElement.addEventListener('touchmove', e => e.preventDefault());

        // // See [Note: Panning/Clicking State Management]
        // options.svgElement.addEventListener('mousedown', function () {
        //   state.isMouseDown = true;
        // });
        // options.svgElement.addEventListener('mousemove', function () {
        //   if (state.isMouseDown) {
        //     app.ports.panning.send(true);
        //   }
        // });
        // options.svgElement.addEventListener('touchend', function (e) {
        //   app.ports.panning.send(false);
        // });
        // options.svgElement.addEventListener('mouseup', function () {
        //   state.isMouseDown = false;
        // })
        // options.svgElement.addEventListener('click', function () {
        //   app.ports.panning.send(false);
        //   state.isMouseDown = false;
        // });
      },
      destroy: () => { if (self.state.hammer) { self.state.hammer.destroy(); } },
    };
  }


  componentDidMount() {
    const pz = svgPanZoom("#pt-grid", {
      dblClickZoomEnabled: false,
      customEventsHandler: this.panzoomEvents(),
      zoomScaleSensitivity: 0.5,
    });
    this.setState({ spz_element: pz });
    this.refreshPanZoom(pz);
  }

  componentWillUnmount() {
    if (this.state.spz_element) {
      this.state.spz_element.destroy();
    }
  }

  refreshPanZoom(panzoom: SvgPanZoom.Instance | undefined) {
    if (panzoom) {
      console.log("[GridSvg.refreshPanZoom]");
      panzoom.updateBBox();
      panzoom.resize();
      panzoom.center();
      panzoom.fit();
      panzoom.zoomOut();
      panzoom.zoomOut();
      panzoom.zoomOut();
    }
  }

  public refresh() {
    if (this.state.spz_element) {
      this.refreshPanZoom(this.state.spz_element);
    }
  }

  render(): JSX.Element {
    return <svg {...this.props}>
      <g>
        {/* this <g> needs to be here for svg-pan-zoom. Otherwise it will reparent all
          nodes inside the <svg> tag to a <g> that it controls, which will mess up react's
          virtualdom rendering */}
        <rect fillOpacity="0" x="0" y="0" width="5" height="5" />
        {/* This <rect> needs to be here for svg-pan-zoom, and it needs to have a non-0 width and
            height. Otherwise various internal bugs crop up in the panzoom code. */}
        {this.props.children}
      </g>
    </svg>;
  }
}
