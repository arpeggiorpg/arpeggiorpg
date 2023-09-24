/// A component which handles svg-pan-zoom and Hammer.js integration to pan and zoom.

import * as Hammer from 'hammerjs';
import * as React from 'react';
import svgPanZoom from 'svg-pan-zoom';

interface SVGPanZoomProps {
  onPanZoom?: (x: boolean) => void;
  shouldPan: (ev: React.MouseEvent<SVGSVGElement>) => boolean;
}
interface SVGPanZoomState {
  spz_element: SvgPanZoom.Instance | undefined;
  hammer?: HammerManager;
  isMouseDown: boolean;
}

export class SVGPanZoom
  extends React.Component<SVGPanZoomProps & React.SVGProps<SVGSVGElement>, SVGPanZoomState> {


  constructor(props: SVGPanZoomProps & React.SVGProps<SVGSVGElement>) {
    super(props);
    this.state = { spz_element: undefined, isMouseDown: false };
  }

  panzoomEvents() {
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
        this.setState({ hammer });
        // Enable pinch
        hammer.get('pinch').set({ enable: true });
        // Handle pan
        hammer.on('panstart panmove', ev => {
          this.setState({ isMouseDown: true });
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
        hammer.on('panend', () => {
          this.setState({ isMouseDown: false });
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

        options.svgElement.addEventListener('mousedown', event => {
          if (event.button !== 0) { return; }
          if (event.pageX === undefined || event.pageY === undefined) {
            console.log("[SVGPanZoom.mousedown] DEBUG: Got non-mouse mousedown event?");
            return;
          }
          if (event.ctrlKey || this.props.shouldPan(event as any as React.MouseEvent<any>)) {
            this.setState({ isMouseDown: true });
          }
        });

        options.svgElement.addEventListener('mousemove', () => {
          if (this.state.isMouseDown) {
            if (this.props.onPanZoom) { this.props.onPanZoom(true); }
          }
        });
        options.svgElement.addEventListener('touchend', () => {
          if (this.props.onPanZoom) { this.props.onPanZoom(false); }
        });
        options.svgElement.addEventListener('mouseup', () => {
          this.setState({ isMouseDown: false });
        }
        );
        options.svgElement.addEventListener('click', () => {
          if (this.props.onPanZoom) { this.props.onPanZoom(false); }
          this.setState({ isMouseDown: false });
        });
      },
      destroy: () => { if (this.state.hammer) { this.state.hammer.destroy(); } },
    };
  }


  componentDidMount() {
    const pz = svgPanZoom("#pt-grid", {
      dblClickZoomEnabled: false,
      customEventsHandler: this.panzoomEvents(),
      zoomScaleSensitivity: 0.3,
      beforePan: this.beforePan.bind(this),
    });
    this.setState({ spz_element: pz });
    this.refreshPanZoom(pz);
  }

  beforePan(_oldPan: SvgPanZoom.Point, _newPan: SvgPanZoom.Point) {
    return this.state.isMouseDown;
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
    }
  }

  public refresh() {
    if (this.state.spz_element) {
      this.refreshPanZoom(this.state.spz_element);
    }
  }

  render(): JSX.Element {
    const {children, onPanZoom, shouldPan, ...props} = this.props;
    (props as any).style = { cursor: this.state.isMouseDown ? 'grabbing' : 'grab', ...props.style };
    return <svg {...props}>
      <g id="svg-pan-zoom-viewport">
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
