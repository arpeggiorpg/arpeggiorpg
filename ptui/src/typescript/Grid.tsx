import * as Hammer from 'hammerjs';
import * as LD from "lodash";
import * as React from "react";
import * as ReactDOM from "react-dom";
import * as ReactRedux from "react-redux";
import * as Redux from "redux";
import * as svgPanZoom from "svg-pan-zoom";

import * as CommonView from "./CommonView";
import { PTUI } from "./Model";
import * as M from "./Model";
import * as T from "./PTTypes";

interface Obj<T> { [index: string]: T; }

export interface GridProps {
  scene: T.Scene;
  creatures: Obj<MapCreature>;
}

export const Grid = M.connectRedux(
  (props: GridProps & M.ReduxProps): JSX.Element => {
    const map_ = M.get(props.ptui.app.current_game.maps, props.scene.map);
    if (!map_) { return <div>Couldn't find map</div>; }
    const map = map_; // WHY TYPESCRIPT, WHY???

    const grid = props.ptui.state.grid;

    const menu = grid.active_menu ? renderMenu(grid.active_menu) : <noscript />;
    const annotation = grid.display_annotation ? renderAnnotation(grid.display_annotation)
      : <noscript />;

    return <div style={{ width: "100%", height: "100%" }}>
      {menu}
      {annotation}
      <GridSvg map={map} creatures={LD.values(props.creatures)} />
    </div>;

    function renderAnnotation({ pt, rect }: { pt: T.Point3, rect: M.Rect }): JSX.Element {
      const special = LD.find(map.specials, ([pt_, _, note, _2]) => M.isEqual(pt, pt_));
      if (!special) { return <noscript />; }
      return <CommonView.ClickAway
        onClick={() => props.dispatch({ type: "ToggleAnnotation", pt })}>
        <div style={{
          position: "fixed",
          fontSize: "24px",
          top: rect.sw.y, left: rect.sw.x,
          border: "1px solid black", borderRadius: "5px",
          backgroundColor: "white",
        }}>{special[2]}</div>
      </CommonView.ClickAway>;
    }

    function renderMenu({ cid, rect }: { cid: T.CreatureID, rect: M.Rect }): JSX.Element {
      const creature_ = M.get(props.creatures, cid);
      if (!creature_) {
        return <noscript />;
      }
      const creature = creature_; // WHY TYPESCRIPT, WHY???
      return <CommonView.ClickAway
        onClick={() => props.dispatch({ type: "ActivateGridCreature", cid, rect })}>
        <div
          style={{
            position: "fixed",
            paddingLeft: "0.5em",
            paddingRight: "0.5em",
            top: rect.sw.y, left: rect.sw.x,
            backgroundColor: "white",
            border: "1px solid black",
            borderRadius: "5px",
            fontSize: "24px",
          }}
        >
          <div style={{ borderBottom: "1px solid grey" }}>{creature.creature.name}</div>
          {
            LD.keys(creature.actions).map(
              actionName => {
                function onClick() {
                  props.dispatch({ type: "ActivateGridCreature", cid, rect });
                  creature.actions[actionName](cid);
                }
                return <div key={actionName}
                  style={{ borderBottom: "1px solid grey", cursor: "pointer" }}>
                  <a onClick={() => onClick()}>{actionName}</a>
                </div>;
              })
          }
        </div>
      </CommonView.ClickAway>;
    }
  });


export interface MapCreature {
  creature: T.Creature;
  pos: T.Point3;
  class_: T.Class;
  actions: { [index: string]: (cid: T.CreatureID) => void };
}

interface GridSvgProps {
  map: T.Map;
  creatures: Array<MapCreature>;
}
interface GridSvgState {
  spz_element: SvgPanZoom.Instance | undefined;
  hammer?: HammerManager;
}
class GridSvgComp extends React.Component<GridSvgProps & M.ReduxProps, GridSvgState> {

  constructor(props: GridSvgProps & M.ReduxProps) {
    super(props);
    this.state = { spz_element: undefined };
  }

  panzoomEvents() {
    const self = this;
    // init: (options: CustomEventOptions) => void;
    // haltEventListeners: string[];
    // destroy: Function;
    return {
      haltEventListeners: ['touchstart', 'touchend', 'touchmove', 'touchleave', 'touchcancel'],
      init: (options: SvgPanZoom.CustomEventOptions) => {
        let initialScale = 1;
        let pannedX = 0;
        let pannedY = 0;
        // Init Hammer
        // Listen only for pointer and touch events
        const hammer = new Hammer(options.svgElement, {
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
      center: true,
      fit: true,
      customEventsHandler: this.panzoomEvents(),
      zoomScaleSensitivity: 0.5,
    });
    this.setState({ spz_element: pz });
  }

  componentWillUnmount() {
    if (this.state.spz_element) {
      this.state.spz_element.destroy();
    }
  }

  shouldComponentUpdate(nextProps: GridSvgProps & M.ReduxProps): boolean {
    const mvmt_diff = !M.isEqual(
      this.props.ptui.state.grid.movement_options,
      nextProps.ptui.state.grid.movement_options);
    const app_diff = !M.isEqual(this.props.ptui.app, nextProps.ptui.app);
    return app_diff || mvmt_diff;
  }

  render(): JSX.Element {
    const { map, creatures, ptui } = this.props;
    console.log("[EXPENSIVE:GridSvg.render]");
    const terrain_els = map.terrain.map(pt => tile("white", "base-terrain", pt));
    const creature_els = creatures.map(
      c => <GridCreature key={c.creature.id} creature={c} />);
    const grid = ptui.state.grid;
    const move = grid.movement_options;
    const movement_target_els = move
      ? move.options.map(pt => <MovementTarget key={pt.toString()} cid={move.cid} pt={pt} />)
      : [];
    const special_els = this.props.map.specials.map(
      ([pt, color, _, vis]) => <SpecialTile key={pt.toString()} pt={pt} color={color} vis={vis} />);
    const annotation_els = M.filterMap(this.props.map.specials,
      ([pt, _, note, vis]) => {
        if (note !== "") {
          return <Annotation key={pt.toString()} pt={pt} note={note} vis={vis} />;
        }
      });

    return <svg id="pt-grid" preserveAspectRatio="xMinYMid slice"
      style={{ width: "100%", height: "100%", backgroundColor: "rgb(215, 215, 215)" }}>
      <g>
        {/* this <g> needs to be here for svg-pan-zoom. Otherwise svg-pan-zoom will reparent all
          nodes inside the <svg> tag to a <g> that it controls, which will mess up react's
          virtualdom rendering */}
        {terrain_els}
        {special_els}
        {annotation_els}
        {creature_els}
        {movement_target_els}
      </g>
    </svg>;
  }
}
export const GridSvg = M.connectRedux(GridSvgComp);

const MovementTarget = M.connectRedux(
  ({ cid, pt, ptui, dispatch }: { cid?: T.CreatureID; pt: T.Point3 } & M.ReduxProps)
    : JSX.Element => {
    const tprops = tile_props("cyan", pt);
    function moveCreature() {
      if (cid) {
        ptui.moveCreature(dispatch, cid, pt);
      } else {
        ptui.moveCombatCreature(dispatch, pt);
      }
    }
    return <rect {...tprops} fillOpacity="0.4" onClick={moveCreature} />;
  });

const SpecialTile = M.connectRedux(
  ({ color, vis, pt, ptui }: { color: string, vis: T.Visibility, pt: T.Point3 } & M.ReduxProps)
    : JSX.Element => {
    if (M.isEqual(vis, { t: "GMOnly" }) && ptui.state.player_id) {
      return <noscript />;
    }
    const tprops = tile_props(color, pt);
    return <rect {...tprops} />;
  });


const Annotation = M.connectRedux(
  ({ ptui, dispatch, pt, note, vis }:
    { pt: T.Point3, note: string, vis: T.Visibility } & M.ReduxProps)
    : JSX.Element => {
    if (M.isEqual(vis, { t: "GMOnly" }) && ptui.state.player_id) {
      return <noscript />;
    }

    let element: SVGRectElement;

    function onClick() {
      dispatch({ type: "ToggleAnnotation", pt, rect: screenCoordsForRect(element) });
    }

    return <g>
      <rect width="100" height="100" x={pt[0] * 100} y={pt[1] * 100 - 50} fillOpacity="0"
        ref={el => element = el} onClick={onClick}
      />
      <text
        style={{ pointerEvents: "none" }}
        x={pt[0] * 100 + 25} y={pt[1] * 100 + 50}
        fontSize="100px" stroke="black" strokeWidth="2px" fill="white">*</text>
    </g>;
  });


const GridCreature = M.connectRedux(
  ({ ptui, dispatch, creature }: { creature: MapCreature; } & M.ReduxProps): JSX.Element => {
    let element: SVGRectElement | SVGImageElement;
    function onClick() {
      const act: M.Action = {
        type: "ActivateGridCreature", cid: creature.creature.id, rect: screenCoordsForRect(element),
      };
      dispatch(act);
    }
    const highlightProps: React.SVGAttributes<SVGGraphicsElement> = {};
    const combat = ptui.app.current_game.current_combat;
    if (combat && ptui.getCurrentCombatCreatureID(combat) === creature.creature.id) {
      highlightProps.stroke = "black";
      highlightProps.strokeWidth = 3;
    }
    const target_opts = ptui.state.grid.target_options;
    if (target_opts && target_opts.options.t === "CreatureIDs") {
      if (LD.includes(target_opts.options.cids, creature.creature.id)) {
        highlightProps.stroke = "red";
        highlightProps.strokeWidth = 3;
      }
    }

    if (creature.creature.portrait_url !== "") {
      const props = tile_props("white", creature.pos, creature.creature.size);
      const bare_props = bare_tile_props(creature.pos, creature.creature.size);
      return <g>
        <image ref={el => element = el} key={creature.creature.id}
          xlinkHref={creature.creature.portrait_url} {...props} />
        <rect {...bare_props} {...highlightProps} fillOpacity="0" onClick={() => onClick()} />
      </g>;
    } else {
      const props = tile_props(creature.class_.color, creature.pos, creature.creature.size);
      return <g onClick={() => onClick()}>
        {<rect ref={el => element = el} {...props} {...highlightProps} />}
        {text_tile(creature.creature.name.slice(0, 4), creature.pos)}
      </g >;
    }
  });


function text_tile(text: string, pos: T.Point3): JSX.Element {
  return <text style={{ pointerEvents: "none" }} fontSize="50" x={pos[0] * 100} y={pos[1] * 100}>
    {text}
  </text>;
}

function tile(color: string, keyPrefix: string, pos: T.Point3, size?: { x: number, y: number })
  : JSX.Element {
  const key = `${keyPrefix}-${pos[0]}-${pos[1]}`;
  const props = tile_props(color, pos, size);
  return <rect key={key} {...props} />;
}

function bare_tile_props(pt: T.Point3, size = { x: 1, y: 1 }): React.SVGProps<SVGElement> {
  return {
    width: 100 * size.x, height: 100 * size.y,
    rx: 5, ry: 5,
    x: pt[0] * 100, y: (pt[1] * 100) - 50,
  };
}

function tile_props(color: string, pt: T.Point3, size = { x: 1, y: 1 }): React.SVGProps<SVGElement> {
  return { ...bare_tile_props(pt, size), stroke: "black", strokeWidth: 1, fill: color };
}

function screenCoordsForRect(rect: SVGRectElement | SVGImageElement): M.Rect {
  const svg = document.getElementById("pt-grid") as any as SVGSVGElement;
  const matrix = rect.getScreenCTM();
  const pt = svg.createSVGPoint();
  pt.x = rect.x.animVal.value;
  pt.y = rect.y.animVal.value;
  const nw = pt.matrixTransform(matrix);
  pt.x += rect.width.animVal.value;
  const ne = pt.matrixTransform(matrix);
  pt.y += rect.height.animVal.value;
  const se = pt.matrixTransform(matrix);
  pt.x -= rect.width.animVal.value;
  const sw = pt.matrixTransform(matrix);
  return { nw, ne, se, sw };
}
