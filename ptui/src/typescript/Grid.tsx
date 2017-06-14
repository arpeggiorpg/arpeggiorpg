import * as LD from "lodash";
import * as React from "react";
import * as ReactDOM from "react-dom";
import * as svgPanZoom from "svg-pan-zoom";

import * as CommonView from "./CommonView";
import { PTUI } from "./Model";
import * as M from "./Model";
import * as T from "./PTTypes";

interface Obj<T> { [index: string]: T; }

export interface GridProps {
  ptui: M.PTUI;
  scene: T.Scene;
  creatures: Obj<MapCreature>;
}

export class Grid extends React.Component<GridProps, { active?: [T.CreatureID, Rect] }> {
  constructor(props: GridProps) {
    super(props);
    this.state = {};
  }

  render(): JSX.Element {
    const map = M.get(this.props.ptui.app.current_game.maps, this.props.scene.map);
    if (!map) { return <div>Couldn't find map</div>; }
    const self = this;
    function onClickCreature(c: MapCreature, rect: Rect) {
      if (self.state.active && M.isEqual(self.state.active[0], c.creature.id)) {
        self.setState({ active: undefined });
      } else if (LD.keys(c.actions).length > 0) {
        self.setState({ active: [c.creature.id, rect] });
      }
    }
    const menu = this.state.active ? this.renderMenu(this.state.active) : <noscript />;

    return <div style={{ width: "100%", height: "100%" }}>
      {menu}
      <GridSvg map={map} creatures={LD.values(this.props.creatures)}
        onClickCreature={onClickCreature} />
    </div>;
  }

  renderMenu([cid, rect]: [T.CreatureID, Rect]): JSX.Element {
    const creature = M.get(this.props.creatures, cid);
    if (!creature) {
      return <noscript />;
    }
    return <div
      style={{
        position: "fixed",
        top: rect.sw.y, left: rect.sw.x,
        backgroundColor: "white",
        border: "1px solid black",
        borderRadius: "5px",
      }}
    >
      {
        LD.keys(creature.actions).map(
          actionName => {
            const onClick = creature.actions[actionName];
            return <div key={actionName}
              style={{ fontSize: "24px", borderBottom: "1px solid grey", cursor: "pointer" }}
            >
              <a onClick={() => onClick(creature.creature.id)}>{actionName}</a>
            </div>;
          })
      }
    </div>;
  }
}

export interface MapCreature {
  creature: T.Creature;
  pos: T.Point3;
  class_: T.Class;
  actions: { [index: string]: (cid: T.CreatureID) => void };
}

interface GridSvgProps {
  map: T.Map;
  creatures: Array<MapCreature>;
  // FIXME: closures are Bad News Bears for component render caching!
  onClickCreature?: (c: MapCreature, rect: Rect) => void;
}
interface GridSvgState { spz_element: SvgPanZoom.Instance | undefined; }
class GridSvg extends React.Component<GridSvgProps, GridSvgState> {

  constructor(props: GridSvgProps) {
    super(props);
    this.state = { spz_element: undefined };
  }

  componentDidMount() {
    const pz = svgPanZoom("#pt-grid", {
      dblClickZoomEnabled: false,
      center: true,
      fit: true,
      // TODO: Hammer.js integration
      // , customEventsHandler: eventsHandler
      zoomScaleSensitivity: 0.5,
    });
    this.setState({ spz_element: pz });
  }

  componentWillUnmount() {
    if (this.state.spz_element) {
      this.state.spz_element.destroy();
    }
  }

  render(): JSX.Element {
    console.log("[EXPENSIVE:GridSvg.render]");
    const open_terrain = this.props.map.terrain;
    const terrain_els = open_terrain.map(pt => tile("white", "base-terrain", pt));
    const self = this;
    const creature_els = this.props.creatures.map(
      c => {
        function onClick(rect: Rect) {
          if (self.props.onClickCreature) {
            self.props.onClickCreature(c, rect);
          }
        }
        return <GridCreature key={c.creature.id} creature={c} onClick={onClick} />;
      });
    // let special_els = this.props.map.specials.map()

    return <svg id="pt-grid" preserveAspectRatio="xMinYMid slice"
      style={{ width: "100%", height: "100%", backgroundColor: "rgb(215, 215, 215)" }}>
      <g>
        {/* this <g> needs to be here for svg-pan-zoom. Otherwise svg-pan-zoom will reparent all
          nodes inside the <svg> tag to a <g> that it controls, which will mess up react's
          virtualdom rendering */}
        {terrain_els}
        {creature_els}
      </g>
    </svg>;
  }
}

interface GridCreatureProps { creature: MapCreature; onClick?: (r: Rect) => void; }
class GridCreature extends React.Component<GridCreatureProps, undefined> {
  render(): JSX.Element {
    let element: SVGRectElement | SVGImageElement;
    const self = this;
    function onClick() {
      if (self.props.onClick) {
        self.props.onClick(screenCoordsForRect(element));
      }
    }
    const creature = this.props.creature;
    if (creature.creature.portrait_url !== "") {
      const props = tile_props("white", creature.pos, creature.creature.size);
      return <image ref={el => element = el} key={creature.creature.id} onClick={() => onClick()}
        xlinkHref={creature.creature.portrait_url} {...props} />;
    } else {
      const props = tile_props(creature.class_.color, creature.pos, creature.creature.size);
      return <g key={creature.creature.name} onClick={() => onClick()}>
        {<rect ref={el => element = el} {...props} />}
        {text_tile(creature.creature.name.slice(0, 4), creature.pos)}
      </g >;
    }
  }
}

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

function tile_props(color: string, pt: T.Point3, size = { x: 1, y: 1 }): React.SVGProps<SVGElement> {
  return {
    width: 100 * size.x, height: 100 * size.y,
    rx: 5, ry: 5,
    x: pt[0] * 100, y: (pt[1] * 100) - 50,
    stroke: "black", strokeWidth: 1,
    fill: color,
  };
}

interface Rect { nw: SVGPoint; ne: SVGPoint; se: SVGPoint; sw: SVGPoint; }

function screenCoordsForRect(rect: SVGRectElement | SVGImageElement): Rect {
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
