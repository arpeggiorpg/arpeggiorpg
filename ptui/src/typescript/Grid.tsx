import * as LD from "lodash";
import * as React from "react";
import * as ReactDOM from "react-dom";
import * as svgPanZoom from "svg-pan-zoom";

import * as CommonView from "./CommonView";
import { PTUI } from "./Model";
import * as M from "./Model";
import * as T from "./PTTypes";

export function Grid({ ptui, scene_id }: { ptui: M.PTUI; scene_id: T.SceneID; }): JSX.Element {
  const scene = M.get(ptui.app.current_game.scenes, scene_id);
  if (!scene) { return <div>Couldn't find scene</div>; }
  const defScene = scene;
  const map = M.get(ptui.app.current_game.maps, scene.map);
  if (!map) { return <div>Couldn't find map</div>; }
  const creatures = M.filterMap(
    ptui.getCreatures(LD.keys(scene.creatures)),
    creature => {
      const pos = defScene.creatures[creature.id][0]; // map over keys -> [] is okay
      const class_ = M.get(ptui.app.current_game.classes, creature.class_);
      if (class_) {
        return { creature, pos, class_ };
      }
    }
  );
  return <GridSvg map={map} creatures={creatures} />;
}

interface MapCreature {
  creature: T.Creature;
  pos: T.Point3;
  class_: T.Class;
}

interface GridSvgProps { map: T.Map; creatures: Array<MapCreature>; }
class GridSvg extends React.Component<GridSvgProps, { spz_element: SvgPanZoom.Instance | undefined }> {
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
    console.log("[EXPENSIVE:Grid.render]");
    const open_terrain = this.props.map.terrain;
    const terrain_els = open_terrain.map(pt => tile("white", "base-terrain", pt));
    const creature_els = this.props.creatures.map(c => creature_tile(c));
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

function creature_tile(creature: MapCreature) {
  if (creature.creature.portrait_url !== "") {
    const props = tile_props("white", creature.pos, creature.creature.size);
    return <image key={creature.creature.id} xlinkHref={creature.creature.portrait_url} {...props} />;
  }
  return <g key={creature.creature.name}>
    {tile(creature.class_.color, "creature-tile", creature.pos, creature.creature.size)}
    {text_tile(creature.creature.name.slice(0, 4), creature.pos)}
  </g>;
}

function text_tile(text: string, pos: T.Point3) {
  return <text fontSize="50" x={pos[0] * 100} y={pos[1] * 100}>{text}</text>;
}

function tile(color: string, keyPrefix: string, pos: T.Point3, size?: { x: number, y: number },
): JSX.Element {
  const key = `${keyPrefix}-${pos[0]}-${pos[1]}`;
  const props = tile_props(color, pos, size);
  return <rect key={key} {...props} />;
}

function tile_props(color: string, [ptx, pty, _]: T.Point3, size?: { x: number, y: number },
): {
    width: number, height: number, rx: number, ry: number, x: number, y: number, stroke: string,
    strokeWidth: number, fill: string,
  } {
  if (!size) {
    size = { x: 1, y: 1 };
  }
  return {
    width: 100 * size.x, height: 100 * size.y,
    rx: 5, ry: 5,
    x: ptx * 100, y: (pty * 100) - 50,
    stroke: "black", strokeWidth: 1,
    fill: color,
  };
}
