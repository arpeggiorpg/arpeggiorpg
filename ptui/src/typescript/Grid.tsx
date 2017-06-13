import * as React from "react";
import * as ReactDOM from "react-dom";
import * as T from './PTTypes';
import * as CommonView from './CommonView';
import { PTUI } from './Model';
import * as M from './Model';
import * as LD from 'lodash';

import * as svgPanZoom from 'svg-pan-zoom';

export function Grid({ ptui, scene_id }: { ptui: M.PTUI; scene_id: T.SceneID; }): JSX.Element {
  let scene = M.get(ptui.app.current_game.scenes, scene_id);
  if (!scene) { return <div>Couldn't find scene</div>; }
  let def_scene = scene;
  let map = M.get(ptui.app.current_game.maps, scene.map);
  if (!map) { return <div>Couldn't find map</div>; }
  let creatures = ptui.getCreatures(LD.keys(scene.creatures)).map(
    (creature) => ({ creature, pos: def_scene.creatures[creature.id][0] }));
  return <GridSvg map={map} creatures={creatures} />;
}

interface MapCreature {
  creature: T.Creature;
  pos: T.Point3;
}


interface GridSvgProps { map: T.Map; creatures: Array<MapCreature> }
class GridSvg extends React.Component<GridSvgProps, { spz_element: SvgPanZoom.Instance | undefined }> {
  constructor(props: GridSvgProps) {
    super(props);
    this.state = { spz_element: undefined };
  }

  componentDidMount() {
    let pz = svgPanZoom("#pt-grid", {
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
      this.state.spz_element.destroy()
    }
  }

  render(): JSX.Element {
    console.log("[EXPENSIVE:Grid.render]");
    let open_terrain = this.props.map.terrain;
    let terrain_els = open_terrain.map((pt) => tile("white", "base-terrain", pt));
    let creature_els = this.props.creatures.map((c) => creature_tile(c));

    return <svg id="pt-grid" preserveAspectRatio="xMinYMid slice"
      style={{ width: "100%", height: "100%", backgroundColor: "rgb(215, 215, 215)" }}>
      {terrain_els}
      {creature_els}
    </svg>;
  }
}

function creature_tile(creature: MapCreature) {
  return <g key={creature.creature.name}>
    {tile("blue", "creature-tile", creature.pos)}
    {textTile(creature.creature.name.slice(0, 4), creature.pos)}
  </g>;
}

function textTile(text: string, pos: T.Point3) {
  return <text fontSize="50" x={pos[0] * 100} y={pos[1] * 100}>{text}</text>;
}

function tile(color: string, keyPrefix: string, [ptx, pty, _]: T.Point3): JSX.Element {
  let key = `${keyPrefix}-${ptx}-${pty}`;
  return <rect key={key} width={100} height={100} x={ptx * 100} y={pty * 100 - 50}
    fill={color} stroke="black" strokeWidth="1" />
}
