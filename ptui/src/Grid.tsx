import * as I from 'immutable';
import * as LD from "lodash";
import * as React from "react";

import TwitterPicker from 'react-color/lib/components/twitter/Twitter';
import { Button, Checkbox, Dimmer, Input, Menu, Segment } from 'semantic-ui-react';

import * as CV from "./CommonView";
import * as M from "./Model";
import * as T from "./PTTypes";
import * as SPZ from './SVGPanZoom';

interface Obj<T> { [index: string]: T; }

export interface MapGridProps {
  map: T.Map;
}

interface MapGridState {
  terrain: I.Set<I.List<number>>;
  specials: I.Map<I.List<number>, T.SpecialTileData>;
  painting: "Terrain" | "Special";
  painting_color: string;
  painting_note: string;
  painting_vis: T.Visibility;
}

export class MapGrid extends React.Component<MapGridProps & M.DispatchProps, MapGridState> {
  constructor(props: MapGridProps & M.DispatchProps) {
    super(props);
    this.state = {
      terrain: I.Set(props.map.terrain.map(I.List)),
      specials: M.specialsRPIToMap(props.map.specials),
      painting: "Terrain",
      painting_color: "white",
      painting_note: "",
      painting_vis: { t: "AllPlayers" },
    };
  }

  componentWillReceiveProps(nextProps: MapGridProps & M.DispatchProps) {
    if (nextProps.map.id !== this.props.map.id) {
      this.setState({
        terrain: I.Set(nextProps.map.terrain.map(I.List)),
        specials: M.specialsRPIToMap(nextProps.map.specials),
        painting: "Terrain",
      });
    }
  }

  shouldComponentUpdate(nextProps: MapGridProps & M.DispatchProps, nextState: MapGridState) {
    return this.props !== nextProps
      || !LD.isEqual(LD.omit(this.state, "painting_note"), LD.omit(nextState, "painting_note"));
  }

  render(): JSX.Element | null {
    const { terrain, specials, painting } = this.state;
    const map = {
      ...this.props.map, terrain: [],
      specials: [],
    };
    console.log("[EXPENSIVE:MapGrid.render]", terrain.toJS());

    const paintOpen = painting === "Terrain" ? this.closeTerrain.bind(this)
      : this.toggleSpecial.bind(this);
    const paintClosed = painting === "Terrain" ? this.openTerrain.bind(this)
      : this.toggleSpecial.bind(this);
    const open_tiles = terrain.toArray().map((spt: I.List<number>) => {
      const pt: T.Point3 = [spt.get(0)!, spt.get(1)!, spt.get(2)!];
      const tprops = tile_props("while", pt, { x: 1, y: 1 }, 0.0);
      return <rect {...tprops}
        style={{ cursor: 'pointer' }}
        onClick={() => paintOpen(pt)}
        key={pointKey("open", pt)} />;
    });
    const closed_tiles = M.filterMap(nearby_points([0, 0, 0]),
      pt => {
        if (terrain.has(I.List(pt))) { return; }
        const tprops = tile_props("black", pt, { x: 1, y: 1 }, 0.5);
        return <rect {...tprops} style={{ cursor: 'pointer' }}
          onClick={() => paintClosed(pt)}
          key={pointKey("closed", pt)} />;
      });

    const tools = this.mapEditingTools();
    return <div>
      {tools}
      <GridSvg map={map}>
        {getSpecials(M.specialsMapToRPI(specials))}
        {open_tiles}
        {closed_tiles}
        {getAnnotations(() => undefined, M.specialsMapToRPI(specials))}
      </GridSvg>
    </div>;
  }

  closeTerrain(pt: T.Point3) {
    this.setState({ terrain: this.state.terrain.delete(I.List(pt)) });
  }
  openTerrain(pt: T.Point3) {
    this.setState({ terrain: this.state.terrain.add(I.List(pt)) });
  }

  toggleSpecial(pt: T.Point3) {
    const { painting, painting_color, painting_note, painting_vis, specials } = this.state;
    if (painting !== "Special") { return; }
    const spt = I.List(pt);
    this.setState(
      {
        specials: specials.has(spt) ? specials.delete(spt)
          : specials.set(spt, [painting_color, painting_note, painting_vis]),
      });
  }

  mapEditingTools() {
    const { painting, painting_color } = this.state;
    const { dispatch } = this.props;
    return <div style={{ width: '100%', height: '50px', display: 'flex' }}>
      <Menu>
        <Menu.Item active={painting === "Terrain"}
          onClick={() => this.setState({ painting: "Terrain" })}>
          Terrain
        </Menu.Item>
        <Menu.Item active={painting === "Special"}
          onClick={() => this.setState({ painting: 'Special' })}>
          Special
        </Menu.Item>
      </Menu>
      <div style={{ display: 'flex', width: '250px', position: 'relative' }}>
        <Dimmer page={false} inverted={true} active={painting !== "Special"} />
        <CV.ModalMaker
          button={open =>
            <div onClick={open}
              style={{
                cursor: 'pointer',
                flex: '0 0 25px',
                ...CV.square_style(25, painting_color),
              }} />}
          header={<span>Choose Special Color</span>}
          content={close => <TwitterPicker
            onChangeComplete={
              color => {
                this.setState({ painting_color: color.hex });
                close();
              }}
          />}
        />
        <Input size="small" label="Annotation"
          onChange={(_, data) => this.setState({ painting_note: data.value })}
        />
        <Checkbox checked={this.state.painting_vis.t === "GMOnly"} label="GM Only"
          onChange={(_, d) =>
            this.setState({
              painting_vis: d.checked as boolean ? { t: "GMOnly" } : { t: "AllPlayers" },
            })} />
      </div>
      <Button style={{ marginLeft: 'auto' }} onClick={() =>
        dispatch(
          M.sendCommand({
            t: "EditMapTerrain",
            id: this.props.map.id,
            terrain: this.state.terrain.toArray().map(
              (spt: I.List<number>): T.Point3 => [spt.get(0)!, spt.get(1)!, spt.get(2)!]),
            specials: M.specialsMapToRPI(this.state.specials),
          }))}>
        Save
      </Button>
    </div >;
  }
}

interface SceneGridProps {
  scene: T.Scene;
  creatures: Obj<MapCreature>;
}
interface SceneGridState {
  targeting_point?: { point: T.Point3, rect: M.Rect };
  affected_points?: Array<T.Point3>;
}
export const SceneGrid = M.connectRedux(class SceneGrid
  extends React.Component<SceneGridProps & M.ReduxProps, SceneGridState> {
  constructor(props: SceneGridProps & M.ReduxProps) {
    super(props);
    this.state = {};
  }

  render(): JSX.Element {
    const { scene, creatures, ptui } = this.props;
    const map_ = M.get(ptui.app.current_game.maps, scene.map);
    if (!map_) { return <div>Couldn't find map</div>; }
    const map = map_; // WHY TYPESCRIPT, WHY???

    const grid = ptui.state.grid;

    const creature_menu = grid.active_menu ? this.renderMenu(grid.active_menu) : null;
    const annotation = grid.display_annotation ? this.renderAnnotation(map, grid.display_annotation)
      : null;

    const creature_els = LD.values(creatures).map(c =>
      <GridCreature key={c.creature.id} creature={c} />);
    const move = grid.movement_options;
    const movement_target_els = move
      ? move.options.map(pt => <MovementTarget key={pt.toString()} cid={move.cid} pt={pt}
        teleport={move.teleport} />)
      : [];

    const target_els = ptui.state.grid.target_options
      ? this.getTargetTiles(ptui.state.grid.target_options.options,
        (point, rect) => this.targetClicked(point, rect))
      : [];

    const affected_els = this.getAffectedTiles();

    return <div style={{ width: "100%", height: "100%" }}>
      <div style={{
        height: '45px', display: 'flex',
        justifyContent: 'space-between', alignItems: 'center',
      }}>
        <TopBar />
      </div>
      {creature_menu}
      {annotation}
      <GridSvg map={map}
        scene_background={scene.background_image_url}>
        {creature_els}
        {movement_target_els}
        {target_els}
        {affected_els}
      </GridSvg>
    </div>;
  }

  targetClicked(point: T.Point3, rect: M.Rect) {
    const { ptui, dispatch } = this.props;
    const options = ptui.state.grid.target_options!;
    const ability = ptui.app.current_game.abilities[options.ability_id];
    if (!ability) { return; }
    if (!ability.target.hasOwnProperty('volume')) { return; }
    const volume = (ability.target as any).volume;
    this.setState({ targeting_point: { point, rect } });
    M.fetchAffectedByVolume(dispatch, ptui.rpi_url, this.props.scene.id, point, volume).then(
      ({ points }) => this.setState({ affected_points: points }));
  }

  renderAnnotation(map: T.Map, { pt, rect }: { pt: T.Point3, rect: M.Rect }): JSX.Element {
    const { dispatch } = this.props;
    const special = LD.find(map.specials, ([pt_, ..._]) => M.isEqual(pt, pt_));
    if (!special) { return <noscript />; }
    return <RectPositioned rect={rect}
      onClose={() => dispatch({ type: "ToggleAnnotation", pt })}>
      <Segment>{special[2]}</Segment>
    </RectPositioned >;
  }

  renderMenu({ cid, rect }: { cid: T.CreatureID, rect: M.Rect }): JSX.Element {
    const { creatures, dispatch } = this.props;
    const creature_ = M.get(creatures, cid);
    if (!creature_) {
      return <noscript />;
    }
    const creature = creature_; // WHY TYPESCRIPT, WHY???
    return <RectPositioned rect={rect}
      onClose={() => dispatch({ type: "ActivateGridCreature", cid, rect })}>
      <Menu vertical={true}>
        <Menu.Item header={true}>
          {CV.classIcon(creature.creature)} {creature.creature.name}
        </Menu.Item>
        {
          creature.actions.entrySeq().toArray().map(
            ([actionName, action]) => {
              function onClick() {
                dispatch({ type: "ActivateGridCreature", cid, rect });
                action(cid);
              }
              return <Menu.Item key={actionName} onClick={() => onClick()}>
                {actionName}
              </Menu.Item>;
            })
        }
      </Menu>
    </RectPositioned>;
  }

  getTargetTiles(
    options: T.PotentialTargets,
    onClick: (pt: T.Point3, rect: M.Rect) => void): JSX.Element[] | undefined {
    switch (options.t) {
      case "CreatureIDs": return undefined;
      case "Points":
        return options.points.map(pt => {
          let element: SVGRectElement;
          const rprops = tile_props("pink", pt, { x: 1, y: 1 }, 0.3);
          function clickTile() {
            screenCoordsForRect(element);
            onClick(pt, screenCoordsForRect(element));
          }
          return <rect key={pointKey("target", pt)}
            ref={el => { if (el !== null) { element = el; } }}
            {...rprops} onClick={clickTile} />;
        });
    }
  }

  getAffectedTiles(): JSX.Element[] | undefined {
    if (!this.state.affected_points) { return; }
    return this.state.affected_points.map(
      pt => {
        const rprops = tile_props("red", pt, { x: 1, y: 1 }, 0.5);
        return <rect key={pointKey("affected", pt)}
          style={{ pointerEvents: "none" }}
          {...rprops} />;
      }
    );
  }
});

interface RectPositionedProps { rect: M.Rect; onClose: () => void; children: React.ReactChild; }
function RectPositioned(props: RectPositionedProps): JSX.Element {
  const { rect, onClose, children } = props;
  return <CV.ClickAway onClick={onClose}>
    <div
      style={{ position: "fixed", top: rect.sw.y, left: rect.sw.x }}>
      {children}
    </div>
  </CV.ClickAway>;
}

const TopBar = M.connectRedux(function TopBar(props): JSX.Element {
  const { ptui, dispatch } = props;
  if (ptui.state.grid.movement_options) {
    return <div>Select a destination or
       <Button onClick={() => dispatch({ type: 'ClearMovementOptions' })}>Cancel</Button>
    </div>;
  }
  if (ptui.state.grid.target_options) {
    return <div>Select a target or
      <Button onClick={() => dispatch({ type: 'ClearPotentialTargets' })}>Cancel</Button>
    </div>;
  }
  return <div>Ready to serve.</div>;
});

export interface MapCreature {
  creature: T.Creature;
  pos: T.Point3;
  class_: T.Class;
  actions: I.Map<string, (cid: T.CreatureID) => void>;
  visibility: T.Visibility;
}

interface GridSvgProps {
  map: T.Map;
  scene_background?: string;
  children?: React.ReactNode;
}
export const GridSvg = M.connectRedux(
  class GridSvg extends React.Component<GridSvgProps & M.ReduxProps> {
    spz: SPZ.SVGPanZoom;

    shouldComponentUpdate(nextProps: GridSvgProps & M.ReduxProps): boolean {
      const mvmt_diff = !M.isEqual(
        this.props.ptui.state.grid.movement_options,
        nextProps.ptui.state.grid.movement_options);
      const app_diff = !M.isEqual(this.props.ptui.app, nextProps.ptui.app);
      const focus_diff = !M.isEqual(this.props.ptui.state.grid_focus,
        nextProps.ptui.state.grid_focus);
      const map_diff = !M.isEqual(this.props.map, nextProps.map);
      const children_diff = !M.isEqual(this.props.children, nextProps.children);
      return app_diff || mvmt_diff || focus_diff || map_diff || children_diff;
    }

    componentDidUpdate(prevProps: GridSvgProps & M.ReduxProps) {
      if (!M.isEqual(prevProps.map, this.props.map) && this.spz) {
        this.spz.refresh();
      }
    }

    render(): JSX.Element {
      const { map, scene_background, ptui, dispatch } = this.props;
      console.log("[EXPENSIVE:GridSvg.render]");
      const open_terrain_color = map.background_image_url ? "transparent" : "white";
      const terrain_els = map.terrain.map(pt => tile(open_terrain_color, "base-terrain", pt));
      const background_image = map.background_image_url
        ? <image xlinkHref={map.background_image_url} width={map.background_image_scale[0]}
          height={map.background_image_scale[1]}
          x={map.background_image_offset[0]} y={map.background_image_offset[1]}
          preserveAspectRatio="none" />
        : null;

      return <SPZ.SVGPanZoom
        id="pt-grid"
        ref={
          (el: any
            /* I can't figure out how to prove that `el` is actually an SPZ.SVGPanZoom instance,
             * hence the `any` type in this `ref` */
          ) => { this.spz = el; }}
        preserveAspectRatio="xMinYMid slice"
        style={{
          width: "100%", height: "100%", backgroundColor: "rgb(215, 215, 215)",
          backgroundImage: scene_background ? `url(${scene_background})` : undefined,
          backgroundRepeat: "no-repeat",
          backgroundSize: "contain",
        }}>
        {background_image}
        {terrain_els}
        {getSpecials(map.specials, ptui.state.player_id)}
        {getAnnotations(dispatch, map.specials, ptui.state.player_id)}
        {this.props.children}
      </SPZ.SVGPanZoom>;
    }
  });

function getSpecials(
  specials: Array<[T.Point3, T.Color, string, T.Visibility]>, player_id?: T.PlayerID) {
  return specials.map(([pt, color, _, vis]) =>
    <SpecialTile key={pointKey("special", pt)} pt={pt} color={color} vis={vis}
      player_id={player_id} />);
}

function getAnnotations(
  dispatch: M.Dispatch,
  specials: Array<[T.Point3, T.Color, string, T.Visibility]>, player_id?: T.PlayerID) {
  return M.filterMap(specials,
    ([pt, _, note, vis]) => {
      if (note !== "") {
        return <Annotation key={pointKey("annotation", pt)} pt={pt} vis={vis} dispatch={dispatch}
          player_id={player_id} />;
      }
    });
}


const MovementTarget = M.connectRedux(
  function MovementTarget(
    props: { cid?: T.CreatureID; pt: T.Point3, teleport: boolean } & M.ReduxProps
  ): JSX.Element {
    const { cid, pt, ptui, dispatch, teleport } = props;
    const tprops = tile_props("cyan", pt);
    function moveCreature() {
      if (cid) {
        if (teleport) {
          ptui.setCreaturePos(dispatch, cid, pt);
        } else {
          ptui.moveCreature(dispatch, cid, pt);
        }
      } else {
        ptui.moveCombatCreature(dispatch, pt);
      }
    }
    return <rect {...tprops} fillOpacity="0.4" onClick={moveCreature} />;
  });

function SpecialTile(
  props: { color: string, vis: T.Visibility, pt: T.Point3, player_id?: T.PlayerID }): JSX.Element {
  const { color, vis, pt, player_id } = props;
  const gmonly = vis.t === "GMOnly";
  if (gmonly && player_id) {
    return <noscript />;
  }
  const tprops = tile_props(color, pt, { x: 1, y: 1 }, 0.5);
  return <g>
    <rect {...tprops} />
    {gmonly ? <text x={pt[0] * 100 + 65} y={pt[1] * 100 + 35} fontSize="25px">👁️</text>
      : <noscript />}
  </g>;
}


function Annotation({ dispatch, pt, vis, player_id }:
  { pt: T.Point3, vis: T.Visibility, player_id?: T.PlayerID } & M.DispatchProps)
  : JSX.Element {
  if (M.isEqual(vis, { t: "GMOnly" }) && player_id) {
    return <noscript />;
  }

  let element: SVGRectElement;

  function onClick() {
    dispatch({ type: "ToggleAnnotation", pt, rect: screenCoordsForRect(element) });
  }

  return <g>
    <rect width="100" height="100" x={pt[0] * 100} y={pt[1] * 100 - 50} fillOpacity="0"
      ref={el => { if (el !== null) { element = el; } }} onClick={onClick}
    />
    <text
      style={{ pointerEvents: "none" }}
      x={pt[0] * 100 + 25} y={pt[1] * 100 + 50}
      fontSize="100px" stroke="black" strokeWidth="2px" fill="white">*</text>
  </g>;
}


const GridCreature = M.connectRedux(
  function GridCreature({ ptui, dispatch, creature }:
    { creature: MapCreature } & M.ReduxProps): JSX.Element {
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

    const opacity = (creature.visibility.t === "GMOnly") ? "0.4" : "1.0";
    if (creature.creature.portrait_url !== "") {
      const props = tile_props("white", creature.pos, creature.creature.size);
      const bare_props = bare_tile_props(creature.pos, creature.creature.size);
      return <g opacity={opacity}>
        <image ref={el => { if (el !== null) { element = el; } }} key={creature.creature.id}
          xlinkHref={creature.creature.portrait_url} {...props} />
        <rect {...bare_props} {...highlightProps} fillOpacity="0" onClick={() => onClick()} />
      </g>;
    } else {
      const props = tile_props(creature.class_.color, creature.pos, creature.creature.size);
      return <g opacity={opacity} onClick={() => onClick()}>
        {<rect ref={el => { if (el !== null) { element = el; } }} {...props} {...highlightProps} />}
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
  const props = tile_props(color, pos, size);
  return <rect key={pointKey(keyPrefix, pos)} {...props} />;
}

function bare_tile_props(pt: T.Point3, size = { x: 1, y: 1 }): React.SVGProps<SVGElement> {
  return {
    width: 100 * size.x, height: 100 * size.y,
    rx: 5, ry: 5,
    x: pt[0] * 100, y: (pt[1] * 100) - 50,
  };
}

function tile_props(color: string, pt: T.Point3, size = { x: 1, y: 1 }, opacity: number = 1):
  React.SVGProps<SVGElement> {
  return {
    ...bare_tile_props(pt, size), stroke: "black", strokeWidth: 1, fill: color,
    fillOpacity: opacity,
  };
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

export function mapCreatures(ptui: M.PTUI, dispatch: M.Dispatch, scene: T.Scene)
  : { [index: string]: MapCreature } {
  const creatures = M.filterMap(
    ptui.getSceneCreatures(scene),
    creature => {
      const [pos, vis] = scene.creatures.get(creature.id)!; // map over keys -> .get() is ok
      const class_ = ptui.app.current_game.classes.get(creature.class_);
      if (class_) {
        let actions: I.Map<string, (cid: T.CreatureID) => void> = I.Map();
        const target = targetAction(creature);
        if (target) {
          actions = actions.set(target.name, target.action);
        }
        return { creature, pos, class_, actions, visibility: vis };
      }
    });
  const result: { [index: string]: MapCreature } = {};
  for (const creature of creatures) {
    result[creature.creature.id] = creature;
  }
  return result;

  function targetAction(
    creature: T.Creature): { name: string, action: ((cid: T.CreatureID) => void) } | undefined {
    if (ptui.state.grid.target_options) {
      const { ability_id, options } = ptui.state.grid.target_options;
      if (options.t !== "CreatureIDs") { return undefined; }
      // this is quadratic (TODO: switch options.cids to a hashmap)
      if (LD.includes(options.cids, creature.id)) {
        const ability = M.get(ptui.app.current_game.abilities, ability_id);
        if (ability) {
          return {
            name: `${ability.name} this creature`,
            action: cid => { ptui.executeCombatAbility(dispatch, cid); },
          };
        }
      }
    }
  }

}

export function nearby_points(pos: T.Point3): Array<T.Point3> {
  const result = [];
  for (const x of LD.range(pos[0] - 20, pos[0] + 20)) {
    for (const y of LD.range(pos[1] - 20, pos[1] + 20)) {
      result.push([x, y, pos[2]] as T.Point3);
    }
  }
  return result;
}

export function requestTeleport(dispatch: M.Dispatch, scene: T.Scene, cid: T.CreatureID) {
  const scene_creature = scene.creatures.get(cid);
  if (scene_creature) {
    dispatch({
      type: 'DisplayMovementOptions',
      cid, teleport: true, options: nearby_points(scene_creature[0]),
    });
  }
}

function pointKey(prefix: string, pt: T.Point3) {
  return `${prefix}-(${pt[0]}/${pt[1]}/${pt[2]})`;
}
