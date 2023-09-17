import * as I from 'immutable';
import * as LD from "lodash";
import * as React from "react";

import { Button, Menu } from 'semantic-ui-react';

import * as CV from "./CommonView";
import * as GM from "./GMComponents";
import * as M from "./Model";
import * as T from "./PTTypes";
import * as SPZ from './SVGPanZoom';

interface Obj<T> { [index: string]: T; }

interface SceneGridProps {
  scene: T.Scene;
  creatures: Obj<MapCreature>;
}
export function SceneGrid(props: SceneGridProps) {
  const [targetingPoint, setTargetingPoint] = React.useState<T.Point3|undefined>();
  const [affectedPoints, setAffectedPoints] = React.useState<T.Point3[]|undefined>();
  const [affectedCreatures, setAffectedCreatures] = React.useState<T.CreatureID[] | undefined>();
  const [painting, setPainting] = React.useState<"Opening" | "Closing" | undefined>();

  const { scene } = props;
  const grid = M.useState(s => s.grid); // This is bad.
  const focus = M.useState(s => s.gridFocus);
  const playerID = M.useState(s => s.playerId);

  const menu = grid.active_objects.objects.length !== 0
    ? renderGridObjectMenu(grid.active_objects)
    : grid.context_menu ? contextMenu(grid.context_menu.pt, grid.context_menu.coords)
      : null;

  const target_els = grid.target_options
    ? getTargetTiles(grid.target_options.options,
      point => targetClicked(point))
    : [];

  const layer = focus?.layer;
  const disable_style = layer ? { pointerEvents: "none", opacity: 0.3 } : {};

  const [bg_width, bg_height] = scene.background_image_scale;
  const background_image = scene.background_image_url && scene.background_image_offset
    ? <image xlinkHref={scene.background_image_url} width={bg_width ? bg_width : undefined}
      height={bg_height ? bg_height : undefined}
      x={scene.background_image_offset[0]} y={scene.background_image_offset[1]}
      preserveAspectRatio="none" />
    : null;
  const static_background = scene.background_image_url && !scene.background_image_offset
    ? `url(${scene.background_image_url})`
    : undefined;

  const current_terrain = layer && layer.t === "Terrain" ? layer.terrain : scene.terrain;
  const open_terrain_color = scene.background_image_url ? "transparent" : "white";
  const closed_terrain_els = layer && layer.t === "Terrain"
    ? getClosedTerrain(current_terrain) : null;
  const open_terrain_els = current_terrain.map(pt => tile(open_terrain_color, "base-terrain", pt));

  const highlights = layer && layer.t === "Highlights"
    ? getEditableHighlights(layer.highlights)
    : getHighlights(scene.highlights, playerID);
  const annotations = getAnnotations(scene.annotations, playerID);

  const scene_hotspots = getSceneHotspots();

  const volumes = layer && layer.t === "Volumes"
    ? getEditableVolumes()
    : getVolumeConditions();

  const annotations_style = layer
    && (layer.t === "Terrain" || layer.t === "Highlights")
    ? disable_style
    : {};
  const highlights_style = layer && (layer.t !== "Highlights") ? disable_style : {};
  const volumes_style = layer && layer.t !== "Volumes" ? disable_style : {};
  const scene_hotspots_style = layer && layer.t !== "LinkedScenes" ? disable_style : {};

  return <div style={{ width: "100%", height: "100%" }}>
    <div style={{
      height: '45px', display: 'flex',
      justifyContent: 'space-between', alignItems: 'center',
    }}>
      {topBar()}
    </div>
    {menu}
    <SPZ.SVGPanZoom
      id="pt-grid"
      preserveAspectRatio="xMinYMid slice"
      style={{
        width: "100%", height: "100%", backgroundColor: "rgb(215, 215, 215)",
        backgroundImage: static_background,
        backgroundRepeat: "no-repeat",
        backgroundSize: "contain",
      }}
      onMouseDown={ev => {
        if (ev.button !== 0 || ev.ctrlKey) { return; }
        if (layer && layer.t === "Terrain") {
          const pt = getPoint3AtMouse(ev);
          const painting = layer.terrain.contains(pt) ? "Closing" : "Opening";
          setPainting(painting);
        }
      }}
      onMouseMove={ev => {
        if (!layer || layer.t !== "Terrain" || painting === undefined) { return; }
        // make sure we've actually got the mouse down before trying to paint
        if (ev.buttons !== 1) {
          setPainting(undefined);
          return;
        }
        const pt = getPoint3AtMouse(ev);
        let terrain;
        switch (painting) {
          case "Opening": {
            if (layer.terrain.contains(pt)) { return; }
            terrain = layer.terrain.add(pt);
            break;
          }
          case "Closing": {
            if (!layer.terrain.contains(pt)) { return; }
            terrain = layer.terrain.remove(pt);
            break;
          }
          default: return;
        }
        M.getState().setTerrain(terrain);
      }}
      onMouseUp={ev => {
        if (!layer || layer.t !== "Terrain" || painting === undefined) { return; }
        const pt = getPoint3AtMouse(ev);
        switch (painting) {
          case "Opening": {
            if (layer.terrain.contains(pt)) { return; }
            M.getState().setTerrain(layer.terrain.add(pt));
            break;
          }
          case "Closing": {
            if (!layer.terrain.contains(pt)) { return; }
            M.getState().setTerrain(layer.terrain.remove(pt));
            break;
          }
        }
        setPainting(undefined);
      }}
      onMouseLeave={() => setPainting(undefined)}
      onContextMenu={ev => {
        ev.preventDefault();
        const pt = getPoint3AtMouse(ev);
        if (playerID === undefined) {
          M.getState().activateContextMenu(pt, [ev.clientX, ev.clientY]);
        }
      }}
      shouldPan={ev => {
        if (layer && (layer.t === "Terrain" || layer.t === "Highlights")) { return false; }
        const objects = findPTObjects(ev);
        return objects.length === 0;
      }}
    >
      {background_image}
      <g id="terrain">{closed_terrain_els}{open_terrain_els}</g>
      <g id="volume-conditions" style={volumes_style}>{volumes}</g>
      <g id="scene_hotspots" style={scene_hotspots_style}>{scene_hotspots}</g>
      <g id="creatures" style={disable_style}>{getCreatures()}</g>
      <g id="highlights" style={highlights_style}>{highlights}</g>
      <g id="annotations" style={annotations_style}>{annotations}</g>
      <g id="movement-targets" style={disable_style}>{getMovementTargets()}</g>
      <g id="targets" style={disable_style}>{target_els}</g>
      <g id="affected" style={disable_style}>{getAffectedTiles()}</g>
      <g id="targeted-volume" style={disable_style}>{drawTargetedVolume()}</g>
    </SPZ.SVGPanZoom>
  </div>;

  function getSceneHotspots() {
    return props.scene.scene_hotspots.entrySeq().toArray().map(
      ([pos, scene_id]) =>
        <SceneHotSpot key={`scene-hotspot-${scene_id}-${pos.toString()}`}
          pos={pos} scene_id={scene_id} />
    );
  }

  function getMovementTargets() {
    const move = grid.movement_options;
    return move
      ? move.options.map(pt => <MovementTarget key={pt.toString()} cid={move.cid} pt={pt}
        teleport={move.teleport} />)
      : null;
  }

  function getCreatures() {
    const creatures = LD.sortBy(props.creatures, c => -c.creature.size.x);
    return LD.values(creatures).map(c => {
      const highlight = LD.includes(affectedCreatures, c.creature.id)
        ? "red" : undefined;
      return <GridCreature key={c.creature.id} creature={c} highlight={highlight} />;
    });
  }

  function getClosedTerrain(terrain: T.Terrain) {
    const closed_tiles = M.filterMap(nearby_points(new T.Point3(0, 0, 0)),
      pt => {
        if (terrain.contains(pt)) { return; }
        const tprops = tile_props("black", pt, { x: 1, y: 1 }, 0.5);
        return <rect {...tprops} style={{ cursor: 'pointer' }}
          key={pointKey("closed", pt)} />;
      });
    return closed_tiles;
  }

  function getHighlights(highlights: T.Highlights, player_id?: T.PlayerID, onClick?: (pt: T.Point3) => void) {
    return highlights.entrySeq().map(([pt, [color, vis]]) => {
      const gmonly = vis.t === "GMOnly";
      if (gmonly && player_id) {
        return null;
      }
      const tprops = tile_props(color, pt, { x: 1, y: 1 }, 0.5);
      const style = onClick === undefined ? { pointerEvents: "none" } : undefined;
      const clicker = onClick !== undefined ? () => onClick(pt) : undefined;
      return <g key={pointKey("highlight", pt)} style={style} onClick={clicker}>
        <rect {...tprops} />
        {gmonly
          ? eyeball(pt)
          : null}
      </g>;
    });
  }

  function getEditableHighlights(highlights: T.Highlights) {
    const color = grid.highlight_color;
    const vis = grid.object_visibility;
    function removeHighlight(pt: T.Point3) {
      M.getState().setHighlights(highlights.remove(pt));
    }
    function addHighlight(pt: T.Point3) {
      M.getState().setHighlights(highlights.set(pt, [color, vis]));
    }
    const highlighted_tiles = getHighlights(highlights, undefined, pt => removeHighlight(pt));
    const empty_tiles = M.filterMap(nearby_points(new T.Point3(0, 0, 0)),
      pt => {
        if (highlights.has(pt)) { return; }
        const tprops = tile_props("black", pt, { x: 1, y: 1 }, 0.0);
        return <rect key={pointKey("non-high", pt)} {...tprops} onClick={() => addHighlight(pt)} />;
      });

    return <>
      <g key="existing-highlights" id="existing-highlights">{highlighted_tiles}</g>,
      <g key="empty-highlights" id="empty-highlights">{empty_tiles}</g>
    </>;
  }

  function getAnnotations(
    annotations: T.Annotations, player_id?: T.PlayerID,
    specialClick?: (pt: T.Point3) => void) {
    return M.filterMap(annotations.entrySeq().toArray(),
      ([pt, [note, vis]]) => {
        if (note !== "") {
          return <Annotation key={pointKey("annotation", pt)} pt={pt} vis={vis}
            specialClick={specialClick}
            player_id={player_id} />;
        }
      });
  }

  function getEditableVolumes(): Array<JSX.Element> | undefined {
    return getVolumeConditions(0.5, (vcid, _, me) => {
      const coords: [number, number] = [me.pageX, me.pageY];
      M.getState().activateObjects([{ t: "VolumeCondition", id: vcid }], coords);
    }
    );
  }

  function getVolumeConditions(
    fillOpacity: number = 0.1,
    onClick: (id: T.ConditionID, vc: T.VolumeCondition, evt: React.MouseEvent<any>) => void
      = () => undefined
  ): Array<JSX.Element> | undefined {
    return props.scene.volume_conditions.entrySeq().map(([id, vol_cond]) =>
      svgVolume(id, vol_cond.volume, vol_cond.point,
        {
          fill: "green", fillOpacity: `${fillOpacity}`, strokeOpacity: "0.5",
          style: { pointerEvents: "auto" },
          onClick: me => onClick(id, vol_cond, me),
        })).toArray();
  }

  function drawTargetedVolume(): JSX.Element | undefined {
    const { scene } = props;
    const options = grid.target_options;
    if (!options) { return; }
    if (!targetingPoint) { return; }
    const ability_id = options.ability_id;
    const ability = M.useState(s => s.getAbility(ability_id));
    if (!ability) { return; }
    const action = ability.action;

    switch (action.t) {
      case "Creature":
        switch (action.target.t) {
          case "AllCreaturesInVolumeInRange":
            return svgVolume("target-volume", action.target.volume, targetingPoint);
          case "LineFromActor":
            const caster_pos = M.getCreaturePos(scene, options.cid);
            if (!caster_pos) { return; }
            return <line x1={caster_pos.x + 50} y1={caster_pos.y + 50}
              x2={targetingPoint.x + 50} y2={targetingPoint.y + 50}
              style={{ pointerEvents: "none" }}
              strokeWidth="3" stroke="black" />;
        }
        return;
      case "SceneVolume":
        switch (action.target.t) {
          case "RangedVolume":
            return svgVolume("target-volume", action.target.volume, targetingPoint);
        }
    }
  }

  async function targetClicked(point: T.Point3) {
    const options = grid.target_options!;
    const ability = M.getState().getAbility(options.ability_id);
    if (!ability) { return; }
    switch (ability.action.t) {
      case "Creature":
        switch (ability.action.target.t) {
          case "SomeCreaturesInVolumeInRange":
          case "AllCreaturesInVolumeInRange":
          case "LineFromActor":
            break;
          default: return;
        }
        break;
      case "SceneVolume":
        switch (ability.action.target.t) {
          case "RangedVolume": break;
          default: return;
        }
    }
    setTargetingPoint(point);
    const {points, creatures} = await M.fetchAbilityTargets(props.scene.id, options.cid, options.ability_id, point);
    setAffectedPoints(points);
    setAffectedCreatures(creatures);
  }

  function contextMenu(pt: T.Point3, coords: [number, number]) {
    const close = () => M.getState().clearContextMenu();
    return <PopupMenu coords={coords} onClose={close}>
      <ContextMenu scene={props.scene} pt={pt} onClose={close} />
    </PopupMenu>;
  }

  function renderGridObjectMenu(arg: { objects: Array<M.GridObject>; coords: [number, number] }) {
    const { objects, coords } = arg;
    const { scene, creatures } = props;
    const close = () => M.getState().clearContextMenu();
    return <PopupMenu coords={coords} onClose={close}>
      <Menu vertical={true}>
        {objects.map(obj => {
          switch (obj.t) {
            case "Annotation": return annotationMenu(close, scene, obj.pt);
            case "SceneHotSpot":
              return <SceneHotspotMenu closeMenu={close} scene={scene} target_scene_id={obj.scene_id} pt={obj.pt} />;
            case "Creature": return creatureMenu(close, creatures, obj.id);
            case "VolumeCondition": return volumeConditionMenu(close, scene, obj.id);
          }
        }
        )}
      </Menu>
    </PopupMenu>;
  }

  function annotationMenu(close: () => void, scene: T.Scene, pt: T.Point3) {
    const ann = scene.annotations.get(pt);
    const delet = () => {
      const annotations = scene.annotations.delete(pt);
      M.sendCommand({ t: "EditSceneAnnotations", scene_id: scene.id, annotations });
      close();
    };
    if (ann) {
      return <>
        <Menu.Item key="ANN">
          <Menu.Header>Annotation: {ann[0]}</Menu.Header>
        </Menu.Item>
        {playerID ? null :
          <Menu.Item onClick={delet}>Delete this annotation</Menu.Item>}
      </>;
    }
    return;
  }

  function creatureMenu(closeMenu: () => void, creatures: Obj<MapCreature>, creature_id: T.CreatureID) {
    const creature = M.get(creatures, creature_id);
    if (creature) {
      return <React.Fragment key="CREATURE">
        <Menu.Item key={creature.creature.id} header={true}>
          <CV.ClassIcon class_id={creature.creature.class_} /> {creature.creature.name}
        </Menu.Item>
        {creature.actions.entrySeq().toArray().map(
          ([actionName, action]) => {
            const onClick = () => { closeMenu(); action(creature_id); };
            return <Menu.Item key={actionName} onClick={() => onClick()}>
              {actionName}
            </Menu.Item>;
          })}
      </React.Fragment>;
    }
    return;
  }

  function volumeConditionMenu(
    closeMenu: () => void, scene: T.Scene, condition_id: T.ConditionID) {
    const onClick = () => {
      M.sendCommand({ t: "RemoveSceneVolumeCondition", scene_id: scene.id, condition_id });
      closeMenu();
    };
    // unimplemented!: put a name here
    return <>
      <Menu.Item key="Header" header={true}>Condition</Menu.Item>
      <Menu.Item key="Remove VC" onClick={() => onClick()}>Remove</Menu.Item>
    </>;
  }

  function getTargetTiles(
    options: T.PotentialTargets,
    onClick: (pt: T.Point3) => void): JSX.Element[] | undefined {
    switch (options.t) {
      case "CreatureIDs": return undefined;
      case "Points":
        return options.points.map(pt => {
          const rprops = tile_props("pink", pt, { x: 1, y: 1 }, 0.3);
          function clickTile() {
            onClick(pt);
          }
          return <rect key={pointKey("target", pt)} {...rprops} onClick={clickTile} />;
        });
    }
  }

  function getAffectedTiles(): JSX.Element[] | undefined {
    if (!affectedPoints) { return; }
    return affectedPoints.map(
      pt => {
        const rprops = tile_props("red", pt, { x: 1, y: 1 }, 0.5);
        return <rect key={pointKey("affected", pt)}
          style={{ pointerEvents: "none" }}
          {...rprops} />;
      }
    );
  }

  function topBar(): JSX.Element {
    if (targetingPoint) {
      return <div>Proceed with action?
        <Button onClick={() => executePointTargetedAbility()}>Act</Button>
        <Button onClick={() => clearTargets()}>Cancel</Button>
      </div>;
    }

    if (grid.movement_options) {
      return <div>Select a destination or
       <Button onClick={() => M.getState().clearMovementOptions()}>Cancel</Button>
      </div>;
    }
    if (grid.target_options) {
      return <div>Select a target or
      <Button onClick={() => M.getState().clearPotentialTargets()}>Cancel</Button>
      </div>;
    }
    return <div>Drag to pan, mousewheel to zoom, click objects for more options,
      right-click to add things.</div>;
  }

  function clearTargets() {
    setAffectedPoints(undefined);
    setAffectedCreatures(undefined);
    setTargetingPoint(undefined);
  }

  function executePointTargetedAbility() {
    if (!targetingPoint) { return; }
    M.executeCombatPointTargetedAbility(targetingPoint);
    clearTargets();
  }
}

function SceneHotspotMenu(
  props: {closeMenu: () => void, scene: T.Scene, target_scene_id: T.SceneID, pt: T.Point3}) {
  const linked_scene = M.useState(s => s.getScene(props.target_scene_id));
  if (!linked_scene) { return; }
  const jumpScene = () => {
    M.getState().setGridFocus(linked_scene.id);
    props.closeMenu();
  };
  const deleteHotspot = () => {
    const scene_hotspots = props.scene.scene_hotspots.remove(props.pt);
    M.sendCommand({ t: "EditSceneSceneHotspots", scene_id: props.scene.id, scene_hotspots });
    props.closeMenu();
  };
  return <React.Fragment key="Scene-Hotspot">
    <Menu.Item><Menu.Header>Scene Hotspot</Menu.Header></Menu.Item>
    <Menu.Item style={{ cursor: 'pointer' }} onClick={jumpScene}>
      {linked_scene.name}
    </Menu.Item>
    <Menu.Item style={{ cursor: 'pointer' }} onClick={deleteHotspot}>
      Delete Hotspot
    </Menu.Item>
  </React.Fragment>;
}

function getPoint3AtMouse(event: React.MouseEvent<any>) {
  const svg = document.getElementById("pt-grid") as any as SVGSVGElement;
  const g = document.getElementById("svg-pan-zoom-viewport") as any as SVGGElement;
  const pt = svg.createSVGPoint();
  pt.x = event.clientX;
  pt.y = event.clientY;

  // The cursor point, translated into svg coordinates
  const cursorpt = pt.matrixTransform(g.getScreenCTM().inverse());
  const x = Math.floor(cursorpt.x / 100) * 100;
  const y = Math.floor(cursorpt.y / 100) * 100;
  return new T.Point3(x, y, 0);
}


interface ContextMenuProps {
  scene: T.Scene;
  pt: T.Point3;
  onClose: () => void;
}
interface ContextMenuState { visible: boolean; }
class ContextMenu extends React.Component<ContextMenuProps, ContextMenuState> {
  constructor(props: ContextMenuProps) {
    super(props);
    this.state = { visible: true };
  }
  render() {
    const { scene, pt, onClose } = this.props;
    const hideAnd = (open: () => void) => () => { this.setState({ visible: false }); open(); };
    // TODO: oh crap, this popup should probably only happen for GMs.
    const items: Array<[string, (c: () => void) => JSX.Element]> = [
      ["Add Scene Hotspot",
        close => <GM.AddSceneHotspot scene={scene} pt={pt} onClose={close} />],
      ["Add Annotation", close => <GM.AddAnnotation scene={scene} pt={pt} onClose={close} />],
    ];
    return <Menu vertical={true} style={{ display: this.state.visible ? 'block' : 'none' }}>
      {items.map(([title, comp]) => <CV.ModalMaker
        key={title}
        button={open =>
          <Menu.Item style={{ cursor: 'pointer' }} onClick={hideAnd(open)}>
            {title}</Menu.Item>}
        header={<>{title}</>}
        content={comp}
        onClose={onClose}
      />)}
    </Menu>;
  }
}

function svgVolume(
  key: string, volume: T.Volume, pt: T.Point3, props?: React.SVGProps<SVGCircleElement>
): JSX.Element {
  switch (volume.t) {
    case "Sphere":
      return <circle key={key} cx={pt.x + 50} cy={pt.y + 50} r={volume.radius}
        style={{ pointerEvents: "none" }}
        strokeWidth={3} stroke="black" fill="none" {...props} />;
    default:
      console.log("unimplemented! svgvolume for", volume);
      return <g key={key} />;
  }
}

function eyeball(pt: T.Point3): JSX.Element {
  return <text x={pt.x} y={pt.y} dominantBaseline="hanging" fontSize="25px">👁️</text>;
}


interface SceneHotSpotProps { scene_id: T.SceneID; pos: T.Point3; }
function SceneHotSpot(props: SceneHotSpotProps) {
  const { pos, scene_id } = props;
  const tprops = bare_tile_props(pos);
  const element = React.useRef<SVGRectElement>(null);

  const onClick = (ev: React.MouseEvent<any>) => element.current && activateGridObjects(ev, element.current);
  const reflection_props = {
    'data-pt-type': 'scene-hotspot', 'data-pt-scene-id': scene_id,
    'data-pt-pos': T.encodePoint3(pos),
  };
  return <g>
    <rect {...tprops} onClick={onClick} style={{ cursor: 'pointer' }} fillOpacity="0"
      ref={element}
      {...reflection_props} />
    <text
      style={{ pointerEvents: "none" }}
      x={pos.x + 50} y={pos.y}
      dominantBaseline="hanging"
      textAnchor="middle"
      fontSize="65px" stroke="black" strokeWidth="2px" fill="white">🔗</text>
  </g>;
}

interface PopupMenuProps {
  coords: [number, number];
  onClose: () => void;
  children: React.ReactNode;
}
function PopupMenu(props: PopupMenuProps): JSX.Element {
  const { coords, onClose, children } = props;
  return <CV.ClickAway onClick={onClose}>
    <div
      style={{ position: "fixed", left: coords[0], top: coords[1] }}>
      {children}
    </div>
  </CV.ClickAway>;
}

export interface MapCreature {
  creature: T.Creature;
  pos: T.Point3;
  class_: T.Class;
  actions: I.Map<string, (cid: T.CreatureID) => void>;
  visibility: T.Visibility;
}

function MovementTarget(props: { cid?: T.CreatureID; pt: T.Point3; teleport: boolean }) {
  const { cid, pt, teleport } = props;
  const tprops = tile_props("cyan", pt);
  function moveCreature() {
    if (cid) {
      if (teleport) {
        M.setCreaturePos(cid, pt);
      } else {
        M.moveCreature(cid, pt);
      }
    } else {
      M.moveCombatCreature(pt);
    }
  }
  return <rect {...tprops} fillOpacity="0.4" onClick={moveCreature} />;
}

interface AnnotationProps {
  pt: T.Point3;
  vis: T.Visibility;
  player_id?: T.PlayerID;
  specialClick?: (pt: T.Point3) => void;
}
function Annotation(props: AnnotationProps): JSX.Element | null {
  const { pt, vis, player_id, specialClick } = props;
  if (M.isEqual(vis, { t: "GMOnly" }) && player_id) {
    return null;
  }

  const element = React.useRef<SVGRectElement>(null);

  const onClick = (event: React.MouseEvent<any>) => {
    if (specialClick) {
      specialClick(pt);
    } else {
      if (element.current) activateGridObjects(event, element.current);
    }
  };

  const reflection_props = { 'data-pt-type': "annotation", 'data-pt-pos': T.encodePoint3(pt) };
  return <g>
    <rect width="100" height="100" x={pt.x} y={pt.y} fillOpacity="0"
      ref={element} onClick={onClick}
      style={{ cursor: 'pointer' }}
      {...reflection_props}
    />
    <text
      style={{ pointerEvents: "none" }}
      dominantBaseline="hanging"
      textAnchor="middle"
      x={pt.x + 50} y={pt.y}
      fontSize="100px" stroke="black" strokeWidth="2px" fill="white">*</text>
    {vis.t === "GMOnly" ? eyeball(pt) : null}
  </g>;
}

function activateGridObjects(event: React.MouseEvent<any>, element: SVGRectElement | SVGImageElement) {
  const objects = findPTObjects(event);
  const coords = screenCoordsForRect(element);
  M.getState().activateObjects(objects, coords);
}

function findPTObjects(event: React.MouseEvent<any>): Array<M.GridObject> {
  return findElementsAtPoint(
    event.pageX, event.pageY,
    (el): M.GridObject | undefined => {
      const type = el.getAttribute('data-pt-type');
      if (type) {
        switch (type) {
          case "creature": {
            const id = el.getAttribute('data-pt-id');
            if (!id) { return; }
            return { t: "Creature", id };
          }
          case "annotation": {
            const pt = el.getAttribute('data-pt-pos');
            if (!pt) { return; }
            return { t: "Annotation", pt: T.parsePoint3(pt) };
          }
          case "scene-hotspot": {
            const scene_id = el.getAttribute('data-pt-scene-id');
            const pt = el.getAttribute('data-pt-pos');
            if (!scene_id || !pt) { return; }
            return { t: "SceneHotSpot", scene_id, pt: T.parsePoint3(pt) };
          }
        }
      }
    },
    'svg');
}

/**
 * Find all elements under a specific coordinate.
 */
function findElementsAtPoint<R>(
  x: number, y: number, filterNode: (el: HTMLElement) => (R | undefined), stopAt: string = 'html'
): Array<R> {
  const results: Array<R> = [];
  const dirtied: Array<{ el: HTMLElement; oldPE: string | null }> = [];
  let el: HTMLElement | null;
  while (true) {
    if (dirtied.length > 100) {
      console.log("[findElementsAtPoint] giving up on determining creatures under click");
      return results;
    }
    el = document.elementFromPoint(x, y) as HTMLElement | null;
    if (!el) { break; }
    if (el.tagName === stopAt) { break; }
    const result = filterNode(el);
    if (result) {
      results.push(result);
    }
    // This is a hack! elementFromPoint finds the most specific element that has
    // pointerEvents enabled. So, in order to find the next one behind this one,
    // we have to disable pointer events and loop. Later, we restore all the
    // pointerEvents values.
    if (el.style.pointerEvents !== 'none') {
      dirtied.push({ el, oldPE: el.style.pointerEvents });
      el.style.pointerEvents = 'none';
    }
  }
  for (const { el, oldPE } of dirtied) {
    el.style.pointerEvents = oldPE || '';
  }
  return results;
}

function GridCreature({ creature, highlight }: { creature: MapCreature; highlight?: string }) {
  const element = React.useRef<SVGImageElement | SVGRectElement>(null);
  const combat = M.useState(s => s.getGame().current_combat);
  const targetOptions = M.useState(s => s.grid.target_options);
  const currentCreatureId = M.useState(s => s.getCurrentCombatCreatureID());

  const onClick = (event: React.MouseEvent<any>) => {
    if (element.current) {
      activateGridObjects(event, element.current);
    } else {
      console.log("NO ELEMENT!!!!!!!!!!", element);
    }}

  const highlightProps: React.SVGAttributes<SVGGraphicsElement> = {};
  if (combat && currentCreatureId === creature.creature.id) {
    highlightProps.stroke = "black";
    highlightProps.strokeWidth = 3;
  }
  if (targetOptions?.options.t === "CreatureIDs") {
    if (LD.includes(targetOptions.options.cids, creature.creature.id)) {
      highlightProps.stroke = "red";
      highlightProps.strokeWidth = 3;
    }
  }
  if (highlight) {
    highlightProps.stroke = highlight;
    highlightProps.strokeWidth = 15;
  }

  const opacity = (creature.visibility.t === "GMOnly") ? "0.4" : "1.0";
  const reflection_props = { 'data-pt-type': "creature", 'data-pt-id': creature.creature.id };

  return <g key={creature.creature.id} opacity={opacity} onClick={onClick}
    style={{ cursor: 'pointer' }}>
    {contents()}
  </g>;

  function contents() {
    if (creature.creature.icon_url !== "") {
      const props = tile_props("white", creature.pos, creature.creature.size);
      const bare_props = bare_tile_props(creature.pos, creature.creature.size);
      return <>
        <image key="image" ref={element} xlinkHref={creature.creature.icon_url} {...props} />
        <rect key="rect" {...bare_props} {...reflection_props} {...highlightProps} fillOpacity="0" />
      </>;
    } else {
      const props = tile_props(creature.class_.color, creature.pos, creature.creature.size);
      return <>
        <rect ref={element} {...props}
          {...reflection_props}
          {...highlightProps} />
        <text style={{ pointerEvents: "none" }} fontSize="50"
          x={creature.pos.x + 50} y={creature.pos.y}
          dominantBaseline="hanging"
          textAnchor="middle">
          {creature.creature.name.slice(0, 4)}
        </text>
      </>;
    }
  }
}

function tile(color: string, keyPrefix: string, pos: T.Point3, size?: { x: number; y: number })
  : JSX.Element {
  const props = tile_props(color, pos, size);
  return <rect key={pointKey(keyPrefix, pos)} {...props} />;
}

function bare_tile_props(pt: T.Point3, size = { x: 1, y: 1 }): React.SVGProps<SVGElement> {
  return {
    width: 100 * size.x, height: 100 * size.y,
    rx: 5, ry: 5,
    x: pt.x, y: pt.y,
  };
}

function tile_props(color: string, pt: T.Point3, size = { x: 1, y: 1 }, opacity: number = 1):
  React.SVGProps<SVGElement> {
  return {
    ...bare_tile_props(pt, size), stroke: "black", strokeWidth: 1, fill: color,
    fillOpacity: opacity,
  };
}

function screenCoordsForRect(rect: SVGRectElement | SVGImageElement): [number, number] {
  const svg = document.getElementById("pt-grid") as any as SVGSVGElement;
  const matrix = rect.getScreenCTM();
  if (!matrix) {
    throw new Error("Couldn't get screen CTM");
  }
  const pt = svg.createSVGPoint();
  pt.x = rect.x.animVal.value;
  pt.y = rect.y.animVal.value;
  pt.y += rect.height.animVal.value;
  const sw = pt.matrixTransform(matrix);
  return [sw.x, sw.y];
}

/**
 * Create the `MapCreature`s for all creatures in a scene. This is common code shared for player
 * and GM views.
 */
export function mapCreatures(state: M.AllStates, scene: T.Scene): { [index: string]: MapCreature } {
  const targetOptions = state.grid.target_options;
  const sceneCreatures = state.getSceneCreatures(scene);
  const creatures = M.filterMap(
    sceneCreatures,
    creature => {
      const [pos, vis] = scene.creatures.get(creature.id)!; // map over keys -> .get() is ok
      const class_ = state.getClass(creature.class_);
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

  function targetAction(creature: T.Creature)
    : { name: string; action: ((cid: T.CreatureID) => void) } | undefined {
    if (targetOptions) {
      const { ability_id, options } = targetOptions;
      if (options.t !== "CreatureIDs") { return undefined; }
      // this is quadratic (TODO: switch options.cids to a hashmap)
      if (LD.includes(options.cids, creature.id)) {
        const ability = state.getAbility(ability_id);
        if (ability) {
          return {
            name: `${ability.name} this creature`,
            action: cid => { M.executeCombatAbility(cid); },
          };
        }
      }
    }
  }
}

export function nearby_points(pos: T.Point3): Array<T.Point3> {
  const result = [];
  for (const x of LD.range(pos.x - 2000, pos.x + 2000, 100)) {
    for (const y of LD.range(pos.y - 2000, pos.y + 2000, 100)) {
      result.push(new T.Point3(x, y, pos.z));
    }
  }
  return result;
}

export function requestTeleport(scene: T.Scene, cid: T.CreatureID) {
  const scene_creature = scene.creatures.get(cid);
  if (scene_creature) {
    M.getState().displayMovementOptions(nearby_points(scene_creature[0]), cid, true);
  }
}

function pointKey(prefix: string, pt: T.Point3) {
  return `${prefix}-(${pt.x}/${pt.y}/${pt.z})`;
}
