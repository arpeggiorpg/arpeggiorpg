import { Set } from "immutable";
import isEqual from "lodash/isEqual";
import range from "lodash/range";
import sortBy from "lodash/sortBy";
import * as React from "react";
import { Button, Menu } from "semantic-ui-react";

import * as A from "./Actions";
import * as CV from "./CommonView";
import * as M from "./Model";
import * as T from "./PTTypes";
import { AddAnnotation, AddSceneHotspot } from "./Scene";
import * as SPZ from "./SVGPanZoom";

export function SceneGrid(props: { creatureIds: T.CreatureID[]; actionProducer: ActionProducer }) {
  const [painting, setPainting] = React.useState<"Opening" | "Closing" | undefined>();

  const playerID = M.useState(s => s.playerId);
  const layerType = M.useState(s => s.gridFocus?.layer?.t);

  const hasScene = M.useState(s => !!s.getFocusedScene());

  if (!hasScene) return <div>No scene!</div>;

  const disable_style: React.CSSProperties = layerType
    ? { pointerEvents: "none", opacity: 0.3 }
    : {};

  const annotations_style = (layerType === "Terrain" || layerType === "Highlights")
    ? disable_style
    : {};
  const highlights_style = layerType !== "Highlights" ? disable_style : {};
  const volumes_style = layerType !== "Volumes" ? disable_style : {};
  const scene_hotspots_style = layerType !== "LinkedScenes" ? disable_style : {};

  return (
    <div style={{ width: "100%", height: "100%" }}>
      <div
        style={{
          height: "45px",
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
        }}
      >
        <TopBar />
      </div>
      <GridMenu actionProducer={props.actionProducer} />
      <SPZ.SVGPanZoom
        id="pt-grid"
        preserveAspectRatio="xMinYMid slice"
        style={{
          width: "100%",
          height: "100%",
          backgroundColor: "rgb(215, 215, 215)",
          backgroundRepeat: "no-repeat",
          backgroundSize: "contain",
        }}
        onMouseDown={mouseDown}
        onMouseMove={mouseMove}
        onMouseUp={mouseUp}
        onMouseLeave={() => setPainting(undefined)}
        onContextMenu={ev => {
          ev.preventDefault();
          const pt = getPoint3AtMouse(ev);
          if (playerID === undefined) {
            M.getState().activateContextMenu(pt, [ev.clientX, ev.clientY]);
          }
        }}
        shouldPan={ev =>
          (layerType !== "Terrain" && layerType !== "Highlights") && findPTObjects(ev).length === 0}
      >
        <BackgroundImage />
        <Terrain />
        <g id="volume-conditions" style={volumes_style}>
          <VolumeConditions />
        </g>
        <g id="scene_hotspots" style={scene_hotspots_style}>
          <SceneHotspots />
        </g>
        <g id="creatures" style={disable_style}>
          <Creatures creatureIds={props.creatureIds} />
        </g>
        <g id="highlights" style={highlights_style}>
          <Highlights />
        </g>
        <g id="annotations" style={annotations_style}>
          <Annotations />
        </g>
        <g id="movement-targets" style={disable_style}>
          <MovementTargets />
        </g>
        <g id="targets" style={disable_style}>
          <TargetTiles />
        </g>
        <g id="affected" style={disable_style}>
          <AffectedTiles />
        </g>
        <g id="targeted-volume" style={disable_style}>
          <TargetedVolume />
        </g>
      </SPZ.SVGPanZoom>
    </div>
  );

  function mouseDown(ev: React.MouseEvent<SVGSVGElement>) {
    if (ev.button !== 0 || ev.ctrlKey) return;
    const layer = M.getState().gridFocus?.layer;
    if (layer && ["Terrain", "Highlights"].includes(layer.t)) {
      const pt = getPoint3AtMouse(ev);
      const painting = (layer.t === "Terrain"
          ? layer.terrain.contains(pt)
          : layer.t === "Highlights"
          ? layer.highlights.has(pt)
          : false)
        ? "Closing"
        : "Opening";
      setPainting(painting);
    }
  }
  function mouseMove(ev: React.MouseEvent<SVGSVGElement>) {
    const layer = M.getState().gridFocus?.layer;
    if (!layer || !["Terrain", "Highlights"].includes(layer.t) || painting === undefined) {
      return;
    }
    const grid = M.getState().grid;
    // make sure we've actually got the mouse down before trying to paint
    if (ev.buttons !== 1) {
      setPainting(undefined);
      return;
    }
    const pt = getPoint3AtMouse(ev);
    switch (painting) {
      case "Opening": {
        if (layer.t === "Terrain") {
          if (layer.terrain.contains(pt)) return;
          M.getState().setTerrain(layer.terrain.add(pt));
        } else if (layer.t === "Highlights") {
          if (layer.highlights.has(pt)) return;
          const color = grid.highlight_color;
          const vis = grid.object_visibility;
          M.getState().setHighlights(layer.highlights.set(pt, [color, vis]));
        }
        break;
      }
      case "Closing": {
        if (layer.t === "Terrain") {
          if (!layer.terrain.contains(pt)) return;
          M.getState().setTerrain(layer.terrain.remove(pt));
        } else if (layer.t === "Highlights") {
          if (!layer.highlights.has(pt)) return;
          M.getState().setHighlights(layer.highlights.remove(pt));
        }
        break;
      }
      default:
        return;
    }
  }

  function mouseUp(ev: React.MouseEvent<SVGSVGElement>) {
    const layer = M.getState().gridFocus?.layer;
    if (!layer || layer.t !== "Terrain" || painting === undefined) return;
    const pt = getPoint3AtMouse(ev);
    switch (painting) {
      case "Opening": {
        if (layer.terrain.contains(pt)) return;
        M.getState().setTerrain(layer.terrain.add(pt));
        break;
      }
      case "Closing": {
        if (!layer.terrain.contains(pt)) return;
        M.getState().setTerrain(layer.terrain.remove(pt));
        break;
      }
    }
    setPainting(undefined);
  }
}

function BackgroundImage() {
  const pendingScale = M.useState(s => s.pendingBackgroundScale);
  const pendingOffset = M.useState(s => s.pendingBackgroundOffset);

  const background = M.useState(s => {
    const scene = s.getFocusedScene();
    if (!scene) return;
    return {
      url: scene.background_image_url,
      scale: scene.background_image_scale,
      offset: scene.background_image_offset,
    };
  }, isEqual);

  const [bgXScale, bgYScale] = pendingScale ? pendingScale : background?.scale || [1.0, 1.0];
  const [offsetX, offsetY] = pendingOffset
    ? pendingOffset
    : background?.offset || [0, 0];
  return background?.url
    ? (
      <image
        xlinkHref={background.url}
        style={{ transform: `scale(${bgXScale}, ${bgYScale})` }}
        x={offsetX}
        y={offsetY}
        preserveAspectRatio="none"
      />
    )
    : null;
}

function MovementTargets() {
  const move = M.useState(s => s.grid.movement_options, isEqual);
  return move
    ? move.options.map(pt => (
      <MovementTarget key={pt.toString()} cid={move.cid} pt={pt} teleport={move.teleport} />
    ))
    : null;
}

function Annotations() {
  const annotations = M.useState(s => s.getFocusedScene()?.annotations);
  const playerId = M.useState(s => s.playerId);
  if (!annotations) return;
  return M.filterMap(annotations.entrySeq().toArray(), ([pt, [note, vis]]) => {
    if (note !== "") {
      return (
        <Annotation
          key={pointKey("annotation", pt)}
          pt={pt}
          vis={vis}
          player_id={playerId}
        />
      );
    }
  });
}

function TargetTiles() {
  const targetOptions = M.useState(s => s.grid.target_options);
  const options = targetOptions?.options;

  if (!options) return;

  if ("CreatureIDs" in options) {
    return undefined;
  } else if ("Points" in options) {
    return options.Points.map(pt => {
      const rprops = tile_props<SVGRectElement>("pink", pt, { x: 1, y: 1 }, 0.3);
      return <rect key={pointKey("target", pt)} {...rprops} onClick={() => targetClicked(pt)} />;
    });
  }

  async function targetClicked(point: T.Point3) {
    const grid = M.getState().grid;
    const sceneId = M.getState().getFocusedScene()?.id;
    if (!sceneId) return;
    const options = grid.target_options!;
    const ability = M.getState().getAbility(options.ability_id);
    if (!ability) return;

    if ("Creature" in ability.action) {
      const creatureTarget = ability.action.Creature.target;
      if (
        // This enum representation is so horrible :-(
        typeof creatureTarget !== "string" && (
          "SomeCreaturesInVolumeInRange" in creatureTarget
          || "AllCreaturesInVolumeInRange" in creatureTarget
          || "LineFromActor" in creatureTarget
        )
      ) {
        // we good
      } else {
        return;
      }
    }
    if ("SceneVolume" in ability.action) {
      if ("RangedVolume" in ability.action.SceneVolume.target) {
        // we good
      } else {
        return;
      }
    }
    M.getState().setTargetingPoint(point);
    const { points, creatures } = await A.fetchAbilityTargets(
      sceneId,
      options.cid,
      options.ability_id,
      point,
    );
    M.getState().setAffectedPoints(points);
    M.getState().setAffectedCreatures(creatures);
  }
}

function AffectedTiles() {
  const affectedPoints = M.useState(s => s.affectedPoints);
  if (!affectedPoints) return;
  return affectedPoints.map(
    pt => {
      const rprops = tile_props<SVGRectElement>("red", pt, { x: 1, y: 1 }, 0.5);
      return (
        <rect
          key={pointKey("affected", pt)}
          style={{ pointerEvents: "none" }}
          {...rprops}
        />
      );
    },
  );
}

function TopBar() {
  const isMoving = M.useState(s => !!s.grid.movement_options);
  const isTargeting = M.useState(s => !!s.grid.target_options);
  const targetingPoint = M.useState(s => s.targetingPoint);
  if (targetingPoint) {
    return (
      <div>
        Proceed with action?
        <Button onClick={() => executePointTargetedAbility()}>Act</Button>
        <Button onClick={() => clearTargets()}>Cancel</Button>
      </div>
    );
  }

  if (isMoving) {
    return (
      <div>
        Select a destination or
        <Button onClick={() => M.getState().clearMovementOptions()}>Cancel</Button>
      </div>
    );
  }
  if (isTargeting) {
    return (
      <div>
        Select a target or
        <Button onClick={() => M.getState().clearPotentialTargets()}>Cancel</Button>
      </div>
    );
  }
  return (
    <div>
      Drag to pan, mousewheel to zoom, click objects for more options, right-click to add things.
    </div>
  );

  function executePointTargetedAbility() {
    if (!targetingPoint) return;
    A.executeCombatPointTargetedAbility(targetingPoint);
    clearTargets();
  }

  function clearTargets() {
    const state = M.getState();
    state.setAffectedPoints(undefined);
    state.setAffectedCreatures([]);
    state.setTargetingPoint(undefined);
  }
}

function Highlights_() {
  const highlights = M.useState(s => {
    const scene = s.getFocusedScene();
    if (!scene) return;
    const layer = s.gridFocus?.layer;
    const pendingHighlights = layer?.t === "Highlights" && layer.highlights;
    return pendingHighlights || scene.highlights;
  });
  const playerId = M.useState(s => s.playerId);
  if (!highlights) return <div>No highlights</div>;
  return highlights.entrySeq().map(([pt, [color, vis]]) => {
    const gmonly = vis === "GMOnly";
    if (gmonly && playerId) {
      return null;
    }
    const tprops = tile_props<SVGRectElement>(color, pt, { x: 1, y: 1 }, 0.5);
    return (
      <g key={pointKey("highlight", pt)} style={{ pointerEvents: "none" }}>
        <rect {...tprops} />
        {gmonly
          ? eyeball(pt)
          : null}
      </g>
    );
  });
}

const Highlights = React.memo(Highlights_);

function Creatures(
  { creatureIds }: {
    creatureIds: T.CreatureID[];
  },
) {
  const creaturesWithColor = M.useState(
    s =>
      sortBy(s.getCreatures(creatureIds), c => -c.size.x).map(c => ({
        id: c.id,
        color: s.affectedCreatures.includes(c.id) ? "red" : undefined,
      })),
    isEqual,
  );
  return creaturesWithColor.map(({ id, color }) => {
    return <GridCreature key={id} creatureId={id} highlight={color} />;
  });
}

/** The Menu either renders menus for active objects, or a right-click context menu. */
function GridMenu(props: { actionProducer: ActionProducer }) {
  const hasActiveObjects = M.useState(s => s.grid.active_objects.objects.length > 0);
  const contextMenuActive = M.useState(s => !!s.grid.context_menu);

  return hasActiveObjects
    ? <GridObjectMenu actionProducer={props.actionProducer} />
    : contextMenuActive
    ? <ContextMenu />
    : null;
}

/** ContextMenu is what gets rendered when you right click. */
function ContextMenu() {
  const contextMenu = M.useState(s => s.grid?.context_menu);
  if (!contextMenu) return;
  const { pt, coords } = contextMenu;
  const close = () => M.getState().clearContextMenu();

  const [visible, setVisible] = React.useState<boolean>(true);

  const items: Array<[string, (c: () => void) => JSX.Element]> = [
    ["Add Scene Hotspot", close => <AddSceneHotspot pt={pt} onClose={close} />],
    ["Add Annotation", close => <AddAnnotation pt={pt} onClose={close} />],
  ];

  return (
    <PopupMenu coords={coords} onClose={close}>
      <Menu vertical={true} style={{ display: visible ? "block" : "none" }}>
        {items.map(([title, comp]) => (
          <CV.ModalMaker
            key={title}
            button={open => (
              <Menu.Item style={{ cursor: "pointer" }} onClick={hideAnd(open)}>
                {title}
              </Menu.Item>
            )}
            header={<>{title}</>}
            content={comp}
            onClose={close}
          />
        ))}
      </Menu>
    </PopupMenu>
  );

  function hideAnd(open: () => void) {
    return () => {
      setVisible(false);
      open();
    };
  }
}

/** The GridObjectMenu renders sub-menus for every object which is at a point.
 */
function GridObjectMenu({ actionProducer }: { actionProducer: ActionProducer }) {
  const { objects, coords } = M.useState(s => s.grid.active_objects, isEqual);
  const close = () => M.getState().clearContextMenu();
  return (
    <PopupMenu coords={coords} onClose={close}>
      <Menu vertical={true}>
        {objects.map(obj => {
          switch (obj.t) {
            case "Annotation":
              return <AnnotationMenu key={pointKey("ann", obj.pt)} onClose={close} pt={obj.pt} />;
            case "SceneHotSpot":
              return (
                <SceneHotspotMenu
                  key={pointKey("hot", obj.pt)}
                  onClose={close}
                  targetSceneId={obj.scene_id}
                  pt={obj.pt}
                />
              );
            case "Creature":
              return (
                <CreatureMenu
                  key={`creat-${obj.id}`}
                  onClose={close}
                  actionProducer={actionProducer}
                  creatureId={obj.id}
                />
              );
            case "VolumeCondition":
              return <VolumeConditionMenu key={`vc-${obj.id}`} onClose={close} id={obj.id} />;
          }
        })}
      </Menu>
    </PopupMenu>
  );
}

function AnnotationMenu({ onClose, pt }: { onClose: () => void; pt: T.Point3 }) {
  const isPlayer = M.useState(s => !!s.playerId);
  const ann = M.useState(s => s.getFocusedScene()?.annotations.get(pt));
  if (!ann) return;
  const delet = () => {
    const scene = M.getState().getFocusedScene();
    if (scene) {
      const annotations = scene.annotations.delete(pt);
      if (annotations) {
        A.sendGMCommand({ t: "EditSceneAnnotations", scene_id: scene.id, annotations });
      }
    }
    onClose();
  };
  return (
    <>
      <Menu.Item key="ANN">
        <Menu.Header>Annotation: {ann[0]}</Menu.Header>
      </Menu.Item>
      {isPlayer
        ? null
        : <Menu.Item onClick={delet}>Delete this annotation</Menu.Item>}
    </>
  );
}

function SceneHotspotMenu(
  { onClose, targetSceneId, pt }: { onClose: () => void; targetSceneId: T.SceneID; pt: T.Point3 },
) {
  const playerId = M.useState(s => s.playerId);
  const linked_scene = M.useState(s => s.getScene(targetSceneId));
  if (!linked_scene) return;
  const jumpScene = () => {
    M.getState().setGridFocus(targetSceneId);
    onClose();
  };
  const deleteHotspot = () => {
    const scene = M.getState().getFocusedScene();
    if (scene) {
      const scene_hotspots = scene.scene_hotspots.remove(pt);
      A.sendGMCommand({ t: "EditSceneSceneHotspots", scene_id: scene.id, scene_hotspots });
    }
    onClose();
  };
  return (
    <React.Fragment key="Scene-Hotspot">
      <Menu.Item>
        <Menu.Header>Scene Hotspot</Menu.Header>
      </Menu.Item>
      <Menu.Item style={{ cursor: "pointer" }} onClick={jumpScene}>
        {linked_scene.name}
      </Menu.Item>
      {playerId ? null : (
        <Menu.Item style={{ cursor: "pointer" }} onClick={deleteHotspot}>
          Delete Hotspot
        </Menu.Item>
      )}
    </React.Fragment>
  );
}

function CreatureMenu(
  { onClose, actionProducer, creatureId }: {
    onClose: () => void;
    actionProducer: ActionProducer;
    creatureId: T.CreatureID;
  },
) {
  const creature = M.useState(s => {
    const creature = s.getCreature(creatureId);
    if (!creature) return;
    return { id: creature.id, name: creature.name, class: creature.class };
  });
  const actions = M.useState(
    s => defaultActions(s, creatureId).concat(actionProducer(s, creatureId)),
    isEqual,
  );
  if (!creature) return <div>Lost creature</div>;
  if (creature) {
    return (
      <React.Fragment key={`creature-menu-${creature.id}`}>
        <Menu.Item key={creature.id} header={true}>
          <CV.ClassIcon class_id={creature.class} /> {creature.name}
        </Menu.Item>
        {actions.map(
          ({ actionName, action }) => {
            const onClick = () => {
              onClose();
              action(creatureId);
            };
            return (
              <Menu.Item key={actionName} onClick={() => onClick()}>
                {actionName}
              </Menu.Item>
            );
          },
        )}
      </React.Fragment>
    );
  }
  return;
}

function VolumeConditionMenu(
  { onClose, id }: {
    onClose: () => void;
    id: T.ConditionID;
  },
) {
  const onClick = () => {
    const scene = M.getState().getFocusedScene();
    if (scene) {
      A.sendGMCommand({ t: "RemoveSceneVolumeCondition", scene_id: scene.id, condition_id: id });
    }
    onClose();
  };
  // unimplemented!: put a name here
  return (
    <>
      <Menu.Item key="Header" header={true}>Condition</Menu.Item>
      <Menu.Item key="Remove VC" onClick={() => onClick()}>Remove</Menu.Item>
    </>
  );
}

function VolumeConditions_() {
  const volumeConditions = M.useState(
    s => s.getFocusedScene()?.volume_conditions.entrySeq(),
    isEqual,
  );
  const editingVolumes = M.useState(s => s.gridFocus?.layer?.t === "Volumes");
  const fillOpacity = editingVolumes ? 0.5 : 0.1;
  if (!volumeConditions) return;
  return volumeConditions.map(([id, vol_cond]) =>
    svgVolume(id, vol_cond.volume, vol_cond.point, {
      fill: "green",
      fillOpacity: `${fillOpacity}`,
      strokeOpacity: "0.5",
      style: { pointerEvents: "auto" },
      onClick: me => onClick(id, me),
    })
  ).toArray();

  function onClick(id: T.ConditionID, me: React.MouseEvent<any>) {
    if (!editingVolumes) return;
    const coords: [number, number] = [me.pageX, me.pageY];
    M.getState().activateObjects([{ t: "VolumeCondition", id }], coords);
  }
}

const VolumeConditions = React.memo(VolumeConditions_, isEqual);

function TargetedVolume() {
  const action = M.useState(s => {
    const options = s.grid.target_options;
    if (!options) return;
    const ability_id = options.ability_id;
    const ability = s.getAbility(ability_id);
    if (!ability) return;
    return ability.action;
  }, isEqual);
  const actorId = M.useState(s => s.grid.target_options?.cid);
  const targetingPoint = M.useState(s => s.targetingPoint);
  if (!targetingPoint || !action) return;

  if ("Creature" in action) {
    if (typeof action.Creature.target === "string") return;
    if ("AllCreaturesInVolumeInRange" in action.Creature.target) {
      return svgVolume(
        "target-volume",
        action.Creature.target.AllCreaturesInVolumeInRange.volume,
        targetingPoint,
      );
    }
    if ("LineFromActor" in action.Creature.target) {
      if (!actorId) {
        console.error("trying to draw a LineFromError but don't have an actor");
        return;
      }
      return <LineFromActor targetingPoint={targetingPoint} actorId={actorId} />;
    }
  } else if ("SceneVolume" in action) {
    return svgVolume(
      "target-volume",
      action.SceneVolume.target.RangedVolume.volume,
      targetingPoint,
    );
  }
}

function LineFromActor(
  { actorId, targetingPoint }: { actorId: T.CreatureID; targetingPoint: T.Point3 },
) {
  const casterPos = M.useState(s => {
    const scene = s.getFocusedScene();
    if (!scene) return;
    return M.optMap(scene.creatures.get(actorId), ([pos, _]) => pos);
  });
  if (!casterPos) return;
  // TODO: DO SOMETHING with LineFromActor.distance
  return (
    <line
      x1={casterPos.x + 50}
      y1={casterPos.y + 50}
      x2={targetingPoint.x + 50}
      y2={targetingPoint.y + 50}
      style={{ pointerEvents: "none" }}
      strokeWidth="3"
      stroke="black"
    />
  );
}

function SceneHotspots_() {
  const hotspots = M.useState(
    s => s.getFocusedScene()?.scene_hotspots.entrySeq().toArray(),
    isEqual,
  );
  if (hotspots) {
    return hotspots.map(
      ([pos, scene_id]) => (
        <SceneHotSpot
          key={`scene-hotspot-${scene_id}-${pos.toString()}`}
          pos={pos}
          scene_id={scene_id}
        />
      ),
    );
  }
}

const SceneHotspots = React.memo(SceneHotspots_, isEqual);

function Terrain_() {
  const currentTerrain = M.useState(s => {
    const layer = s.gridFocus?.layer;
    const scene = s.getFocusedScene();
    const terrain = layer?.t === "Terrain" ? layer.terrain : scene?.terrain;
    return terrain?.sort();
  }, isEqual) || Set();
  const openTerrainColor = M.useState(s =>
    s.getFocusedScene()?.background_image_url ? "transparent" : "white"
  );

  const openTerrain = currentTerrain.map(pt => (
    <OpenTerrainTile color={openTerrainColor} key={pointKey("base-terrain", pt)} pt={pt} />
  ));

  return <g id="terrain">{openTerrain}</g>;
}
const Terrain = React.memo(Terrain_, isEqual);

function OpenTerrainTile_(
  { color, pt, size }: {
    color: string;
    pt: T.Point3;
    size?: { x: number; y: number };
  },
): JSX.Element {
  const props = tile_props<SVGRectElement>(color, pt, size);
  return <rect {...props} />;
}
const OpenTerrainTile = React.memo(OpenTerrainTile_, isEqual);

function getPoint3AtMouse(event: React.MouseEvent<any>) {
  // "as": getElementById has no typed version, so we unfortunately have to assert here.
  const svg = document.getElementById("pt-grid") as any as SVGSVGElement;
  const g = document.getElementById("svg-pan-zoom-viewport") as any as SVGGElement;
  const pt = svg.createSVGPoint();
  pt.x = event.clientX;
  pt.y = event.clientY;

  // The cursor point, translated into svg coordinates
  const ctm = g.getScreenCTM();
  if (!ctm) {
    throw new Error();
  }
  const cursorpt = pt.matrixTransform(ctm.inverse());
  const x = Math.floor(cursorpt.x / 100) * 100;
  const y = Math.floor(cursorpt.y / 100) * 100;
  return new T.Point3(x, y, 0);
}

function svgVolume(
  key: string,
  volume: T.Volume,
  pt: T.Point3,
  props?: React.SVGProps<SVGCircleElement>,
): JSX.Element {
  if ("Sphere" in volume) {
    return (
      <circle
        key={key}
        cx={pt.x + 50}
        cy={pt.y + 50}
        r={volume.Sphere}
        style={{ pointerEvents: "none" }}
        strokeWidth={3}
        stroke="black"
        fill="none"
        {...props}
      />
    );
  }
  console.log("unimplemented! svgvolume for", volume);
  return <g key={key} />;
}

function eyeball(pt: T.Point3): JSX.Element {
  return <text x={pt.x} y={pt.y} dominantBaseline="hanging" fontSize="25px">👁️</text>;
}

interface SceneHotSpotProps {
  scene_id: T.SceneID;
  pos: T.Point3;
}
function SceneHotSpot(props: SceneHotSpotProps) {
  const { pos, scene_id } = props;
  const tprops = bare_tile_props(pos);
  const element = React.useRef<SVGRectElement>(null);

  const onClick = (ev: React.MouseEvent<any>) =>
    element.current && activateGridObjects(ev, element.current);
  const reflection_props = {
    "data-pt-type": "scene-hotspot",
    "data-pt-scene-id": scene_id,
    "data-pt-pos": T.encodePoint3(pos),
  };
  return (
    <g>
      <rect
        {...tprops}
        onClick={onClick}
        style={{ cursor: "pointer" }}
        fillOpacity="0"
        ref={element}
        {...reflection_props}
      />
      <text
        style={{ pointerEvents: "none" }}
        x={pos.x + 50}
        y={pos.y}
        dominantBaseline="hanging"
        textAnchor="middle"
        fontSize="65px"
        stroke="black"
        strokeWidth="2px"
        fill="white"
      >
        🔗
      </text>
    </g>
  );
}

interface PopupMenuProps {
  coords: [number, number];
  onClose: () => void;
  children: React.ReactNode;
}
function PopupMenu(props: PopupMenuProps): JSX.Element {
  const { coords, onClose, children } = props;
  return (
    <CV.ClickAway onClick={onClose}>
      <div
        style={{ position: "fixed", left: coords[0], top: coords[1] }}
      >
        {children}
      </div>
    </CV.ClickAway>
  );
}

export type Action = { actionName: string; action: (cid: T.CreatureID) => void };
export type ActionProducer = (state: M.AllStates, creatureId: T.CreatureID) => Action[];

function MovementTarget(props: { cid?: T.CreatureID; pt: T.Point3; teleport: boolean }) {
  const { cid, pt, teleport } = props;
  const tprops = tile_props<SVGRectElement>("cyan", pt);
  function moveCreature() {
    if (cid) {
      if (teleport) {
        A.setCreaturePos(cid, pt);
      } else {
        A.moveCreature(cid, pt);
      }
    } else {
      A.moveCombatCreature(pt);
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
  if (M.isEqual(vis, "GMOnly") && player_id) {
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

  const reflection_props = { "data-pt-type": "annotation", "data-pt-pos": T.encodePoint3(pt) };
  return (
    <g>
      <rect
        width="100"
        height="100"
        x={pt.x}
        y={pt.y}
        fillOpacity="0"
        ref={element}
        onClick={onClick}
        style={{ cursor: "pointer" }}
        {...reflection_props}
      />
      <text
        style={{ pointerEvents: "none" }}
        dominantBaseline="hanging"
        textAnchor="middle"
        x={pt.x + 50}
        y={pt.y}
        fontSize="100px"
        stroke="black"
        strokeWidth="2px"
        fill="white"
      >
        *
      </text>
      {vis === "GMOnly" ? eyeball(pt) : null}
    </g>
  );
}

function activateGridObjects(event: React.MouseEvent<any>, element: MySVGElement) {
  const objects = findPTObjects(event);
  const coords = screenCoordsForRect(element);
  M.getState().activateObjects(objects, coords);
}

function findPTObjects(event: React.MouseEvent<any>): Array<M.GridObject> {
  return findElementsAtPoint(
    event.pageX,
    event.pageY,
    (el): M.GridObject | undefined => {
      const type = el.getAttribute("data-pt-type");
      if (type) {
        switch (type) {
          case "creature": {
            const id = el.getAttribute("data-pt-id");
            if (!id) return;
            return { t: "Creature", id };
          }
          case "annotation": {
            const pt = el.getAttribute("data-pt-pos");
            if (!pt) return;
            return { t: "Annotation", pt: T.parsePoint3(pt) };
          }
          case "scene-hotspot": {
            const scene_id = el.getAttribute("data-pt-scene-id");
            const pt = el.getAttribute("data-pt-pos");
            if (!scene_id || !pt) return;
            return { t: "SceneHotSpot", scene_id, pt: T.parsePoint3(pt) };
          }
        }
      }
    },
    "svg",
  );
}

/**
 * Find all elements under a specific coordinate.
 */
function findElementsAtPoint<R>(
  x: number,
  y: number,
  filterNode: (el: SVGElement) => R | undefined,
  stopAt: string = "html",
): Array<R> {
  const results: Array<R> = [];
  const dirtied: Array<{ el: SVGElement; oldPE: string | null }> = [];
  let el: SVGElement | null;
  while (true) {
    if (dirtied.length > 100) {
      console.log("[findElementsAtPoint] giving up on determining creatures under click");
      return results;
    }
    // "as": elementFromPoint returns *Element*, which doesn't have a `style`
    // attribute, and unfortunately there is no generic version of
    // elementFromPoint that does a type-check.
    el = document.elementFromPoint(x, y) as SVGElement | null;
    if (!el) break;
    if (el.tagName === stopAt) break;
    const result = filterNode(el);
    if (result) {
      results.push(result);
    }
    // This is a hack! elementFromPoint finds the most specific element that has
    // pointerEvents enabled. So, in order to find the next one behind this one,
    // we have to disable pointer events and loop. Later, we restore all the
    // pointerEvents values.
    if (el.style.pointerEvents !== "none") {
      dirtied.push({ el, oldPE: el.style.pointerEvents });
      el.style.pointerEvents = "none";
    }
  }
  for (const { el, oldPE } of dirtied) {
    el.style.pointerEvents = oldPE || "";
  }
  return results;
}

function GridCreature({ creatureId, highlight }: { creatureId: T.CreatureID; highlight?: string }) {
  const element = React.useRef<MySVGElement | null>(null);
  const combat = M.useState(s => s.game.current_combat);
  const targetOptions = M.useState(s => s.grid.target_options);
  const currentCreatureId = M.useState(s => s.getCurrentCombatCreatureID());
  const creatureData = M.useState(getCreatureData, isEqual);

  if (!creatureData) return;

  const highlightProps: React.SVGAttributes<SVGGraphicsElement> = {};
  if (combat && currentCreatureId === creatureData.id) {
    highlightProps.stroke = "black";
    highlightProps.strokeWidth = 3;
  }
  if (
    targetOptions?.options
    && "CreatureIDs" in targetOptions.options
    && targetOptions.options.CreatureIDs.includes(creatureData.id)
  ) {
    highlightProps.stroke = "red";
    highlightProps.strokeWidth = 3;
  }
  if (highlight) {
    highlightProps.stroke = highlight;
    highlightProps.strokeWidth = 15;
  }

  const opacity = (creatureData.visibility === "GMOnly") ? "0.4" : "1.0";
  const reflection_props = { "data-pt-type": "creature", "data-pt-id": creatureData.id };

  return (
    <g key={creatureData.id} opacity={opacity} onClick={onClick} style={{ cursor: "pointer" }}>
      {contents(creatureData)}
    </g>
  );

  function onClick(event: React.MouseEvent<any>) {
    if (element.current) {
      activateGridObjects(event, element.current);
    } else {
      console.log("NO ELEMENT!!!!!!!!!!", element);
    }
  }

  interface CreatureData {
    id: string;
    name: string;
    classColor: string;
    icon: string;
    visibility: T.Visibility;
    pos: T.Point3;
    size: T.AABB;
  }

  function getCreatureData(s: M.AllStates): CreatureData | undefined {
    const creature = s.getCreature(creatureId);
    if (!creature) return;
    const class_ = s.getClass(creature.class);
    if (!class_) throw new Error("no class :-(");
    const scene = s.getFocusedScene();
    if (!scene) throw new Error("no scene :-(");
    const sceneCreature = scene.creatures.get(creature.id);
    if (!sceneCreature) throw new Error("no scene creature :-(");
    return {
      id: creature.id,
      name: creature.name,
      classColor: class_.color,
      icon: creature.icon_url,
      visibility: sceneCreature[1],
      pos: sceneCreature[0],
      size: creature.size,
    };
  }

  function contents(creatureData: CreatureData) {
    if (creatureData.icon !== "") {
      const props = tile_props<SVGImageElement>("white", creatureData.pos, creatureData.size);
      const bare_props = bare_tile_props<SVGRectElement>(creatureData.pos, creatureData.size);
      // we need to use the old-fashioned ref callback syntax here to avoid some
      // type errors. This is a workaround that is still type-safe.
      return (
        <>
          <image
            key="image"
            ref={e => {
              element.current = e;
            }}
            xlinkHref={creatureData.icon}
            {...props}
          />
          <rect
            key="rect"
            {...bare_props}
            {...reflection_props}
            {...highlightProps}
            fillOpacity="0"
          />
        </>
      );
    } else {
      const props = tile_props<SVGRectElement>(
        creatureData.classColor,
        creatureData.pos,
        creatureData.size,
      );
      return (
        <>
          <rect
            ref={e => {
              element.current = e;
            }}
            {...props}
            {...reflection_props}
            {...highlightProps}
          />
          <text
            style={{ pointerEvents: "none" }}
            fontSize="50"
            x={creatureData.pos.x + 50}
            y={creatureData.pos.y}
            dominantBaseline="hanging"
            textAnchor="middle"
          >
            {creatureData.name.slice(0, 4)}
          </text>
        </>
      );
    }
  }
}

function bare_tile_props<T>(pt: T.Point3, size = { x: 1, y: 1 }): React.SVGProps<T> {
  return {
    width: 100 * size.x,
    height: 100 * size.y,
    rx: 5,
    ry: 5,
    x: pt.x,
    y: pt.y,
  };
}

function tile_props<T>(
  color: string,
  pt: T.Point3,
  size = { x: 1, y: 1 },
  opacity: number = 1,
): React.SVGProps<T> {
  return {
    ...bare_tile_props<T>(pt, size),
    stroke: "black",
    strokeWidth: 1,
    fill: color,
    fillOpacity: opacity,
  };
}

// For some reason, even though SVGImageElement and SVGRectElement share all of
// these fields, they are not factored out into their own interface in
// lib.dom.d.ts.
interface MySVGElement {
  x: SVGAnimatedLength;
  y: SVGAnimatedLength;
  height: SVGAnimatedLength;
  width: SVGAnimatedLength;
  getScreenCTM: () => DOMMatrix | null;
}

function screenCoordsForRect(rect: MySVGElement): [number, number] {
  // "as": getElementById has no type checking. it sure would be cool to have getElementByIdOfType<T>
  const svg = document.getElementById("pt-grid") as any as SVGSVGElement | null;
  if (!svg) {
    throw new Error("Couldn't find pt-grid in screenCoordsForRect; this should be impossible");
  }
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

function defaultActions(state: M.AllStates, creatureId: T.CreatureID): Action[] {
  const targetOptions = state.grid.target_options;

  if (!targetOptions) return [];
  const { ability_id, options } = targetOptions;
  if (!("CreatureIDs" in options)) return [];
  // this is quadratic (TODO: switch options.cids to a hashmap)
  if (!options.CreatureIDs.includes(creatureId)) return [];
  const ability = state.getAbility(ability_id);
  if (!ability) return [];
  return [{
    actionName: `${ability.name} this creature`,
    action: A.executeCombatAbility,
  }];
}

export function nearby_points(pos: T.Point3, radius: number = 2000): Array<T.Point3> {
  const result = [];
  for (const x of range(pos.x - radius, pos.x + radius, 100)) {
    for (const y of range(pos.y - radius, pos.y + radius, 100)) {
      result.push(new T.Point3(x, y, pos.z));
    }
  }
  return result;
}

export function requestTeleport(cid: T.CreatureID) {
  const scene = M.getState().getFocusedScene();
  if (!scene) throw new Error("no scene");
  const scene_creature = scene.creatures.get(cid);
  if (scene_creature) {
    M.getState().displayMovementOptions(nearby_points(scene_creature[0]), cid, true);
  }
}

function pointKey(prefix: string, pt: T.Point3) {
  return `${prefix}-(${pt.x}/${pt.y}/${pt.z})`;
}
