/**
 * UI design notes.
 *
 * Things we need to do with Grids:
 * - Edit Maps (GM-only)
 *   - Toggle collision tiles
 *   - Toggle highlighted tiles
 *   - (eventually) Toggle vision-occluding tiles
 *   - Add/edit annotations on tiles
 * - View scenes
 *   - View background image for scene
 *   - View map
 *     - Collision tiles
 *     - Special tiles (w/ permission)
 *     - Annotations (w/ permission)
 *   - Objects in scene:
 *     - Creatures (w/ permission)
 *     - Other "objects" w/ tile graphics
 *     - Volume conditions
 * - Interact with scenes:
 *   - Creature actions (some GM-only, others player-only, assuming player control)
 *   - Volume Condition actions (remove)
 *   - Move creatures with long-press
 *   - Add creatures to them
 *
 * Objects we need to interact with:
 * - arbitrary open tiles (add creature here, add volume condition here)
 * - creatures
 * - volume conditions
 * - handle cases when these objects are stacked: volume condition covering area where there's a
 *   large creature and a small creature in that large creature's space
 *
 * Ways in which we want to interact with the Grid:
 * - click/touch: bring up menu for objects under point
 * - long-click / long-press: move object (and maybe enable deleting items a la iOS).
 *   - This can only affect one object, so it must be the topmost?
 *   - Maybe restricted to creatures as well.
 * - Pan (swipe), zoom (2-finger pinch)
 *
 * https://stackoverflow.com/a/38727977/4930992
 *
 */


import * as I from 'immutable';
import * as LD from "lodash";
import * as React from "react";

import {
  Button,
  // Checkbox, Dimmer, Input,
  Menu, Segment
} from 'semantic-ui-react';

import * as CV from "./CommonView";
import * as M from "./Model";
import * as T from "./PTTypes";
import * as SPZ from './SVGPanZoom';

interface Obj<T> { [index: string]: T; }

interface SceneGridProps {
  scene: T.Scene;
  creatures: Obj<MapCreature>;
}
interface SceneGridState {
  targeting_point?: { point: T.Point3; rect: M.Rect };
  affected_points?: Array<T.Point3>;
  affected_creatures?: Array<T.CreatureID>;
}
export const SceneGrid = M.connectRedux(class SceneGrid
  extends React.Component<SceneGridProps & M.ReduxProps, SceneGridState> {

  constructor(props: SceneGridProps & M.ReduxProps) {
    super(props);
    this.state = {};
  }

  render(): JSX.Element {
    const { scene, ptui, dispatch } = this.props;

    const grid = ptui.state.grid;

    const creature_menu = grid.active_objects.objects.length !== 0
      ? this.renderMenu(grid.active_objects) : null;
    const annotation = grid.display_annotation
      ? this.renderAnnotation(scene, grid.display_annotation)
      : null;

    const target_els = ptui.state.grid.target_options
      ? this.getTargetTiles(ptui.state.grid.target_options.options,
        (point, rect) => this.targetClicked(point, rect))
      : [];

    const layer = ptui.state.grid_focus && ptui.state.grid_focus.layer;
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

    const open_terrain_color = scene.background_image_url ? "transparent" : "white";
    const terrain_els = layer && layer.t === "Terrain"
      ? this.getEditableTerrain(layer.terrain)
      : scene.terrain.map(pt => tile(open_terrain_color, "base-terrain", pt));

    const highlights = layer && layer.t === "Objects" && ptui.state.grid.object_tool === "Highlight"
      ? this.getEditableHighlights(layer.highlights)
      : this.getHighlights(scene.highlights, ptui.state.player_id);
    const annotations = layer && layer.t === "Objects"
      && ptui.state.grid.object_tool === "Annotation"
      ? this.getEditableAnnotations(layer.annotations)
      : this.getAnnotations(dispatch, scene.annotations, ptui.state.player_id);

    const volumes = layer && layer.t === "Volumes"
      ? this.getEditableVolumes()
      : this.getVolumeConditions();

    const annotations_style = layer
      && ((layer.t === "Terrain")
        || (layer.t === "Objects" && ptui.state.grid.object_tool !== "Annotation"))
      ? disable_style
      : {};
    const highlights_style = layer
      && ((layer.t === "Terrain")
        || (layer.t === "Objects" && ptui.state.grid.object_tool !== "Highlight"))
      ? disable_style
      : {};
    const volumes_style = layer && layer.t !== "Volumes" ? disable_style : {};
    return <div style={{ width: "100%", height: "100%" }}>
      <div style={{
        height: '45px', display: 'flex',
        justifyContent: 'space-between', alignItems: 'center',
      }}>
        {this.topBar()}
      </div>
      {creature_menu}
      {annotation}
      <SPZ.SVGPanZoom
        id="pt-grid"
        preserveAspectRatio="xMinYMid slice"
        style={{
          width: "100%", height: "100%", backgroundColor: "rgb(215, 215, 215)",
          backgroundImage: static_background,
          backgroundRepeat: "no-repeat",
          backgroundSize: "contain",
        }}>
        {background_image}
        <g id="terrain">{terrain_els}</g>
        <g id="volume-conditions" style={volumes_style}>{volumes}</g>
        <g id="creatures" style={disable_style}>{this.getCreatures()}</g>
        <g id="highlights" style={highlights_style}>{highlights}</g>
        <g id="annotations" style={annotations_style}>{annotations}</g>
        <g id="movement-targets" style={disable_style}>{this.getMovementTargets()}</g>
        <g id="targets" style={disable_style}>{target_els}</g>
        <g id="affected" style={disable_style}>{this.getAffectedTiles()}</g>
        <g id="targeted-volume" style={disable_style}>{this.drawTargetedVolume()}</g>
      </SPZ.SVGPanZoom>
    </div>;
  }

  getMovementTargets() {
    const move = this.props.ptui.state.grid.movement_options;
    return move
      ? move.options.map(pt => <MovementTarget key={pt.toString()} cid={move.cid} pt={pt}
        teleport={move.teleport} />)
      : null;
  }

  getCreatures() {
    const creatures = LD.sortBy(this.props.creatures, c => -c.creature.size.x);
    return LD.values(creatures).map(c => {
      const highlight = LD.includes(this.state.affected_creatures, c.creature.id)
        ? "red" : undefined;
      return <GridCreature key={c.creature.id} creature={c} highlight={highlight} />;
    });
  }

  getEditableTerrain(terrain: T.Terrain) {
    const { dispatch } = this.props;
    function closeTerrain(pt: T.Point3) {
      dispatch({ type: "SetTerrain", terrain: terrain.remove(pt) });
    }
    function openTerrain(pt: T.Point3) {
      dispatch({ type: "SetTerrain", terrain: terrain.add(pt) });
    }

    const open_tiles = terrain.toArray().map(pt => {
      const tprops = tile_props("white", pt, { x: 1, y: 1 }, 0.0);
      return <rect {...tprops}
        style={{ cursor: 'pointer' }}
        onClick={() => closeTerrain(pt)}
        key={pointKey("open", pt)} />;
    });
    const closed_tiles = M.filterMap(nearby_points(new T.Point3(0, 0, 0)),
      pt => {
        if (terrain.contains(pt)) { return; }
        const tprops = tile_props("black", pt, { x: 1, y: 1 }, 0.5);
        return <rect {...tprops} style={{ cursor: 'pointer' }}
          onClick={() => openTerrain(pt)}
          key={pointKey("closed", pt)} />;
      });
    return [open_tiles, closed_tiles];
  }

  getHighlights(highlights: T.Highlights, player_id?: T.PlayerID, onClick?: (pt: T.Point3) => void) {
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
        {gmonly ? <text x={pt.x + 65} y={pt.y + 35} fontSize="25px">👁️</text> : null}
      </g>;
    });
  }

  getEditableHighlights(highlights: T.Highlights) {
    const { dispatch } = this.props;
    const color = this.props.ptui.state.grid.highlight_color;
    const vis = this.props.ptui.state.grid.object_visibility;
    function removeHighlight(pt: T.Point3) {
      dispatch({ type: "SetHighlights", highlights: highlights.remove(pt) });
    }
    function addHighlight(pt: T.Point3) {
      dispatch({
        type: "SetHighlights", highlights: highlights.set(pt, [color, vis]),
      });
    }
    const highlighted_tiles = this.getHighlights(highlights, undefined, pt => removeHighlight(pt));
    const empty_tiles = M.filterMap(nearby_points(new T.Point3(0, 0, 0)),
      pt => {
        if (highlights.has(pt)) { return; }
        const tprops = tile_props("black", pt, { x: 1, y: 1 }, 0.0);
        return <rect key={pointKey("non-high", pt)} {...tprops} onClick={() => addHighlight(pt)} />;
      });

    return [
      <g key="existing-highlights" id="existing-highlights">{highlighted_tiles}</g>,
      <g key="empty-highlights" id="empty-highlights">{empty_tiles}</g>];
  }

  getAnnotations(
    dispatch: M.Dispatch, annotations: T.Annotations, player_id?: T.PlayerID,
    specialClick?: (pt: T.Point3) => void) {
    return M.filterMap(annotations.entrySeq().toArray(),
      ([pt, [note, vis]]) => {
        if (note !== "") {
          return <Annotation key={pointKey("annotation", pt)} pt={pt} vis={vis} dispatch={dispatch}
            specialClick={specialClick}
            player_id={player_id} />;
        }
      });
  }

  getEditableAnnotations(annotations: T.Annotations) {
    const { dispatch } = this.props;
    const ann_text = this.props.ptui.state.grid.annotation_text;
    const vis = this.props.ptui.state.grid.object_visibility;
    function removeAnnotation(pt: T.Point3) {
      dispatch({ type: "SetAnnotations", annotations: annotations.remove(pt) });
    }
    function addAnnotation(pt: T.Point3) {
      if (!ann_text) { return; }
      dispatch({
        type: "SetAnnotations", annotations: annotations.set(pt, [ann_text, vis]),
      });
    }
    const annotated_tiles = this.getAnnotations(dispatch, annotations, undefined,
      pt => removeAnnotation(pt));
    const empty_tiles = M.filterMap(nearby_points(new T.Point3(0, 0, 0)),
      pt => {
        if (annotations.has(pt)) { return; }
        const tprops = tile_props("black", pt, { x: 1, y: 1 }, 0.0);
        return <rect key={pointKey("non-ann", pt)} {...tprops} onClick={() => addAnnotation(pt)} />;
      });

    return <>
      <g id="existing-annotations">{annotated_tiles}</g>,
      <g id="empty-annotations">{empty_tiles}</g>
    </>;
  }

  getEditableVolumes(): Array<JSX.Element> | undefined {
    const { dispatch } = this.props;
    return this.getVolumeConditions(0.5, (vcid, _, me) => {
      const coords: [number, number] = [me.pageX, me.pageY];
      dispatch({
        type: "ActivateGridObjects", objects: [{ t: "VolumeCondition", id: vcid }],
        coords,
      });
    }
    );
  }
  getVolumeConditions(
    fillOpacity: number = 0.1,
    onClick: (id: T.ConditionID, vc: T.VolumeCondition, evt: React.MouseEvent<any>) => void
      = () => undefined
  ): Array<JSX.Element> | undefined {
    return this.props.scene.volume_conditions.entrySeq().map(([id, vol_cond]) =>
      svgVolume(id, vol_cond.volume, vol_cond.point,
        {
          fill: "green", fillOpacity: `${fillOpacity}`, strokeOpacity: "0.5",
          style: { pointerEvents: "auto" },
          onClick: me => onClick(id, vol_cond, me),
        })).toArray();
  }

  drawTargetedVolume(): JSX.Element | undefined {
    const { scene, ptui } = this.props;
    const options = ptui.state.grid.target_options;
    if (!options) { return; }
    if (!this.state.targeting_point) { return; }
    const target = this.state.targeting_point.point;
    const ability_id = options.ability_id;
    const ability = ptui.getAbility(ability_id);
    if (!ability) { return; }
    const action = ability.action;

    switch (action.t) {
      case "Creature":
        switch (action.target.t) {
          case "AllCreaturesInVolumeInRange":
            return svgVolume("target-volume", action.target.volume, target);
          case "LineFromActor":
            const caster_pos = M.getCreaturePos(scene, options.cid);
            if (!caster_pos) { return; }
            return <line x1={caster_pos.x + 50} y1={caster_pos.y}
              x2={target.x + 50} y2={target.y}
              style={{ pointerEvents: "none" }}
              strokeWidth="3" stroke="black" />;
        }
        return;
      case "SceneVolume":
        switch (action.target.t) {
          case "RangedVolume":
            return svgVolume("target-volume", action.target.volume, target);
        }
    }
  }

  targetClicked(point: T.Point3, rect: M.Rect) {
    const { ptui, dispatch } = this.props;
    const options = ptui.state.grid.target_options!;
    const ability = ptui.getAbility(options.ability_id);
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
    this.setState({ targeting_point: { point, rect } });
    M.fetchAbilityTargets(dispatch, ptui.rpi_url, this.props.scene.id, options.cid,
      options.ability_id, point).then(
        ({ points, creatures }) =>
          this.setState({ affected_points: points, affected_creatures: creatures }));
  }

  renderAnnotation(scene: T.Scene, { pt, rect }: { pt: T.Point3; rect: M.Rect })
    : JSX.Element | null {
    const { dispatch } = this.props;
    const ann = scene.annotations.get(pt);
    if (!ann) { return null; }
    return <RectPositioned coords={[rect.sw.x, rect.sw.y]}
      onClose={() => dispatch({ type: "ToggleAnnotation", pt })}>
      <Segment>{ann[0]}</Segment>
    </RectPositioned>;
  }

  renderMenu(arg: { objects: Array<M.GridObject>; coords: [number, number] }): JSX.Element | null {
    const { objects, coords } = arg;
    const { scene, creatures, dispatch } = this.props;
    const close = () => dispatch({ type: 'ClearActiveGridObjects' });
    return <RectPositioned coords={coords}
      onClose={close}>
      <Menu vertical={true}>
        {objects.map(obj => {
          switch (obj.t) {
            case "Creature":
              const creature = M.get(creatures, obj.id);
              if (creature) {
                return [
                  <Menu.Item key={creature.creature.id} header={true}>
                    <CV.ClassIcon class_id={creature.creature.class_} /> {creature.creature.name}
                  </Menu.Item>,
                ].concat(creature.actions.entrySeq().toArray().map(
                  ([actionName, action]) => {
                    function onClick() {
                      close();
                      action(obj.id);
                    }
                    return <Menu.Item key={actionName} onClick={() => onClick()}>
                      {actionName}
                    </Menu.Item>;
                  }));
              }
              return;
            case "VolumeCondition":
              const onClick = () => {
                dispatch(M.sendCommand(
                  { t: "RemoveSceneVolumeCondition", scene_id: scene.id, condition_id: obj.id }));
                close();
              };
              return [
                // unimplemented!: put a name here
                <Menu.Item key="Header" header={true}>Condition</Menu.Item>,
                <Menu.Item key="Remove VC" onClick={() => onClick()}>Remove</Menu.Item>];
          }
        }
        )}
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

  topBar(): JSX.Element {
    const { ptui, dispatch } = this.props;
    if (this.state.targeting_point) {
      return <div>Proceed with action?
        <Button onClick={() => this.executePointTargetedAbility()}>Act</Button>
        <Button onClick={() => this.clearTargets()}>Cancel</Button>
      </div>;
    }

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
  }

  clearTargets() {
    this.setState({
      affected_points: undefined, affected_creatures: undefined, targeting_point: undefined,
    });
  }

  executePointTargetedAbility() {
    if (!this.state.targeting_point) { return; }
    this.props.ptui.executeCombatPointTargetedAbility(
      this.props.dispatch, this.state.targeting_point.point);
    this.clearTargets();
  }
});

function svgVolume(
  key: string, volume: T.Volume, pt: T.Point3, props?: React.SVGProps<SVGGraphicsElement>
): JSX.Element {
  switch (volume.t) {
    case "Sphere":
      return <circle key={key} cx={pt.x + 50} cy={pt.y} r={volume.radius}
        style={{ pointerEvents: "none" }}
        strokeWidth={3} stroke="black" fill="none" {...props} />;
    default:
      console.log("unimplemented! svgvolume for", volume);
      return <g key={key} />;
  }
}


interface RectPositionedProps {
  coords: [number, number];
  onClose: () => void;
  children: React.ReactChild;
}
function RectPositioned(props: RectPositionedProps): JSX.Element {
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

const MovementTarget = M.connectRedux(
  function MovementTarget(
    props: { cid?: T.CreatureID; pt: T.Point3; teleport: boolean } & M.ReduxProps
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

interface AnnotationProps {
  pt: T.Point3;
  vis: T.Visibility;
  player_id?: T.PlayerID;
  specialClick?: (pt: T.Point3) => void;
}
function Annotation(props: AnnotationProps & M.DispatchProps): JSX.Element | null {
  const { dispatch, pt, vis, player_id, specialClick } = props;
  if (M.isEqual(vis, { t: "GMOnly" }) && player_id) {
    return null;
  }

  let element: SVGRectElement;

  function onClick() {
    if (specialClick) {
      specialClick(pt);
    } else {
      dispatch({ type: "ToggleAnnotation", pt, rect: screenCoordsForRect(element) });
    }
  }

  return <g>
    <rect width="100" height="100" x={pt.x} y={pt.y - 50} fillOpacity="0"
      ref={el => { if (el !== null) { element = el; } }} onClick={onClick}
    />
    <text
      style={{ pointerEvents: "none" }}
      x={pt.x + 25} y={pt.y + 50}
      fontSize="100px" stroke="black" strokeWidth="2px" fill="white">*</text>
  </g>;
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
    if (el.style.pointerEvents !== 'none') {
      dirtied.push({ el, oldPE: el.style.pointerEvents });
      el.style.pointerEvents = 'none';
    }
  }
  for (const { el, oldPE } of dirtied) {
    el.style.pointerEvents = oldPE;
  }
  return results;
}


const GridCreature = M.connectRedux(
  function GridCreature({ ptui, dispatch, creature, highlight }:
    { creature: MapCreature; highlight?: string } & M.ReduxProps): JSX.Element {
    let timer: number;
    let element: SVGRectElement | SVGImageElement;
    function onClick(event: React.MouseEvent<any>) {
      const creatures = findElementsAtPoint(event.pageX, event.pageY,
        el => el.getAttribute('data-pt-type') === 'creature'
          && (el.getAttribute('data-pt-id') || undefined)
          || undefined,
        'svg');
      const rect = screenCoordsForRect(element);
      const act: M.Action = {
        type: "ActivateGridObjects",
        objects: creatures.map((id): M.GridObject => ({ t: "Creature", id })),
        coords: [rect.sw.x, rect.sw.y],
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
    if (highlight) {
      highlightProps.stroke = highlight;
      highlightProps.strokeWidth = 15;
    }

    const opacity = (creature.visibility.t === "GMOnly") ? "0.4" : "1.0";
    const reflection_props = { 'data-pt-type': "creature", 'data-pt-id': creature.creature.id };
    return <g key={creature.creature.id} opacity={opacity} onClick={e => onClick(e)}
      onMouseUp={() => clearTimeout(timer)}
      onMouseDown={() => {
        timer = window.setTimeout(() => console.log("LONG PRESS!", creature.creature.name), 500);
      }}>
      {contents()}
    </g>;
    function contents() {
      if (creature.creature.icon_url !== "") {
        const props = tile_props("white", creature.pos, creature.creature.size);
        const bare_props = bare_tile_props(creature.pos, creature.creature.size);
        return [
          <image key="image" ref={el => { if (el !== null) { element = el; } }}
            xlinkHref={creature.creature.icon_url} {...props} />,
          <rect key="rect" {...bare_props} {...reflection_props} {...highlightProps}
            fillOpacity="0" />
        ];
      } else {
        const props = tile_props(creature.class_.color, creature.pos, creature.creature.size);
        return [
          <rect key="rect" ref={el => { if (el !== null) { element = el; } }} {...props}
            {...reflection_props}
            {...highlightProps} />,
          text_tile(creature.creature.name.slice(0, 4), creature.pos, "text")
        ];
      }
    }
  });


function text_tile(text: string, pos: T.Point3, key?: string): JSX.Element {
  return <text key={key} style={{ pointerEvents: "none" }} fontSize="50"
    x={pos.x} y={pos.y}>
    {text}
  </text>;
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
    x: pt.x, y: pt.y - 50,
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

/**
 * Create the `MapCreature`s for all creatures in a scene. This is common code shared for player
 * and GM views.
 */
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
    creature: T.Creature): { name: string; action: ((cid: T.CreatureID) => void) } | undefined {
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
  for (const x of LD.range(pos.x - 2000, pos.x + 2000, 100)) {
    for (const y of LD.range(pos.y - 2000, pos.y + 2000, 100)) {
      result.push(new T.Point3(x, y, pos.z));
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
  return `${prefix}-(${pt.x}/${pt.y}/${pt.z})`;
}
