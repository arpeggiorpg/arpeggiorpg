import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from "react";
import * as ReactDOM from "react-dom";

import { Button, Checkbox, List } from 'semantic-ui-react';
import * as SUI from 'semantic-ui-react';

import * as CV from './CommonView';
import * as GM from './GMComponents';
import * as M from './Model';
import * as T from './PTTypes';
import * as TextInput from './TextInput';


type FolderContentType = "scene" | "map" | "creature" | "note" | "item" | "folder";

class CampaignComp extends React.Component<M.ReduxProps, undefined> {
  shouldComponentUpdate(newProps: M.ReduxProps) {
    return newProps.ptui.app.current_game.campaign !== this.props.ptui.app.current_game.campaign;
  }
  render(): JSX.Element {
    const { ptui, dispatch } = this.props;
    console.log("[EXPENSIVE:Campaign.render]");
    return <FolderTree
      name="Campaign" path={[]} folder={ptui.app.current_game.campaign} start_open={true} />;
  }
}
export const Campaign = M.connectRedux(CampaignComp);


interface MultiCreatureSelectorProps {
  already_selected: I.Set<T.CreatureID>;
  on_selected: (cs: I.Set<T.CreatureID>) => void;
}
class MultiCreatureSelectorComp
  extends React.Component<
  MultiCreatureSelectorProps & M.ReduxProps,
  { selections: I.Set<T.CreatureID> }> {
  constructor(props: MultiCreatureSelectorProps & M.ReduxProps) {
    super(props);
    this.state = { selections: this.props.already_selected };
  }
  render(): JSX.Element {
    const { ptui } = this.props;
    const self = this;
    function on_check(checked: boolean, path: T.FolderPath, folder_item: T.FolderItemID) {
      switch (folder_item.t) {
        case "CreatureID":
          const new_selected = checked
            ? self.state.selections.add(folder_item.id)
            : self.state.selections.remove(folder_item.id);
          self.setState({ selections: new_selected });
          return;
        default:
          console.log("Got a non-creature selection in a creature-only campaign selector:",
            folder_item);
      }
    }
    const stype: FolderContentType = 'creature';
    const selecting: SelectableProps = {
      item_type: stype, allow_multiple: true, on_select_object: on_check,
      is_selected: (path, item_id) =>
        item_id.t === "CreatureID" && this.state.selections.includes(item_id.id),
    };
    return <div>
      <FolderTree name="Campaign" path={[]} folder={ptui.app.current_game.campaign} start_open={true}
        selecting={selecting} />
      <Button onClick={() => this.props.on_selected(this.state.selections)}>Select Creatures</Button>
    </div>;
  }
}
export const MultiCreatureSelector = M.connectRedux(MultiCreatureSelectorComp);


interface SelectableProps {
  item_type: FolderContentType;
  allow_multiple: boolean;
  is_selected: (path: T.FolderPath, item_id: T.FolderItemID) => boolean;

  on_select_object?: (select: boolean, path: T.FolderPath, item: T.FolderItemID) => void;
  on_select_folder?: (select: boolean, path: T.FolderPath) => void;
}

type FolderObject =
  | { t: "Scene"; path: T.FolderPath; scene: T.Scene }
  | { t: "Map"; path: T.FolderPath; map: T.Map }
  | { t: "Creature"; path: T.FolderPath; creature: T.Creature }
  | { t: "Note"; path: T.FolderPath; name: string }
  | { t: "Item"; path: T.FolderPath; item: T.Item }
  ;

interface FTProps {
  path: T.FolderPath;
  name: string;
  folder: T.Folder;
  start_open?: boolean;
  selecting?: SelectableProps;
}
class FolderTreeComp extends React.Component<FTProps & M.ReduxProps,
  { expanded: boolean }> {
  constructor(props: FTProps & M.ReduxProps) {
    super(props);
    this.state = { expanded: props.start_open || false };
  }
  render(): JSX.Element {
    const { folder, ptui, selecting, path } = this.props;
    function dont_show(t: FolderContentType) {
      return selecting && selecting.item_type !== t;
    }

    const scene_objects = dont_show("scene") ? [] :
      ptui.getScenes(folder.data.scenes).map((scene): FolderObject =>
        ({ t: "Scene", path, scene }));
    const map_objects = dont_show("map") ? [] :
      ptui.getMaps(folder.data.maps).map((map): FolderObject =>
        ({ t: "Map", path, map }));
    const creature_objects = dont_show("creature") ? [] :
      ptui.getCreatures(folder.data.creatures).map((creature): FolderObject =>
        ({ t: "Creature", path, creature }));
    const note_objects = dont_show("note") ? [] :
      LD.keys(folder.data.notes).map((name): FolderObject =>
        ({ t: "Note", path, name }));
    const item_objects = dont_show("item") ? [] :
      ptui.getItems(folder.data.items).map((item): FolderObject =>
        ({ t: "Item", path, item }));
    const objects = LD.concat(
      scene_objects, map_objects, creature_objects, note_objects, item_objects);
    const children = objects.map(obj => {
      const iid = object_to_item_id(obj);
      return <TreeObject key={`${iid.t}/${iid.id}`} object={obj} selecting={selecting} />;
    }
    );

    const subfolders = LD.sortBy(LD.toPairs(folder.children), ([name, _]) => name).map(
      ([name, subfolder]) =>
        <FolderTree key={name} name={name} folder={subfolder}
          path={LD.concat(this.props.path, name)} selecting={selecting} />
    );
    const display = this.state.expanded ? "block" : "none";
    const toggle = () => this.setState({ expanded: !this.state.expanded });
    const list_item = <List.Item>
      {this.state.expanded
        ? <List.Icon name='folder open' />
        : <List.Icon name='folder' />}
      <List.Content>
        <List.Header style={{ cursor: "pointer" }} content={this.props.name}
          onClick={() => this.setState({ expanded: !this.state.expanded })} />
        {this.state.expanded
          ? <List.List> {children} {subfolders} </List.List>
          : null}
      </List.Content>
    </List.Item>;

    if (M.isEqual(path, [])) {
      return <List size="large">{list_item}</List>;
    } else {
      return list_item;
    }
  }
}
const FolderTree = M.connectRedux(FolderTreeComp);


function object_icon(obj: FolderObject): string {
  switch (obj.t) {
    case "Scene": return "game";
    case "Map": return "map";
    case "Creature": return "user";
    case "Note": return "write";
    case "Item": return "object group";
  }
}
function object_name(obj: FolderObject): string {
  switch (obj.t) {
    case "Scene": return obj.scene.name;
    case "Map": return obj.map.name;
    case "Creature": return obj.creature.name;
    case "Note": return obj.name;
    case "Item": return obj.item.name;
  }
}

function object_to_item_id(obj: FolderObject): T.FolderItemID {
  switch (obj.t) {
    case "Scene": return { t: "SceneID", id: obj.scene.id };
    case "Map": return { t: "MapID", id: obj.map.id };
    case "Creature": return { t: "CreatureID", id: obj.creature.id };
    case "Note": return { t: "NoteID", id: obj.name };
    case "Item": return { t: "ItemID", id: obj.item.id };
  }
}

function activate_object(obj: FolderObject, dispatch: M.Dispatch): void {
  switch (obj.t) {
    case "Scene":
      return dispatch({ type: "FocusGrid", focus: { t: "Scene", scene_id: obj.scene.id } });
    case "Map":
      return dispatch({ type: "FocusGrid", focus: { t: "Map", map_id: obj.map.id } });
    case "Creature":
      return dispatch({
        type: "FocusSecondary",
        focus: { t: "Creature", creature_id: obj.creature.id },
      });
    case "Note":
      return dispatch({
        type: "FocusSecondary",
        focus: { t: "Note", path: obj.path, name: obj.name },
      });
    case "Item": return;
  }
}


interface TreeObjectProps {
  object: FolderObject;
  selecting: SelectableProps | undefined;
}
const TreeObject = M.connectRedux(
  function TreeObject({ object, selecting, dispatch }: TreeObjectProps & M.ReduxProps) {
    function handler() {
      if (selecting) { return; }
      return activate_object(object, dispatch);
    }
    const name = object_name(object);
    function onCheck(evt: any, data: SUI.CheckboxProps) {
      if (selecting && selecting.on_select_object && data.checked !== undefined) {
        return selecting.on_select_object(data.checked, object.path, object_to_item_id(object));
      }
    }
    return <List.Item style={{ cursor: 'pointer' }} onClick={handler}>
      <List.Icon name={object_icon(object)} />
      <List.Content>
        {
          selecting
            ? <Checkbox checked={selecting.is_selected(object.path, object_to_item_id(object))}
              label={name} onChange={onCheck} />
            : name
        }
      </List.Content>
    </List.Item>;
  });
