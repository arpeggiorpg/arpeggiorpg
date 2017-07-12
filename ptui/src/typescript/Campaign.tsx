import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from "react";

import { Button, Checkbox, Dropdown, List } from 'semantic-ui-react';
import * as SUI from 'semantic-ui-react';

import * as CV from './CommonView';
import * as Comp from './Component';
import * as GM from './GMComponents';
import * as M from './Model';
import * as T from './PTTypes';

export const Campaign = Comp.connect(
  ptui => ({ campaign: ptui.app.current_game.campaign })
)(
  function campaignComp(props: { campaign: T.Folder; dispatch: M.Dispatch }): JSX.Element {
    const { campaign } = props;
    return <FolderTree name="Campaign" path={[]} folder={campaign} start_open={true} />;
  });

interface MultiItemSelectorProps {
  require_selected: I.Set<T.ItemID>;
  on_selected: (cs: I.Set<T.ItemID>) => void;
  on_cancel: () => void;
}
class MultiItemSelectorComp
  extends React.Component<
  MultiItemSelectorProps & M.ReduxProps,
  { selections: I.Set<T.ItemID> }> {
  constructor(props: MultiItemSelectorProps & M.ReduxProps) {
    super(props);
    this.state = { selections: this.props.require_selected };
  }
  render(): JSX.Element {
    const { ptui } = this.props;
    const self = this;
    function on_check(checked: boolean, _: T.FolderPath, folder_item: T.FolderItemID) {
      switch (folder_item.t) {
        case "ItemID":
          if (self.props.require_selected.includes(folder_item.id)) {
            return;
          }
          const new_selected = checked
            ? self.state.selections.add(folder_item.id)
            : self.state.selections.remove(folder_item.id);
          self.setState({ selections: new_selected });
          return;
        default:
          console.log("Got a non-item selection in a item-only campaign selector:",
            folder_item);
      }
    }
    const selecting: SelectableProps = {
      item_type: 'Item', allow_multiple: true, on_select_object: on_check,
      is_selected: (_, item_id) =>
        item_id.t === "ItemID" && this.state.selections.includes(item_id.id),
    };
    return <div>
      <FolderTree name="Campaign" path={[]} folder={ptui.app.current_game.campaign} start_open={true}
        selecting={selecting} />
      <Button onClick={() => this.props.on_selected(this.state.selections)}>Select Items</Button>
      <Button onClick={this.props.on_cancel}>Cancel</Button>
    </div>;
  }
}
export const MultiItemSelector = M.connectRedux(MultiItemSelectorComp);


interface MultiCreatureSelectorProps {
  already_selected: I.Set<T.CreatureID>;
  on_selected: (cs: I.Set<T.CreatureID>) => void;
  on_cancel: () => void;
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
    function on_check(checked: boolean, _: T.FolderPath, folder_item: T.FolderItemID) {
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
    const selecting: SelectableProps = {
      item_type: 'Creature', allow_multiple: true, on_select_object: on_check,
      is_selected: (_, item_id) =>
        item_id.t === "CreatureID" && this.state.selections.includes(item_id.id),
    };
    return <div>
      <FolderTree name="Campaign" path={[]} folder={ptui.app.current_game.campaign} start_open={true}
        selecting={selecting} />
      <Button onClick={() => this.props.on_selected(this.state.selections)}>Select Creatures</Button>
      <Button onClick={this.props.on_cancel}>Cancel</Button>
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

type FolderContentType = "Scene" | "Map" | "Creature" | "Note" | "Item" | "Folder";

type FolderObject =
  | { t: "Scene"; path: T.FolderPath; id: T.SceneID; name: string }
  | { t: "Map"; path: T.FolderPath; id: T.MapID; name: string }
  | { t: "Creature"; path: T.FolderPath; id: T.CreatureID; name: string }
  | { t: "Note"; path: T.FolderPath; name: string }
  | { t: "Item"; path: T.FolderPath; id: T.ItemID; name: string }
  ;

interface FTProps {
  path: T.FolderPath;
  name: string;
  folder: T.Folder;
  start_open?: boolean;
  selecting?: SelectableProps;
}
interface FTDerivedProps {
  objects: Array<FolderObject>;
}

class FolderTreeComp
  extends Comp.Component<
  FTProps & FTDerivedProps & M.DispatchProps,
  { expanded: boolean }
  > {

  constructor(props: FTProps & FTDerivedProps & M.DispatchProps) {
    super(props);
    this.state = { expanded: props.start_open || false };
  }
  render(): JSX.Element {
    console.log("[EXPENSIVE:FolderTree]", this.props.path);
    const { folder, selecting, path, objects, dispatch } = this.props;

    const children = objects.map(obj => {
      const iid = object_to_item_id(obj);
      return <TreeObject key={`${iid.t}/${iid.id}`} object={obj} selecting={selecting}
        dispatch={dispatch} />;
    });

    const subfolders = LD.sortBy(LD.toPairs(folder.children), ([name, _]) => name).map(
      ([name, subfolder]) =>
        <FolderTree key={name} name={name} folder={subfolder}
          path={LD.concat(this.props.path, name)} selecting={selecting} />
    );
    const folder_menu = <Dropdown icon='ellipsis horizontal'>
      <Dropdown.Menu>
        <Dropdown.Header content={M.folderPathToString(path)} />
        <Dropdown.Item icon={object_icon("Scene")} text='Create Scene' />
        <Dropdown.Item icon={object_icon("Map")} text='Create Map' />
        <CV.ModalMaker
          button={open =>
            <Dropdown.Item icon={object_icon("Creature")} onClick={open} text='Create Creature' />}
          header={<span>Create new creature in {M.folderPathToString(path)}</span>}
          content={close => <GM.CreateCreature path={path} onClose={close} />}
        />
        <Dropdown.Item icon={object_icon("Note")} text='Create Note'
          onClick={() =>
            dispatch({ type: "FocusSecondary", focus: { t: "Note", path, name: undefined } })} />
        <CV.ModalMaker
          button={toggler =>
            <Dropdown.Item icon={object_icon("Item")} text='Create Item' onClick={toggler} />}
          header={<span>Create item in {M.folderPathToString(path)}</span>}
          content={toggler => <GM.GMCreateItem path={path} onClose={toggler} />} />
        <CV.ModalMaker
          button={open => <Dropdown.Item icon={object_icon("Folder")} text='Create Folder'
            onClick={open} />}
          header={<span>Create a folder in {M.folderPathToString(path)}</span>}
          content={close => <GM.CreateFolder path={path} onDone={close} />}
        />
        {!M.isEqual(path, [])
          ? [
            <Dropdown.Divider key="ble" />,
            <Dropdown.Item key="blo" text="Delete this folder" icon="delete"
              onClick={() => dispatch(M.sendCommand({ t: "DeleteFolder", path }))} />]
          : null}
      </Dropdown.Menu>
    </Dropdown>;
    const list_item = <List.Item>
      <List.Icon name={this.state.expanded ? 'folder open' : 'folder'} />
      <List.Content>
        <List.Header style={{ cursor: "pointer" }}
          onClick={() => this.setState({ expanded: !this.state.expanded })}>
          {this.props.name}
          {this.state.expanded ? folder_menu : null}
        </List.Header>
        {this.state.expanded
          ? <List.List>
            {children} {subfolders}
          </List.List>
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

const FolderTree = Comp.connect<FTProps, FTDerivedProps>(
  (ptui: M.PTUI, props: FTProps) => {

    const { selecting, folder, path } = props;

    function dont_show(t: FolderContentType) {
      return selecting && selecting.item_type !== t;
    }

    const scene_objects = dont_show("Scene") ? [] :
      ptui.getScenes(folder.data.scenes).map(
        (scene): FolderObject => ({ t: "Scene", path, id: scene.id, name: scene.name }));
    const map_objects = dont_show("Map") ? [] :
      ptui.getMaps(folder.data.maps).map(
        (map): FolderObject => ({ t: "Map", path, id: map.id, name: map.name }));
    const creature_objects = dont_show("Creature") ? [] :
      ptui.getCreatures(folder.data.creatures).map(
        (creature): FolderObject => ({ t: "Creature", path, id: creature.id, name: creature.name }));
    const note_objects = dont_show("Note") ? [] :
      LD.sortBy(LD.keys(folder.data.notes), n => n).map(
        (name): FolderObject => ({ t: "Note", path, name }));
    const item_objects = dont_show("Item") ? [] :
      ptui.getItems(folder.data.items).map(
        (item): FolderObject => ({ t: "Item", path, id: item.id, name: item.name }));
    return {
      objects: LD.concat(
        scene_objects, map_objects, creature_objects, note_objects, item_objects),
    };
  })(FolderTreeComp);


function object_icon(name: FolderContentType): string {
  switch (name) {
    case "Scene": return "object group";
    case "Map": return "map";
    case "Creature": return "user";
    case "Note": return "comment";
    case "Item": return "shopping bag";
    case "Folder": return "folder";
  }
}

function object_to_item_id(obj: FolderObject): T.FolderItemID {
  switch (obj.t) {
    case "Note": return { t: "NoteID", id: obj.name };
    case "Scene": return { t: "SceneID", id: obj.id };
    case "Map": return { t: "MapID", id: obj.id };
    case "Creature": return { t: "CreatureID", id: obj.id };
    case "Item": return { t: "ItemID", id: obj.id };
  }
}

function activate_object(obj: FolderObject, dispatch: M.Dispatch): void {
  switch (obj.t) {
    case "Scene":
      dispatch({ type: "FocusGrid", focus: { t: "Scene", scene_id: obj.id } }); return;
    case "Map":
      dispatch({ type: "FocusGrid", focus: { t: "Map", map_id: obj.id } }); return;
    case "Creature":
      dispatch({
        type: "FocusSecondary",
        focus: { t: "Creature", creature_id: obj.id },
      });
      return;
    case "Note":
      dispatch({
        type: "FocusSecondary",
        focus: { t: "Note", path: obj.path, name: obj.name },
      });
      return;
    case "Item":
      dispatch(
        { type: "FocusSecondary", focus: { t: "Item", path: obj.path, item_id: obj.id } });
      return;
  }
}


interface TreeObjectProps extends M.DispatchProps {
  object: FolderObject;
  selecting: SelectableProps | undefined;
}

function TreeObject({ object, selecting, dispatch }: TreeObjectProps) {
  console.log('[EXPENSIVE:TreeObject]', object);
  function handler() {
    if (selecting) { return; }
    return activate_object(object, dispatch);
  }
  const name = object.name;
  function onCheck(_: any, data: SUI.CheckboxProps) {
    if (selecting && selecting.on_select_object && data.checked !== undefined) {
      return selecting.on_select_object(data.checked, object.path, object_to_item_id(object));
    }
  }
  return <List.Item>
    <List.Icon name={object_icon(object.t)} />
    <List.Content style={{ cursor: 'pointer' }} onClick={handler}>
      {
        selecting
          ? <Checkbox checked={selecting.is_selected(object.path, object_to_item_id(object))}
            label={name} onChange={onCheck} />
          : name
      }
    </List.Content>
  </List.Item>;
}
