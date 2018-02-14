import * as Fuse from 'fuse.js';
import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from "react";

import {
  Button, Checkbox, Divider, Dropdown, Icon, Input, Label, List, Menu,
} from 'semantic-ui-react';

import * as SUI from 'semantic-ui-react';

import * as CV from './CommonView';
import * as Comp from './Component';
import * as CF from './CoolForm';
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
export const MultiItemSelector = M.connectRedux(class MultiItemSelector
  extends React.Component<MultiItemSelectorProps & M.ReduxProps, { selections: I.Set<T.ItemID> }> {
  constructor(props: MultiItemSelectorProps & M.ReduxProps) {
    super(props);
    this.state = { selections: this.props.require_selected };
  }
  render(): JSX.Element {
    const { ptui } = this.props;
    const items = collectAllItems(ptui, [], ptui.app.current_game.campaign);
    const display = ([path, item]: [T.FolderPath, T.Item]) =>
      `${M.folderPathToString(path)}/${item.name}`;
    return <div>
      {ptui.getItems(this.state.selections.toArray()).map(
        item => <Label key={item.id}>{item.name}</Label>)}
      <SearchSelect values={items}
        onSelect={([_, item]: [T.FolderPath, T.Item]) =>
          this.setState({ selections: this.state.selections.add(item.id) })}
        display={display}
      />
      <Button onClick={() => this.props.on_selected(this.state.selections)}>Select Items</Button>
      <Button onClick={this.props.on_cancel}>Cancel</Button>
    </div>;
  }
});

interface MultiCreatureSelectorProps {
  already_selected: I.Set<T.CreatureID>;
  on_selected: (cs: I.Set<T.CreatureID>) => void;
  on_cancel: () => void;
}
export const MultiCreatureSelector = M.connectRedux(class MultiCreatureSelector
  extends React.Component<
  MultiCreatureSelectorProps & M.ReduxProps,
  { selections: I.Set<T.CreatureID> }> {
  constructor(props: MultiCreatureSelectorProps & M.ReduxProps) {
    super(props);
    this.state = { selections: this.props.already_selected };
  }
  render(): JSX.Element {
    const { ptui } = this.props;
    const on_check = (checked: boolean, _: T.FolderPath, folder_item: T.FolderItemID) => {
      switch (folder_item.t) {
        case "CreatureID":
          const new_selected = checked
            ? this.state.selections.add(folder_item.id)
            : this.state.selections.remove(folder_item.id);
          this.setState({ selections: new_selected });
          return;
        default:
          console.log("Got a non-creature selection in a creature-only campaign selector:",
            folder_item);
      }
    };
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
});

interface SelectableProps {
  item_type: FolderContentType;
  allow_multiple: boolean;
  is_selected: (path: T.FolderPath, item_id: T.FolderItemID) => boolean;

  on_select_object?: (select: boolean, path: T.FolderPath, item: T.FolderItemID) => void;
  on_select_folder?: (select: boolean, path: T.FolderPath) => void;
}

type FolderContentType = "Scene" | "Creature" | "Note" | "Item" | "Ability" | "Class" | "Folder";

type FolderObject =
  | { t: "Scene"; path: T.FolderPath; id: T.SceneID; name: string }
  | { t: "Creature"; path: T.FolderPath; id: T.CreatureID; name: string }
  | { t: "Note"; path: T.FolderPath; name: string }
  | { t: "Item"; path: T.FolderPath; id: T.ItemID; name: string }
  | { t: "Ability"; path: T.FolderPath; id: T.AbilityID; name: string }
  | { t: "Class"; path: T.FolderPath; id: T.ClassID; name: string }
  ;

interface FTProps {
  path: T.FolderPath;
  name: string;
  folder: T.Folder;
  start_open?: boolean;
  selecting?: SelectableProps;
}
interface FTDerivedProps {
  class_objects: Array<FolderObject>;
  ability_objects: Array<FolderObject>;
  note_objects: Array<FolderObject>;
  scene_objects: Array<FolderObject>;
  creature_objects: Array<FolderObject>;
  item_objects: Array<FolderObject>;
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
    const { folder, selecting, path, dispatch } = this.props;

    const children = [
      section("Notes", this.props.note_objects),
      section("Classes", this.props.class_objects),
      section("Abilities", this.props.ability_objects),
      section("Scenes", this.props.scene_objects),
      section("Creatures", this.props.creature_objects),
      section("Items", this.props.item_objects),
    ];
    function divider(name: string) {
      return <Divider horizontal={true} fitted={true} key={`${name}-div`}
        style={{
          fontSize: "70%", color: "gray", marginBottom: "4px",
          marginTop: "4px", width: '200px',
        }}
      >{name}</Divider>;
    }
    function section(name: string, objects: Array<FolderObject>) {
      if (objects.length === 0) { return []; }
      return [divider(name)].concat(
        objects.map(obj => {
          const iid = object_to_item_id(obj);
          return <TreeObject key={`${iid.t}/${iid.id}`} object={obj} selecting={selecting}
            dispatch={dispatch} />;
        }));
    }

    const subfolders = LD.sortBy(folder.children.entrySeq().toArray(), ([name, _]) => name).map(
      ([name, subfolder]) =>
        <FolderTree key={name} name={name} folder={subfolder}
          path={LD.concat(this.props.path, name)} selecting={selecting} />
    );
    const folder_menu = <Dropdown icon='ellipsis horizontal'>
      <Dropdown.Menu>
        <Dropdown.Header content={M.folderPathToString(path)} />
        <CV.ModalMaker
          button={open =>
            <Dropdown.Item icon={object_icon("Scene")} text='Create Scene' onClick={open} />}
          header={<span>Create new scene in {M.folderPathToString(path)}</span>}
          content={close => <GM.CreateScene path={path} onDone={close} />} />
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
            <Dropdown.Divider key="bla" />,
            <Dropdown.Item key="ble" text="Delete this folder" icon="delete"
              onClick={() => dispatch(M.sendCommand(
                {
                  t: "DeleteFolderItem", location: LD.slice(path, 0, -1),
                  // ! because we KNOW this isn't [] (see conditional above)
                  item_id: { t: "SubfolderID", id: LD.nth(path, -1)! },
                }))} />,
            <CV.ModalMaker key="bli"
              button={open => <Dropdown.Item text="Move this folder" icon="font" onClick={open} />}
              header={<span>Rename {M.folderPathToString(path)}</span>}
              content={close => <MoveFolderItem
                source={LD.slice(path, 0, -1)} item_id={{ t: "SubfolderID", id: LD.nth(path, -1)! }}
                onDone={close} dispatch={dispatch} />}
            />,
            <Dropdown.Divider key="blo" />,
            <CV.ModalMaker key="blu"
              button={open =>
                <Dropdown.Item key="blub" text="Export as module" icon="upload" onClick={open} />}
              header={<span>Export folder</span>}
              content={close => <GM.ExportModule path={path} onDone={close} />} />,
            <CV.ModalMaker key="bly"
              button={open =>
                <Dropdown.Item text="Import module here" icon="download" onClick={open} />}
              header={<span>Import Module</span>}
              content={close => <GM.ImportModule path={path} onDone={close} />}
            />
          ]
          : null}
      </Dropdown.Menu>
    </Dropdown>;
    const list_item = <List.Item>
      <List.Icon name={this.state.expanded ? 'folder open' : 'folder'} />
      <List.Content style={{ width: '100%' }}>
        <List.Header style={{ cursor: "pointer" }}
          onClick={() => this.setState({ expanded: !this.state.expanded })}>
          {this.props.name}
          {this.state.expanded ? folder_menu : null}
        </List.Header>
        {this.state.expanded
          ? <List.List>
            {children}
            {subfolders.length > 0 ? divider("Folders") : null}
            {subfolders}
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
    const creature_objects = dont_show("Creature") ? [] :
      ptui.getCreatures(folder.data.creatures).map(
        (creature): FolderObject => ({ t: "Creature", path, id: creature.id, name: creature.name }));
    const note_objects = dont_show("Note") ? [] :
      LD.sortBy(LD.keys(folder.data.notes), n => n).map(
        (name): FolderObject => ({ t: "Note", path, name }));
    const item_objects = dont_show("Item") ? [] :
      ptui.getItems(folder.data.items).map(
        (item): FolderObject => ({ t: "Item", path, id: item.id, name: item.name }));
    const ability_objects = dont_show("Ability") ? [] : ptui.getAbilities(folder.data.abilities).map(
      (item): FolderObject => ({ t: "Ability", path, id: item.id, name: item.name }));
    const class_objects = dont_show("Class") ? [] : ptui.getClasses(folder.data.classes).map(
      (item): FolderObject => ({ t: "Class", path, id: item.id, name: item.name }));

    return {
      note_objects,
      class_objects,
      ability_objects,
      scene_objects,
      creature_objects,
      item_objects,
    };
  })(FolderTreeComp);


function object_icon(name: FolderContentType): SUI.SemanticICONS {
  switch (name) {
    case "Scene": return "object group";
    case "Creature": return "user";
    case "Note": return "comment";
    case "Item": return "shopping bag";
    case "Folder": return "folder";
    case "Ability": return "play circle";
    case "Class": return "users";
  }
}

function object_to_item_id(obj: FolderObject): T.FolderItemID {
  switch (obj.t) {
    case "Note": return { t: "NoteID", id: obj.name };
    case "Scene": return { t: "SceneID", id: obj.id };
    case "Creature": return { t: "CreatureID", id: obj.id };
    case "Item": return { t: "ItemID", id: obj.id };
    case "Ability": return { t: "AbilityID", id: obj.id };
    case "Class": return { t: "ClassID", id: obj.id };
  }
}

function activate_object(obj: FolderObject, dispatch: M.Dispatch): void {
  switch (obj.t) {
    case "Scene":
      dispatch({ type: "FocusGrid", scene_id: obj.id });
      return;
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
  const name = object.name;

  return <List.Item style={{ width: '100%' }}>
    <List.Icon name={object_icon(object.t)} />
    <List.Content style={{ cursor: 'pointer', width: '100%' }} onClick={handler}>
      {
        selecting
          ? <Checkbox checked={selecting.is_selected(object.path, object_to_item_id(object))}
            label={name} onChange={onCheck} />
          : name
      }
      {selecting ? null : <FolderItemDropdown />}
    </List.Content>
  </List.Item>;

  function FolderItemDropdown(_: {}) {
    return <Dropdown icon='caret down'
      className='right' pointing={true} floating={true}>
      <Dropdown.Menu>
        <Dropdown.Header content={name} />
        <Dropdown.Divider />
        <CV.ModalMaker
          button={open => <Dropdown.Item onClick={open}>Copy</Dropdown.Item>}
          header={<span>Copy {name}</span>}
          content={close => <CopyFolderItem source={object.path} onDone={close}
            item_id={object_to_item_id(object)}
            dispatch={dispatch} />}
        />
        <CV.ModalMaker
          button={open => <Dropdown.Item onClick={open}>Move</Dropdown.Item>}
          header={<span>Move {name}</span>}
          content={close => <MoveFolderItem source={object.path} onDone={close}
            item_id={object_to_item_id(object)} dispatch={dispatch} />}
        />
        <CV.ModalMaker
          button={open => <Dropdown.Item onClick={open}>Delete</Dropdown.Item>}
          header={<span>Delete {name}</span>}
          content={close => <DeleteFolderItem location={object.path} onDone={close}
            item_id={object_to_item_id(object)}
            dispatch={dispatch} />}
        />

      </Dropdown.Menu>
    </Dropdown>;
  }

  function handler() {
    if (selecting) { return; }
    return activate_object(object, dispatch);
  }

  function onCheck(_: any, data: SUI.CheckboxProps) {
    if (selecting && selecting.on_select_object && data.checked !== undefined) {
      return selecting.on_select_object(data.checked, object.path, object_to_item_id(object));
    }
  }
}

interface CopyFolderItemProps {
  source: T.FolderPath; item_id: T.FolderItemID; onDone: () => void; dispatch: M.Dispatch;
}
class CopyFolderItem extends React.Component<CopyFolderItemProps, { dest: T.FolderPath }> {
  constructor(props: CopyFolderItemProps) {
    super(props);
    this.state = { dest: props.source };
  }
  render() {
    return <div>
      <CV.Toggler
        a={toggle => <span onClick={toggle} style={{ cursor: 'pointer' }}>
          <Label><Icon name='edit' />Destination</Label>{M.folderPathToString(this.state.dest!)}
        </span>}
        b={toggle => <SelectFolder onSelect={dest => { this.setState({ dest }); toggle(); }} />}
      />
      <CF.CoolForm>
        <CF.NumericInput label="Copies" name="copies" min={1} default={1} />
        <CF.Submit onClick={d => this.copy(d as { copies: number })}>Copy!</CF.Submit>
      </CF.CoolForm></div>;
  }
  copy({ copies }: { copies: number }) {
    const { source, item_id, onDone, dispatch } = this.props;
    for (const _ of LD.range(copies)) {
      dispatch(M.sendCommand({ t: "CopyFolderItem", source, item_id, dest: this.state.dest }));
    }
    onDone();
  }
}

interface DeleteFolderItemProps {
  location: T.FolderPath; item_id: T.FolderItemID; onDone: () => void; dispatch: M.Dispatch;
}
function DeleteFolderItem(props: DeleteFolderItemProps) {
  const { location, item_id, onDone, dispatch } = props;
  return <Button onClick={deleteIt}>Yes, really!</Button>;

  function deleteIt() {
    dispatch(M.sendCommand({ t: "DeleteFolderItem", location, item_id }));
    onDone();
  }
}

interface FuseResult { item: number; matches: Array<{ indices: Array<[number, number]> }>; }

interface SearchSelectProps<T> {
  label?: string;
  values: Array<T>;
  onSelect: (value: T) => void;
  display: (t: T) => string;
}
interface SearchSelectState { results: Array<FuseResult>; current_selection: number; }

class SearchSelect<T> extends React.Component<SearchSelectProps<T>, SearchSelectState> {
  constructor(props: SearchSelectProps<T>) {
    super(props);
    this.state = { results: [], current_selection: 0 };
  }
  render(): JSX.Element | null {
    const { values, label } = this.props;
    const fuse = new Fuse(values.map(this.props.display),
      {
        shouldSort: true,
        includeMatches: true,
        minMatchCharLength: 2,
      });
    return <div>
      <Input label={label ? label : "Search"}
        onChange={(_, d) => this.search(fuse, d.value)}
        onKeyDown={(e: React.KeyboardEvent<HTMLInputElement>) => this.handleKey(e)}
      />
      <Menu vertical={true} fluid={true} style={{ height: "400px", overflowY: "auto" }}>
        {this.state.results.map((result, i) => {
          const matched_value = values[result.item];
          return <Menu.Item active={i === this.state.current_selection}
            onClick={() => this.props.onSelect(matched_value)}
            key={this.props.display(matched_value)}>
            <span
              dangerouslySetInnerHTML={{
                __html: this.highlight(this.props.display(matched_value), result.matches),
              }} />
          </Menu.Item>;
        })}

      </Menu>
    </div>;
  }

  search(fuse: Fuse, term: string) {
    const results = fuse.search<FuseResult>(term);
    this.setState({ results });
    if (this.state.current_selection >= results.length) {
      this.setState({ current_selection: Math.max(results.length - 1, 0) });
    }
  }

  handleKey(event: React.KeyboardEvent<HTMLInputElement>) {
    if (event.keyCode === 38) {
      this.setState({ current_selection: Math.max(this.state.current_selection - 1, 0) });
    } else if (event.keyCode === 40) {
      this.setState({
        current_selection: Math.min(this.state.current_selection + 1, this.state.results.length - 1),
      });
    } else if (event.keyCode === 13) {
      if (this.state.results.length > this.state.current_selection) {
        this.props.onSelect(
          this.props.values[this.state.results[this.state.current_selection].item]);
      }
    }
  }

  highlight(s: string, matches: Array<{ indices: Array<[number, number]> }>): string {
    const result = ['<span style="color: gray;">'];
    const pairs = LD.flatMap(matches, m => m.indices);
    let pair = pairs.shift();
    for (let i = 0; i < s.length; i++) {
      const char = s.charAt(i);
      if (pair && i === pair[0]) {
        result.push('<span style="color: black; font-weight: bold;">');
      }
      result.push(LD.escape(char));
      if (pair && i === pair[1]) {
        result.push('</span>');
        pair = pairs.shift();
      }
    }
    result.push('</span>');
    return result.join('');
  }
}

function collectAllFolders(path: T.FolderPath, folder: T.Folder): Array<T.FolderPath> {
  return LD.flatMap(folder.children.keySeq().toArray(),
    name => {
      const subfolder = path.concat(name);
      return [subfolder].concat(collectAllFolders(path.concat(name), folder.children.get(name)!));
    });
}

function collectFolderObjects<T>(
  ptui: M.PTUI, path: T.FolderPath, folder: T.Folder,
  getObjects: (ptui: M.PTUI, node: T.FolderNode) => Array<T>):
  Array<[T.FolderPath, T]> {
  const objs = getObjects(ptui, folder.data);
  const this_folder_results = objs.map((obj): [T.FolderPath, T] => [path, obj]);
  return LD.concat(
    this_folder_results,
    LD.flatMap(
      folder.children.entrySeq().toArray(),
      ([subname, subfolder]) =>
        collectFolderObjects(ptui, path.concat(subname), subfolder, getObjects)));
}

function collectAllItems(ptui: M.PTUI, path: T.FolderPath, folder: T.Folder):
  Array<[T.FolderPath, T.Item]> {
  return collectFolderObjects(ptui, path, folder, (ptui, node) => ptui.getItems(node.items));
}

interface SelectFolderProps { onSelect: (p: T.FolderPath) => void; }
const SelectFolder = Comp.connect<SelectFolderProps, { all_folders: Array<T.FolderPath> }>(
  ptui => ({ all_folders: collectAllFolders([], ptui.app.current_game.campaign) }),
)(class SelectFolderComp
  extends
  React.Component<SelectFolderProps & { all_folders: Array<T.FolderPath> } & M.DispatchProps,
  SearchSelectState> {

  constructor(props: SelectFolderProps & { all_folders: Array<T.FolderPath> } & M.DispatchProps) {
    super(props);
    this.state = { results: [], current_selection: 0 };
  }

  render() {
    return <SearchSelect values={this.props.all_folders}
      onSelect={(entry: T.FolderPath) => this.props.onSelect(entry)}
      display={M.folderPathToString} />;
  }

});

interface MoveFolderItemProps { source: T.FolderPath; item_id: T.FolderItemID; onDone: () => void; }
function MoveFolderItem(props: MoveFolderItemProps & M.DispatchProps) {
  const { source, item_id, onDone, dispatch } = props;
  return <div>
    <SelectFolder onSelect={move} />
  </div>;

  function move(dest: T.FolderPath) {
    dispatch(M.sendCommand({ t: "MoveFolderItem", source, item_id, dest }));
    onDone();
  }
}
