import Fuse from 'fuse.js';
import escape from 'lodash/escape';
import sortBy from 'lodash/sortBy';
import { Set } from 'immutable';
import * as React from "react";

import {
  Button, Checkbox, Divider, Dropdown, Icon, Input, Label, List, Menu,
} from 'semantic-ui-react';

import { SemanticICONS, CheckboxProps } from 'semantic-ui-react';

import * as CV from './CommonView';
import * as CF from './CoolForm';
import * as GM from './GMComponents';
import * as M from './Model';
import * as A from './Actions';
import * as T from './PTTypes';

export function Campaign() {
  return <FolderTree name="Campaign" path={[]} start_open={true} />;
}

interface MultiItemSelectorProps {
  require_selected: Set<T.ItemID>;
  on_selected: (cs: Set<T.ItemID>) => void;
  on_cancel: () => void;
}
export function MultiItemSelector(props: MultiItemSelectorProps) {
  const [selections, setSelections] = React.useState<Set<T.ItemID>>(props.require_selected);
  const items = M.useState(s =>
    collectAllItems(s).map(([path, {id, name}]) => ({path, id, name}))
  );
  const selectedItems = M.useState(s => s.getItems(selections.toArray()));
  const display = ({path, name}: {name: string, path: T.FolderPath}) =>
    `${T.folderPathToString(path)}/${name}`;
  return <div>
    {selectedItems.map(item => <Label key={item.id}>{item.name}</Label>)}
    <SearchSelect values={items}
      onSelect={({id}) => setSelections(selections.add(id))}
      display={display}
    />
    <Button onClick={() => props.on_selected(selections)}>Select Items</Button>
    <Button onClick={props.on_cancel}>Cancel</Button>
  </div>;
}

interface SceneSelectorProps {
  onCancel: () => void;
  onSelect: (sid: T.SceneID) => void;
}
export function SceneSelector(props: SceneSelectorProps) {
  // RADIX: okay, so this is a case where I think I've actually done a good job at only selecting
  // the things I need out of the store. Is this actually optimized? Is "shallow" equality actually
  // sufficient to get this? I kinda doubt it! scenes is an array of objects, and those objects are
  // created fresh on every render. I suspect we probably need deep equality.
  const scenes: {id: string, name: string, path: T.FolderPath}[] = M.useState(s => {
    return collectAllScenes(s).map(([path, {id, name}]) => ({path, id, name}));
  });
  const display = ({path, name}: {name: string, path: T.FolderPath}) =>
    `${T.folderPathToString(path)}/${name}`;
  return <div>
    <SearchSelect values={scenes}
      onSelect={({id}) => props.onSelect(id)}
      display={display}
    />
    <Button onClick={props.onCancel}>Cancel</Button>
  </div>;
}

interface MultiSceneSelectorProps {
  already_selected: Set<T.SceneID>;
  on_selected: (cs: Set<T.SceneID>) => void;
  on_cancel: () => void;
}
export function MultiSceneSelector(props: MultiSceneSelectorProps) {
  const [selections, setSelections] = React.useState<Set<T.ItemID>>(props.already_selected);
  const scenes = M.useState(s => collectAllScenes(s)).map(([path, {id, name}]) => ({path, id, name}));
  const selectedScenes = M.useState(s => s.getScenes(selections.toArray()).map(({id, name}) => ({id, name})));
  const display = ({path, name}: {name: string, path: T.FolderPath}) =>
    `${T.folderPathToString(path)}/${name}`;
  return <div>
    <Menu compact={true}>
      <Menu.Item header={true}>Currently selected</Menu.Item>
      {selectedScenes.map(
        scene => <Menu.Item key={scene.id}>
          {scene.name}
          <Icon name="delete" style={{ cursor: 'pointer' }}
            onClick={() => setSelections(selections.remove(scene.id))}
          />
        </Menu.Item>)}
    </Menu>
    <SearchSelect values={scenes}
      onSelect={({id}) => setSelections(selections.add(id))}
      display={display}
    />
    <Button onClick={() => props.on_selected(selections)}>Select Scenes</Button>
    <Button onClick={props.on_cancel}>Cancel</Button>
  </div>;
}

interface MultiCreatureSelectorProps {
  already_selected: Set<T.CreatureID>;
  on_selected: (cs: Set<T.CreatureID>) => void;
  on_cancel: () => void;
}
export class MultiCreatureSelector extends React.Component< MultiCreatureSelectorProps, { selections: Set<T.CreatureID> }> {
  constructor(props: MultiCreatureSelectorProps) {
    super(props);
    this.state = { selections: this.props.already_selected };
  }
  render(): JSX.Element {
    const on_check = (checked: boolean, _: T.FolderPath, folder_item: T.FolderItemID) => {
      if ("CreatureID" in folder_item) {
          const new_selected = checked
            ? this.state.selections.add(folder_item.CreatureID)
            : this.state.selections.remove(folder_item.CreatureID);
          this.setState({ selections: new_selected });
          return;
      } else {
        console.log("Got a non-creature selection in a creature-only campaign selector:", folder_item);
      }
    };
    const selecting: SelectableProps = {
      item_type: 'Creature', allow_multiple: true, on_select_object: on_check,
      is_selected: (_, item_id) =>
        "CreatureID" in item_id && this.state.selections.includes(item_id.CreatureID),
    };
    return <div>
      <FolderTree name="Campaign" path={[]} start_open={true}
        selecting={selecting} />
      <Button onClick={() => this.props.on_selected(this.state.selections)}>Select Creatures</Button>
      <Button onClick={this.props.on_cancel}>Cancel</Button>
    </div>;
  }
}

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
  start_open?: boolean;
  selecting?: SelectableProps;
}

function FolderTree(props: FTProps) {
  const { selecting, path } = props;
  const [expanded, setExpanded] = React.useState(props.start_open || false);
  const folder = M.useState(s => s.getFolder(path));
  if (!folder) { return <div>No folder found for {T.folderPathToString(path)}</div>;}

  const objects = M.useState(s => useFolderTreeData(s, path, folder, selecting));

  const children = [
    section("Notes", objects.note_objects),
    section("Classes", objects.class_objects),
    section("Abilities", objects.ability_objects),
    section("Scenes", objects.scene_objects),
    section("Creatures", objects.creature_objects),
    section("Items", objects.item_objects),
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
        const variant = Object.keys(iid)[0] ?? "unknown";
        const iidvalue = Object.values(iid)[0] ?? "unknown";
        return <TreeObject key={`${variant}/${iidvalue}`} object={obj} selecting={selecting}
          />;
      }));
  }

  const subfolders = Array.from(folder.children.keys()).sort().map(
    name =>
      <FolderTree key={name} name={name} path={[...props.path, name]} selecting={selecting} />
  );
  const folder_menu = <Dropdown icon='ellipsis horizontal'>
    <Dropdown.Menu>
      <Dropdown.Header content={T.folderPathToString(path)} />
      <CV.ModalMaker
        button={open =>
          <Dropdown.Item icon={object_icon("Scene")} text='Create Scene' onClick={open} />}
        header={<span>Create new scene in {T.folderPathToString(path)}</span>}
        content={close => <GM.CreateScene path={path} onDone={close} />} />
      <CV.ModalMaker
        button={open =>
          <Dropdown.Item icon={object_icon("Creature")} onClick={open} text='Create Creature' />}
        header={<span>Create new creature in {T.folderPathToString(path)}</span>}
        content={close => <GM.CreateCreature path={path} onClose={close} />}
      />
      <Dropdown.Item icon={object_icon("Note")} text='Create Note'
        onClick={() =>
          M.getState().setSecondaryFocus({ t: "Note", path, name: undefined })} />
      <CV.ModalMaker
        button={toggler =>
          <Dropdown.Item icon={object_icon("Item")} text='Create Item' onClick={toggler} />}
        header={<span>Create item in {T.folderPathToString(path)}</span>}
        content={toggler => <GM.GMCreateItem path={path} onClose={toggler} />} />
      <CV.ModalMaker
        button={open => <Dropdown.Item icon={object_icon("Folder")} text='Create Folder'
          onClick={open} />}
        header={<span>Create a folder in {T.folderPathToString(path)}</span>}
        content={close => <GM.CreateFolder path={path} onDone={close} />}
      />

      <Dropdown.Divider />

      {path.length > 0
        ? <>
          <Dropdown.Item text="Delete this folder" icon="delete"
            onClick={() => A.sendCommand(
              {
                t: "DeleteFolderItem", location: path.slice(0, -1),
                // "!": we KNOW this isn't [] (see conditional above)
                item_id: { SubfolderID: path.at(-1)! },
              })} />
          <CV.ModalMaker
            button={open => <Dropdown.Item text="Move this folder" icon="font" onClick={open} />}
            header={<span>Rename {T.folderPathToString(path)}</span>}
            content={close => <MoveFolderItem
              source={path.slice(0, -1)} item_id={{ SubfolderID: path.at(-1)! }}
              onDone={close} />}
          />
          <Dropdown.Divider />
        </>
        : null}

      <CV.ModalMaker
        button={open =>
          <Dropdown.Item text="Export as module" icon="upload" onClick={open} />}
        header={<span>Export folder</span>}
        content={close => <GM.ExportModule path={path} onDone={close} />} />
      <CV.ModalMaker
        button={open =>
          <Dropdown.Item text="Import module here" icon="download" onClick={open} />}
        header={<span>Import Module</span>}
        content={close => <GM.ImportModule path={path} onDone={close} />} />
    </Dropdown.Menu>
  </Dropdown>;
  const list_item = <List.Item>
    <List.Icon name={expanded ? 'folder open' : 'folder'} />
    <List.Content style={{ width: '100%' }}>
      <List.Header style={{ cursor: "pointer" }}
        onClick={() => setExpanded(!expanded)}>
        {props.name}
        {expanded ? folder_menu : null}
      </List.Header>
      {expanded
        ? <List.List>
          {children}
          {subfolders.length > 0 ? divider("Folders") : null}
          {subfolders}
        </List.List>
        : null}
    </List.Content>
  </List.Item>;

  if (path.length === 0) {
    return <List size="large">{list_item}</List>;
  } else {
    return list_item;
  }
}

interface FTData {
  class_objects: Array<FolderObject>;
  ability_objects: Array<FolderObject>;
  note_objects: Array<FolderObject>;
  scene_objects: Array<FolderObject>;
  creature_objects: Array<FolderObject>;
  item_objects: Array<FolderObject>;
}

function useFolderTreeData(state: M.AllStates, path: T.FolderPath, folder: T.Folder, selecting?: SelectableProps): FTData {
  function dont_show(t: FolderContentType) {
    return selecting && selecting.item_type !== t;
  }

  const scene_objects =
    dont_show("Scene") ? []
    : state.getScenes(folder.data.scenes).map(
        (scene): FolderObject => ({ t: "Scene", path, id: scene.id, name: scene.name }));
  const creature_objects =
    dont_show("Creature") ? []
    : state.getCreatures(folder.data.creatures).map(
      (creature): FolderObject => ({ t: "Creature", path, id: creature.id, name: creature.name }));
  const item_objects =
    dont_show("Item") ? []
    : state.getItems(folder.data.items).map(
      (item): FolderObject => ({ t: "Item", path, id: item.id, name: item.name }));
  const ability_objects =
    dont_show("Ability") ? []
    : state.getAbilities(folder.data.abilities).map(
      (item): FolderObject => ({ t: "Ability", path, id: item.id, name: item.name }));
  const class_objects =
    dont_show("Class") ? []
    : state.getClasses(folder.data.classes).map(
      (item): FolderObject => ({ t: "Class", path, id: item.id, name: item.name }));

  const note_objects = dont_show("Note") ? [] :
    sortBy(Object.keys(folder.data.notes), n => n).map(
      (name): FolderObject => ({ t: "Note", path, name }));

  return {
    note_objects,
    class_objects,
    ability_objects,
    scene_objects,
    creature_objects,
    item_objects,
  };
}


function object_icon(name: FolderContentType): SemanticICONS {
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
    case "Note": return { NoteID: obj.name };
    case "Scene": return { SceneID: obj.id };
    case "Creature": return { CreatureID: obj.id };
    case "Item": return { ItemID: obj.id };
    case "Ability": return { AbilityID: obj.id };
    case "Class": return { ClassID: obj.id };
  }
}

function activate_object(obj: FolderObject): void {
  switch (obj.t) {
    case "Scene":
      M.getState().setGridFocus(obj.id);
      return;
    case "Creature":
      M.getState().setSecondaryFocus({ t: "Creature", creature_id: obj.id });
      return;
    case "Note":
      M.getState().setSecondaryFocus({ t: "Note", path: obj.path, name: obj.name });
      return;
    case "Item":
      M.getState().setSecondaryFocus({ t: "Item", item_id: obj.id });
      return;
  }
}


interface TreeObjectProps {
  object: FolderObject;
  selecting: SelectableProps | undefined;
}

function TreeObject({ object, selecting }: TreeObjectProps) {
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
            />}
        />
        <CV.ModalMaker
          button={open => <Dropdown.Item onClick={open}>Move</Dropdown.Item>}
          header={<span>Move {name}</span>}
          content={close => <MoveFolderItem source={object.path} onDone={close}
            item_id={object_to_item_id(object)} />}
        />
        <CV.ModalMaker
          button={open => <Dropdown.Item onClick={open}>Delete</Dropdown.Item>}
          header={<span>Delete {name}</span>}
          content={close => <DeleteFolderItem location={object.path} onDone={close}
            item_id={object_to_item_id(object)}
            />}
        />

      </Dropdown.Menu>
    </Dropdown>;
  }

  function handler() {
    if (selecting) { return; }
    return activate_object(object);
  }

  function onCheck(_: any, data: CheckboxProps) {
    if (selecting && selecting.on_select_object && data.checked !== undefined) {
      return selecting.on_select_object(data.checked, object.path, object_to_item_id(object));
    }
  }
}

interface CopyFolderItemProps {
  source: T.FolderPath; item_id: T.FolderItemID; onDone: () => void;
}
function CopyFolderItem(props: CopyFolderItemProps) {
  const [dest, setDest] = React.useState<T.FolderPath>(props.source);
  return <div>
    <CV.Toggler
      a={toggle => <span onClick={toggle} style={{ cursor: 'pointer' }}>
        <Label><Icon name='edit' />Destination</Label>{T.folderPathToString(dest)}
      </span>}
      b={toggle => <SelectFolder onSelect={dest => { setDest(dest); toggle(); }} />}
    />
    <CF.CoolForm>
      <CF.NumericInput label="Copies" name="copies" min={1} default={1} />
      <CF.Submit onClick={d => copy(d as { copies: number })}>Copy!</CF.Submit>
    </CF.CoolForm></div>;

function copy({ copies }: { copies: number }) {
    const { source, item_id, onDone } = props;
    for (const _ of Array(5).keys()) {
      A.sendCommand({ t: "CopyFolderItem", source, item_id, dest: dest });
    }
    onDone();
  }
}

interface DeleteFolderItemProps {
  location: T.FolderPath; item_id: T.FolderItemID; onDone: () => void;
}
function DeleteFolderItem(props: DeleteFolderItemProps) {
  const { location, item_id, onDone } = props;
  return <Button onClick={deleteIt}>Yes, really!</Button>;

  function deleteIt() {
    A.sendCommand({ t: "DeleteFolderItem", location, item_id });
    onDone();
  }
}

interface SearchSelectProps<T> {
  label?: string;
  values: Array<T>;
  onSelect: (value: T) => void;
  display: (t: T) => string;
}
function SearchSelect<T>(props: SearchSelectProps<T>) {
  const [results, setResults] = React.useState<Fuse.FuseResult<string>[]>([]);
  const [ currentSelection, setCurrentSelection] = React.useState<number>(0);
  const { values, label } = props;
  const fuse = new Fuse(values.map(props.display),
    {
      shouldSort: true,
      includeMatches: true,
      minMatchCharLength: 0,
    });
  const optionsToDisplay = results.length === 0 ? values.slice(0, 20).map((v, i) => ({refIndex: i, matches: []})) : results;
  return <div>
    <Input label={label ?? "Search"}
      onChange={(_, d) => search(fuse, d.value)}
      onKeyDown={(e: React.KeyboardEvent<HTMLInputElement>) => handleKey(e)}
    />
    <Menu vertical={true} fluid={true} style={{ height: "400px", overflowY: "auto" }}>
      {optionsToDisplay.map((result, i) => {
        const matchedValue = values[result.refIndex];
        if (!matchedValue) { return; }
        return <Menu.Item active={i === currentSelection}
          onClick={() => props.onSelect(matchedValue)}
          key={props.display(matchedValue)}>
          {/* RADIX WTF? */}
          <span
            dangerouslySetInnerHTML={{
              __html: highlight(props.display(matchedValue), result.matches ?? []),
            }} />
        </Menu.Item>;
      })}

    </Menu>
  </div>;

  function search(fuse: Fuse<string>, term: string) {
    const results = fuse.search(term);
    setResults(results);
    if (currentSelection >= results.length) {
      setCurrentSelection(Math.max(results.length - 1, 0));
    }
  }

  function handleKey(event: React.KeyboardEvent<HTMLInputElement>) {
    console.log(event.key);
    if (event.key === "ArrowUp") {
      setCurrentSelection(Math.max(currentSelection - 1, 0));
    } else if (event.key === "ArrowDown") {
      setCurrentSelection(Math.min(currentSelection + 1, results.length - 1));
    } else if (event.key === "Enter") {
      const result = results[currentSelection];
      if (result) {
        const value = values[result.refIndex];
        if (value)
          props.onSelect(value);
      }
    }
  }

  function highlight(s: string, matches: readonly Fuse.FuseResultMatch[]): string {
    const result = ['<span style="color: gray;">'];
    const pairs = matches.flatMap(m => m.indices);
    let pair = pairs.shift();
    for (let i = 0; i < s.length; i++) {
      const char = s.charAt(i);
      if (pair && i === pair[0]) {
        result.push('<span style="color: black; font-weight: bold;">');
      }
      result.push(escape(char));
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
  return folder.children.keySeq().toArray().flatMap(
    name => {
      const subfolder = path.concat(name);
      return [subfolder].concat(collectAllFolders(path.concat(name), folder.children.get(name)!));
    });
}

function collectFolderObjects<T>(
  path: T.FolderPath, folder: T.Folder, getObjects: (node: T.FolderNode) => Array<T>
): Array<[T.FolderPath, T]> {
  const objs = getObjects(folder.data);
  const this_folder_results = objs.map((obj): [T.FolderPath, T] => [path, obj]);
  return this_folder_results.concat(
      folder.children.entrySeq().toArray().flatMap(
        ([subname, subfolder]) => collectFolderObjects(path.concat(subname), subfolder, getObjects)
      )
  );
}

function collectAllItems(state: M.AppState): Array<[T.FolderPath, T.Item]> {
  return collectFolderObjects([], state.getGame().campaign, node => state.getItems(node.items));
}

function collectAllScenes(state: M.AppState): Array<[T.FolderPath, T.Scene]> {
  return collectFolderObjects([], state.getGame().campaign, node => state.getScenes(node.scenes));
}

interface SelectFolderProps { onSelect: (p: T.FolderPath) => void; }
function SelectFolder(props: SelectFolderProps) {
  const all_folders = M.useState(s => collectAllFolders([], s.getGame().campaign));

  return <SearchSelect values={all_folders}
    onSelect={(entry: T.FolderPath) => props.onSelect(entry)}
    display={T.folderPathToString} />;
}

interface MoveFolderItemProps { source: T.FolderPath; item_id: T.FolderItemID; onDone: () => void; }
function MoveFolderItem(props: MoveFolderItemProps) {
  const { source, item_id, onDone } = props;
  return <div>
    <SelectFolder onSelect={move} />
  </div>;

  function move(dest: T.FolderPath) {
    A.sendCommand({ t: "MoveFolderItem", source, item_id, dest });
    onDone();
  }
}
