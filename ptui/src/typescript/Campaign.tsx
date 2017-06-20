import * as LD from 'lodash';
import * as React from "react";
import * as ReactDOM from "react-dom";

import * as CV from './CommonView';
import * as M from './Model';
import * as T from './PTTypes';

export const Campaign = M.connectRedux(({ ptui, dispatch }: M.ReduxProps): JSX.Element => {
  return <div>
    <FolderTree
      name="Campaign" path={[]} folder={ptui.app.current_game.campaign} start_open={true} />
  </div>;
});

interface FTProps {
  path: T.FolderPath;
  name: string;
  folder: T.Folder;
  start_open?: boolean;
}
class FolderTreeComp extends React.Component<FTProps & M.ReduxProps, { expanded: boolean }> {
  constructor(props: FTProps & M.ReduxProps) {
    super(props);
    this.state = { expanded: props.start_open || false };
  }
  render(): JSX.Element {
    const { folder, ptui } = this.props;
    const scene_list = ptui.getScenes(folder.data.scenes).map(
      s => <SceneItem key={s.name} scene={s} />);
    const map_list = ptui.getMaps(folder.data.maps).map(
      m => <MapItem key={m.name} map={m} />);
    const creature_list = ptui.getCreatures(folder.data.creatures).map(
      c => <CreatureItem key={c.name} creature={c} />);
    const note_list = LD.keys(folder.data.notes).map(
      name => <NoteItem key={name} path={this.props.path} name={name} />);
    const item_list = ptui.getItems(folder.data.items).map(
      item => <div style={{ display: "flex" }}><CV.Icon>attachment</CV.Icon>{item.name}</div>);
    const subfolders = LD.sortBy(LD.toPairs(folder.children), ([name, _]) => name).map(
      ([name, subfolder]) => <div>
        <FolderTree name={name} folder={subfolder} path={LD.concat(this.props.path, name)} />
      </div>);
    const display = this.state.expanded ? "block" : "none";
    return <div>
      <div style={{ display: "flex" }}>
        <div style={{ display: "flex", cursor: "pointer" }}
          onClick={() => this.setState({ expanded: !this.state.expanded })}>
          <CV.Icon>folder_open</CV.Icon> {this.props.name}
        </div>
      </div>
      <div style={{ marginLeft: "1em", display }}>
        {scene_list} {map_list} {creature_list} {note_list} {item_list} {subfolders}
      </div>
    </div>;
  }
}
const FolderTree = M.connectRedux(FolderTreeComp);


const SceneItem = M.connectRedux(
  function SceneItem({ scene, dispatch }: { scene: T.Scene } & M.ReduxProps): JSX.Element {
    return <div style={{ display: "flex" }}>
      <div style={{ display: "flex", cursor: "pointer" }}
        onClick={() => dispatch({ type: "Focus", focus: { t: "Scene", scene_id: scene.id } })}>
        <CV.Icon>casino</CV.Icon>{scene.name}</div>
    </div >;
  });

const MapItem = M.connectRedux(
  function MapItem({ map, dispatch }: { map: T.Map } & M.ReduxProps): JSX.Element {
    return <div style={{ display: "flex" }}>
      <div style={{ display: "flex", cursor: "pointer" }}
        onClick={() => dispatch({ type: "Focus", focus: { t: "Map", map_id: map.id } })}>
        <CV.Icon>map</CV.Icon>{map.name}</div>
    </div >;
  }
);


class CreatureItemComp
  extends React.Component<{ creature: T.Creature } & M.ReduxProps, { expanded: boolean }> {
  constructor(props: { creature: T.Creature } & M.ReduxProps) {
    super(props);
    this.state = { expanded: false };
  }
  render(): JSX.Element {
    const creature = this.props.creature;

    const display = this.state.expanded ? "block" : "none";

    return <div>
      <div style={{ display: "flex" }}>
        <div style={{ display: "flex", cursor: "pointer" }}
          onClick={() => this.setState({ expanded: !this.state.expanded })}>
          <CV.Icon>contacts</CV.Icon>{creature.name}
        </div>
      </div>
      <div style={{ display, marginLeft: "1em" }}>
        <GMCreatureCard creature={creature} />
        <CV.Collapsible name="Inventory">
          <CV.CreatureInventory creature={creature} />
        </CV.Collapsible>
      </div>
    </div>;
  }
}

const CreatureItem = M.connectRedux(CreatureItemComp);


function GMCreatureCard(props: { creature: T.Creature }): JSX.Element {
  return <CV.CreatureCard creature={props.creature}>
    <textarea value={props.creature.note} />
  </CV.CreatureCard>;
}


const NoteItem = M.connectRedux(
  function NoteItem({ path, name, ptui, dispatch }:
    { path: T.FolderPath; name: string } & M.ReduxProps): JSX.Element {
    return <div style={{ display: "flex", cursor: "pointer" }}
      onClick={() => dispatch({ type: "FocusNote", path, name })}>
      <CV.Icon>note</CV.Icon>{name}
    </div>;
  }
);
