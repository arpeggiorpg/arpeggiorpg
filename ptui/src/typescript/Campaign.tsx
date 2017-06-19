import * as LD from 'lodash';
import * as React from "react";
import * as ReactDOM from "react-dom";

import * as CV from './CommonView';
import * as M from './Model';
import * as T from './PTTypes';

export const Campaign = M.connectRedux(({ ptui, dispatch }: M.ReduxProps): JSX.Element => {
  return <div>
    <FolderTree name="Campaign" folder={ptui.app.current_game.campaign} start_open={true} />
  </div>;
});

interface FTProps { name: string; folder: T.Folder; start_open?: boolean; }
class FolderTreeComp extends React.Component<FTProps & M.ReduxProps, { expanded: boolean }> {
  constructor(props: FTProps & M.ReduxProps) {
    super(props);
    this.state = { expanded: props.start_open || false };
  }
  render(): JSX.Element {
    const { folder, ptui } = this.props;
    const scene_list = ptui.getScenes(folder.data.scenes).map(
      s => <div style={{ display: "flex" }}><CV.Icon>casino</CV.Icon>{s.name}</div>);
    const map_list = ptui.getMaps(folder.data.maps).map(
      m => <div style={{ display: "flex" }}><CV.Icon>map</CV.Icon>{m.name}</div>);
    const creature_list = ptui.getCreatures(folder.data.creatures).map(
      c => <div style={{ display: "flex" }}><CV.Icon>contacts</CV.Icon>{c.name}</div>);
    const note_list = LD.keys(folder.data.notes).map(
      name => <div style={{ display: "flex" }}><CV.Icon>note</CV.Icon>{name}</div>);
    const item_list = ptui.getItems(folder.data.items).map(
      item => <div style={{ display: "flex" }}><CV.Icon>attachment</CV.Icon>{item.name}</div>);
    const subfolders = LD.sortBy(LD.toPairs(folder.children), ([name, _]) => name).map(
      ([name, subfolder]) => <div>
        <FolderTree name={name} folder={subfolder} />
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