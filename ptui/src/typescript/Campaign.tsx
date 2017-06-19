import * as LD from 'lodash';
import * as React from "react";
import * as ReactDOM from "react-dom";

import * as M from './Model';
import * as T from './PTTypes';

export const Campaign = M.connectRedux(({ ptui, dispatch }: M.ReduxProps): JSX.Element => {
  return <div>
    <FolderTree folder={ptui.app.current_game.campaign} />
  </div>;
});


interface FTProps { folder: T.Folder; }
class FolderTreeComp extends React.Component<FTProps & M.ReduxProps, {}> {
  render(): JSX.Element {
    const { folder, ptui } = this.props;
    const scene_list = ptui.getScenes(folder.data.scenes).map(s => <div>{s.name}</div>);
    const map_list = ptui.getMaps(folder.data.maps).map(m => <div>{m.name}</div>);
    const creature_list = ptui.getCreatures(folder.data.creatures).map(c => <div>{c.name}</div>);
    const note_list = LD.keys(folder.data.notes).map(name => <div>{name}</div>);
    const item_list = ptui.getItems(folder.data.items).map(item => <div>{item.name}</div>);
    const subfolders = LD.sortBy(LD.toPairs(folder.children), ([name, _]) => name).map(
      ([name, subfolder]) => <div>
        {name}
        <div style={{ marginLeft: "1em" }}>
          <FolderTree folder={subfolder} />
        </div>
      </div>);
    return <div>
      {scene_list} {map_list} {creature_list} {note_list} {item_list} {subfolders}
    </div>;
  }
}
const FolderTree = M.connectRedux(FolderTreeComp);
