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
    const scenes = ptui.getScenes(folder.data.scenes);
    const scene_list = scenes.map(s => <div>{s.name}</div>);
    const subfolders = LD.sortBy(LD.toPairs(folder.children), ([name, _]) => name).map(
      ([name, subfolder]) => <div>{name}</div>);
    return <div>
      {scene_list}
      {/*{maps}{creatures}{notes}{items}*/}
      {subfolders}
    </div>;
  }
}
const FolderTree = M.connectRedux(FolderTreeComp);
