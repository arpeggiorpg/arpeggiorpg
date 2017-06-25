import * as LD from 'lodash';
import * as React from "react";
import * as ReactDOM from "react-dom";

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
    return <div>
      <FolderTree
        name="Campaign" path={[]} folder={ptui.app.current_game.campaign} start_open={true} />
    </div>;
  }
}
export const Campaign = M.connectRedux(CampaignComp);

interface CampaignSelectorProps {
  item_type: FolderContentType;
  allow_multiple?: boolean;
}
class CampaignSelectorComp extends React.Component<CampaignSelectorProps & M.ReduxProps, undefined> {
  shouldComponentUpdate(newProps: M.ReduxProps) {
    return newProps.ptui.app.current_game.campaign !== this.props.ptui.app.current_game.campaign;
  }
  render(): JSX.Element {
    const { ptui, dispatch } = this.props;
    console.log("[EXPENSIVE:CampaignSelector.render]");
    return <div>
      <FolderTree
        name="Campaign" path={[]} folder={ptui.app.current_game.campaign} start_open={true}
        selecting_item={this.props.item_type} allow_multiple={this.props.allow_multiple} />
    </div>;
  }
}
export const CampaignSelector = M.connectRedux(CampaignSelectorComp);


interface FTProps {
  path: T.FolderPath;
  name: string;
  folder: T.Folder;
  start_open?: boolean;
  selecting_item?: FolderContentType;
  allow_multiple?: boolean;
  item_controls?: (path: T.FolderPath, item: T.FolderItemID) => JSX.Element | undefined;
  folder_controls?: (path: T.FolderPath) => JSX.Element | undefined;
  disable_ops?: boolean; // disables selecting items, and Create, Delete, Move etc
}
class FolderTreeComp extends React.Component<FTProps & M.ReduxProps, { expanded: boolean }> {
  constructor(props: FTProps & M.ReduxProps) {
    super(props);
    this.state = { expanded: props.start_open || false };
  }
  render(): JSX.Element {
    const { folder, ptui, selecting_item } = this.props;
    function dont_show(t: FolderContentType) {
      return selecting_item && selecting_item !== t;
    }
    const scene_list = dont_show("scene") ? [] :
      ptui.getScenes(folder.data.scenes).map(s => <TreeScene key={s.name} scene={s} />);
    const map_list = dont_show("map") ? [] :
      ptui.getMaps(folder.data.maps).map(m => <TreeMap key={m.name} map={m} />);
    const creature_list = dont_show("creature") ? [] :
      ptui.getCreatures(folder.data.creatures).map(c => <TreeCreature key={c.name} creature={c} />);
    const note_list = dont_show("note") ? [] :
      LD.keys(folder.data.notes).map(
        name => <TreeNote key={name} path={this.props.path} name={name} />);
    const item_list = dont_show("item") ? [] :
      ptui.getItems(folder.data.items).map(
        item => <TreeItem key={item.name} path={this.props.path} item={item} />);
    const subfolders = LD.sortBy(LD.toPairs(folder.children), ([name, _]) => name).map(
      ([name, subfolder]) => <FolderTree key={name} name={name} folder={subfolder}
        path={LD.concat(this.props.path, name)} selecting_item={selecting_item}
        allow_multiple={this.props.allow_multiple} />
    );
    const display = this.state.expanded ? "block" : "none";
    return <div>
      <div style={{ display: "flex" }}>
        <div style={{ display: "flex", cursor: "pointer" }}
          onClick={() => this.setState({ expanded: !this.state.expanded })}>
          {this.state.expanded ? <CV.Icon>folder_open</CV.Icon> : <CV.Icon>folder</CV.Icon>}
          {this.props.name}
        </div>
      </div>
      <div style={{ marginLeft: "1em", display }}>
        {scene_list} {map_list} {creature_list} {note_list} {item_list} {subfolders}
      </div>
    </div>;
  }
}
const FolderTree = M.connectRedux(FolderTreeComp);


interface TreeObjectProps {
  allow_ops?: boolean;
  icon: string;
  text: string;
  onClick?: () => void;
}
function TreeObject(props: TreeObjectProps) {
  return <div style={{ display: "flex" }}>
    <div style={{ cursor: "pointer", display: "flex" }} onClick={props.onClick}>
      <CV.Icon>{props.icon}</CV.Icon> {props.text}
    </div>
  </div>;
}

const TreeScene = M.connectRedux(
  function TreeScene({ scene, dispatch }: { scene: T.Scene } & M.ReduxProps): JSX.Element {
    return <TreeObject icon="casino" text={scene.name}
      onClick={() => dispatch({ type: "Focus", focus: { t: "Scene", scene_id: scene.id } })}
    />;
  });

const TreeMap = M.connectRedux(
  function TreeMap({ map, dispatch }: { map: T.Map } & M.ReduxProps): JSX.Element {
    return <TreeObject icon="map" text={map.name}
      onClick={() => dispatch({ type: "Focus", focus: { t: "Map", map_id: map.id } })} />;
  } );


class TreeCreatureComp
  extends React.Component<{ creature: T.Creature } & M.ReduxProps, { expanded: boolean }> {
  constructor(props: { creature: T.Creature } & M.ReduxProps) {
    super(props);
    this.state = { expanded: false };
  }
  render(): JSX.Element {
    const creature = this.props.creature;


    return <div>
      <TreeObject icon="contacts" text={creature.name}
        onClick={() => this.setState({ expanded: !this.state.expanded })} />
      {
        this.state.expanded
          ? <div style={{ marginLeft: "1em" }}>
            <GM.GMCreatureCard creature={creature} />
            <CV.CollapsibleInventory creature={creature} />
          </div>
          : null
      }
    </div>;
  }
}

const TreeCreature = M.connectRedux(TreeCreatureComp);


const TreeNote = M.connectRedux(
  function TreeNote(
    { path, name, ptui, dispatch }: { path: T.FolderPath; name: string } & M.ReduxProps
  ): JSX.Element {
    return <TreeObject icon="note" text={name}
      onClick={() => dispatch({ type: "FocusNote", path, name })} />;
  });

const TreeItem = M.connectRedux(function TreeItem(
  { path, item, ptui, dispatch }: { path: T.FolderPath; item: T.Item } & M.ReduxProps): JSX.Element {
  return <TreeObject icon="attachment" text={item.name} />;
});
