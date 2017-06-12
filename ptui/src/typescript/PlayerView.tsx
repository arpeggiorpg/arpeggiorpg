import * as React from "react";
import * as ReactDOM from "react-dom";
import * as T from './PTTypes';
import * as CommonView from './CommonView';
import { PTUI } from './Model';
import * as M from './Model';
import * as LD from 'lodash';

export function renderPlayerUI(
  elmApp: any,
  [id, player_id, current_scene, data]: [string, T.PlayerID, T.SceneID | undefined, any]) {
  let element = document.getElementById(id);
  console.log("[renderPlayerUI] Rendering Player component from Elm", id, element, player_id, current_scene);
  let app = T.decodeApp.decodeAny(data);
  let ptui = new M.PTUI(elmApp, app);
  ReactDOM.render(
    <PlayerUI ptui={ptui} player_id={player_id} current_scene={current_scene} />,
    element
  );
}

export class PlayerMain extends React.Component<{ app?: object; elm_app: any }, { player_id: T.PlayerID | undefined }> {
  // RADIX THOUGHTS:
  // * Woops, I'm going to need to keep calling ReactDOM.render on this PlayerMain component in my
  //   Elm workflow.
  //   I should add a new port, "renderReactMain" that causes this component to be re-rendered,
  //   and is always called on every App update from Update.elm.
  // * Point 2: This component could take `app?: any` which would be undefined in the initial render
  //   and then provided by the renderReactMain elm port.
  // * Point 3: In addition to getting the App from renderReactMain, we need the ElmApp, which can
  //   just be passed in.
  // * Point 4: I need a player_id to invoke PlayerUI, so I need to write a registration form!

  constructor(props: { elm_app: any; app?: object }) {
    super(props);
    this.state = { player_id: undefined };
  }
  render(): JSX.Element {
    if (!this.props.app) {
      return <div>Waiting for initial data</div>;
    }
    if (this.state.player_id) {
    let app = T.decodeApp.decodeAny(this.props.app);
      let pid = this.state.player_id;
      let ptui = new M.PTUI(this.props.elm_app, app);
      return <PlayerUI player_id={pid} current_scene={undefined} ptui={ptui} />;
    } else {
      return <div>Player Registration form goes here!</div>;
    }
  }
}

function PlayerUI(props: { player_id: T.PlayerID; current_scene: string | undefined; ptui: M.PTUI; })
  : JSX.Element {
  return <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
    <div style={{ flex: "1 0 auto" }}>
      <CommonView.TabbedView>
        <CommonView.Tab name="Creatures">
          <PlayerCreatures player_id={props.player_id} current_scene={props.current_scene} ptui={props.ptui} />
        </CommonView.Tab>
        <CommonView.Tab name="Combat">
          <CommonView.Combat ptui={props.ptui} />
        </CommonView.Tab>
        <CommonView.Tab name="Notes">
          <PlayerNote player_id={props.player_id} ptui={props.ptui} />
        </CommonView.Tab>
      </CommonView.TabbedView>
    </div>
    <div>
      <PlayerActionBar player_id={props.player_id} ptui={props.ptui}></PlayerActionBar>
    </div>
  </div>;
}

function PlayerCreatures(
  props: { current_scene: T.SceneID | undefined; player_id: T.PlayerID; ptui: M.PTUI; })
  : JSX.Element {
  let cids = props.ptui.app.players[props.player_id].creatures;
  let creatures = props.ptui.getCreatures(cids);
  console.log("[PlayerCreatures]", cids, creatures);
  return <div>
    {creatures.map((creature) =>
      <div key={creature.id}>
        <CommonView.CreatureCard app={props.ptui.app} creature={creature} />
        <div style={{ marginLeft: "1em" }}>
          <CommonView.Collapsible name="Inventory">
            <CommonView.CreatureInventory ptui={props.ptui} current_scene={props.current_scene}
              creature={creature} />
          </CommonView.Collapsible>
        </div>
      </div>
    )}
  </div>
}

interface PlayerNoteProps { player_id: T.PlayerID; ptui: M.PTUI; }
class PlayerNote extends React.Component<PlayerNoteProps, { content: string | undefined }> {
  private path: Array<string>;
  constructor(props: PlayerNoteProps) {
    super(props);
    this.state = { content: undefined };
    this.path = ["Players", props.player_id];
  }

  render(): JSX.Element {
    let player_folder = this.props.ptui.getFolderNode(this.path);
    if (!player_folder) {
      return <div>Please ask your GM to creature the folder "{M.folderPathToString(this.path)}"</div>;
    }
    let note = player_folder.notes["Scratch"];
    let origContent = note ? note.content : "Enter notes here!";
    return <div>
      <div><button
        disabled={this.state.content === undefined || this.state.content === origContent}
        onClick={() => this.submit(note)}>Save</button></div>
      <div><textarea style={{ width: "100%", height: "100%" }}
        defaultValue={origContent} value={this.state.content}
        onChange={(e) => this.setState({ content: e.currentTarget.value })} /></div>
    </div>;
  }

  submit(origNote: T.Note | undefined) {
    if (!this.state.content) { return; }
    let note = { name: "Scratch", content: this.state.content };
    let cmd: T.GameCommand = origNote
      ? { t: "EditNote", path: this.path, name: "Scratch", note }
      : { t: "CreateNote", path: this.path, note };
    this.props.ptui.sendCommand(cmd);
  }
}

function PlayerActionBar(props: { player_id: T.PlayerID; ptui: M.PTUI }): JSX.Element {
  if (props.ptui.app.current_game.current_combat) {
    let combat = props.ptui.app.current_game.current_combat;
    let cid = props.ptui.getCurrentCombatCreatureID(combat);
    let player = props.ptui.app.players[props.player_id];
    if (LD.includes(player.creatures, cid)) {
      let creature = props.ptui.getCreature(cid);
      if (creature) {
        return <CommonView.ActionBar combat={combat} creature={creature} ptui={props.ptui}></CommonView.ActionBar>;
      }
    }
  }
  return <noscript />;
}
