import * as React from "react";
import * as ReactDOM from "react-dom";
import * as T from './PTTypes';
import * as CommonView from './CommonView';
import { PTUI } from './Model';

export function renderPlayerUI(
  elmApp: any,
  [id, player_id, current_scene, data]: [string, T.PlayerID, T.SceneID | undefined, any]) {
  let element = document.getElementById(id);
  console.log("[renderPlayerUI] Rendering Player component from Elm", id, element, player_id, current_scene);
  let app = T.decodeApp.decodeAny(data);
  let ptui = new PTUI(elmApp, app);
  ReactDOM.render(
    <PlayerUI ptui={ptui} player_id={player_id} current_scene={current_scene} />,
    element
  );
}

function PlayerUI(props: { player_id: T.PlayerID; current_scene: string | undefined; ptui: PTUI; })
  : JSX.Element {
  return <CommonView.TabbedView>
    <CommonView.Tab name="Creatures">
      <PlayerCreatures player_id={props.player_id} current_scene={props.current_scene} ptui={props.ptui} />
    </CommonView.Tab>
    <CommonView.Tab name="Combat">
      <CommonView.Combat ptui={props.ptui} />
    </CommonView.Tab>
    <CommonView.Tab name="Notes">
      <PlayerNote player_id={props.player_id} ptui={props.ptui} />
    </CommonView.Tab>
  </CommonView.TabbedView>;
}

function PlayerCreatures(
  props: { current_scene: T.SceneID | undefined; player_id: T.PlayerID; ptui: PTUI; })
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

interface PlayerNoteProps { player_id: T.PlayerID; ptui: PTUI; }
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
      return <div>Please ask your GM to creature the folder "{this.path}"</div>;
    }
    let note = player_folder.notes["Scratch"];
    let origContent = note ? note.content : "Enter notes here!";
    return <div>
      <div><button
        disabled={this.state.content === origContent}
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
