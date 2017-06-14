import * as LD from "lodash";
import * as React from "react";
import * as ReactDOM from "react-dom";
import { Provider } from 'react-redux';
import * as Redux from 'redux';

import * as CommonView from "./CommonView";
import * as Grid from './Grid';
import { PTUI } from './Model';
import * as M from './Model';
import * as T from './PTTypes';

/// renders the player *sidebar*
export function renderPlayerUI(
  elmApp: any,
  [id, player_id, current_scene, data]: [string, T.PlayerID, T.SceneID | undefined, any]) {
  const element = document.getElementById(id);
  console.log(
    "[renderPlayerUI] Rendering Player component from Elm",
    id, element, player_id, current_scene);
  const app = T.decodeApp.decodeAny(data);
  const ptui = new M.PTUI(elmApp, app);
  const player = M.get(ptui.app.players, player_id);
  if (player) {
    ReactDOM.render(
      <PlayerSideBar ptui={ptui} player={player} current_scene={current_scene} />,
      element
    );
  }
}

interface PlayerMainProps { app?: object; elm_app: any; }
export class PlayerMain extends React.Component<PlayerMainProps,
  {
    player_id: T.PlayerID | undefined;
    typing_player_id: string;
    store: Redux.Store<M.PTUI> | undefined;
    ptui: M.PTUI | undefined
  }> {
  constructor(props: PlayerMainProps) {
    super(props);
    const ptui = props.app ? new M.PTUI(props.elm_app, T.decodeApp.decodeAny(props.app)) : undefined;
    const store = ptui ? Redux.createStore(M.update, ptui) : undefined;
    this.state = { player_id: undefined, typing_player_id: "", ptui, store };
  }

  componentWillReceiveProps(nextProps: PlayerMainProps) {
    if (!M.isEqual(this.props, nextProps)) {
      if (this.state.store) {
        if (nextProps.app) {
          this.state.store.dispatch(
            { type: "RefreshApp", app: T.decodeApp.decodeAny(nextProps.app) });
        }
      } else {
        if (nextProps.app) {
          const ptui = new M.PTUI(nextProps.elm_app, T.decodeApp.decodeAny(nextProps.app));
          const store = Redux.createStore(M.update, ptui);
          this.setState({ store });
        }
      }
    }
  }

  render(): JSX.Element {
    if (!this.state.store) {
      return <div>Waiting for initial data from server.</div>;
    }
    const ptui = this.state.store.getState();
    return <Provider store={this.state.store}>{this.ptui(ptui)}</Provider>;
  }

  ptui(ptui: M.PTUI): JSX.Element {
    if (this.state.player_id) {
      const player = M.get(ptui.app.players, this.state.player_id);
      if (player) {
        return <PlayerGameView player={player} ptui={ptui} />;
      } else {
        return <div>Couldn't find player {this.state.player_id}</div>;
      }
    } else {
      return <div>
        <h1>P&T</h1>
        <p>Welcome to P&T!</p>
        {LD.keys(ptui.app.players).length > 0
          ? <div>
            <p>You can rejoin a session if you've already registered as a player.</p>
            {LD.keys(ptui.app.players).map(pid =>
              <button key={pid} onClick={() => this.setState({ player_id: pid })}>{pid}</button>)}
          </div>
          : <noscript />}
        <p>You can register a new player. Enter your name (not your character's name) here:</p>
        <input type="text" value={this.state.typing_player_id}
          onChange={e => this.setState({ typing_player_id: e.currentTarget.value })} />
        <button
          onClick={() => this.registerPlayer()}>
          Register</button>
      </div>;
    }
  }
  registerPlayer() {
    if (!this.state.ptui) {
      console.log("[PlayerMain] registerPlayer called without PTUI state.");
      return;
    }
    this.state.ptui.sendCommand({ t: "RegisterPlayer", player_id: this.state.typing_player_id });
    this.setState({ player_id: this.state.typing_player_id });
  }
}

function PlayerGameView({ player, ptui }: { player: T.Player; ptui: M.PTUI }): JSX.Element {
  const scene = player.scene ? M.get(ptui.app.current_game.scenes, player.scene) : undefined;

  const grid = scene
    ? <Grid.Grid scene={scene} creatures={selectMapCreatures(ptui, player, scene)} />
    : <div>No scene loaded</div>;

  return <div style={{
    display: "flex", justifyContent: "space-between",
    height: "100%", width: "100%",
  }}>
    {grid}
    <div style={{ width: 450, height: "100%", border: "1px solid black" }}>
      <PlayerSideBar player={player} current_scene={player.scene} ptui={ptui} />
    </div>
  </div>;
}

function selectMapCreatures(ptui: M.PTUI, player: T.Player, scene: T.Scene)
  : { [index: string]: Grid.MapCreature } {
  const creatures = M.filterMap(
    ptui.getCreatures(LD.keys(scene.creatures)),
    creature => {
      const pos = scene.creatures[creature.id][0]; // map over keys -> [] is okay
      const class_ = M.get(ptui.app.current_game.classes, creature.class_);
      if (class_) {
        const actions = { move: (cid: T.CreatureID) => console.log("Moving creature!", cid) };
        return { creature, pos, class_, actions };
      }
    }
  );
  const result: { [index: string]: Grid.MapCreature } = {};
  for (const creature of creatures) {
    result[creature.creature.id] = creature;
  }
  return result;
}


function PlayerSideBar(props: { player: T.Player; current_scene: string | undefined; ptui: M.PTUI; })
  : JSX.Element {
  return <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
    <div style={{ flex: "1 0 auto" }}>
      <CommonView.TabbedView>
        <CommonView.Tab name="Creatures">
          <PlayerCreatures player={props.player} current_scene={props.current_scene}
            ptui={props.ptui} />
        </CommonView.Tab>
        <CommonView.Tab name="Combat">
          <CommonView.Combat ptui={props.ptui} />
        </CommonView.Tab>
        <CommonView.Tab name="Notes">
          <PlayerNote player_id={props.player.player_id} ptui={props.ptui} />
        </CommonView.Tab>
      </CommonView.TabbedView>
    </div>
    <div>
      <PlayerActionBar player={props.player} ptui={props.ptui} />
    </div>
  </div>;
}

function PlayerCreatures(
  props: { current_scene: T.SceneID | undefined; player: T.Player; ptui: M.PTUI; })
  : JSX.Element {
  const cids = props.player.creatures;
  const creatures = props.ptui.getCreatures(cids);
  console.log("[PlayerCreatures]", cids, creatures);
  if (creatures.length === 0) {
    return <div>You have no creatures in your control yet.</div>;
  }
  return <div>
    {creatures.map(creature =>
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
  </div>;
}

function PlayerNote({ player_id, ptui }: { player_id: T.PlayerID; ptui: M.PTUI; }): JSX.Element {
  let content: string | undefined;

  const path = ["Players", player_id];
  const player_folder = ptui.getFolderNode(path);
  if (!player_folder) {
    return <div>Please ask your GM to creature the folder "{M.folderPathToString(path)}"</div>;
  }
  const note = M.get(player_folder.notes, "Scratch");
  const origContent = note ? note.content : "Enter notes here!";
  return <div>
    <div><button
      disabled={content === undefined || content === origContent}
      onClick={() => submit(note)}>Save</button></div>
    <div><textarea style={{ width: "100%", height: "100%" }}
      defaultValue={origContent} value={content}
      onChange={e => content = e.currentTarget.value} /></div>
  </div>;

  function submit(origNote: T.Note | undefined) {
    if (!content) { return; }
    const newNote = { name: "Scratch", content };
    const cmd: T.GameCommand = origNote
      ? { t: "EditNote", path, name: "Scratch", note: newNote }
      : { t: "CreateNote", path, note: newNote };
    ptui.sendCommand(cmd);
  }
}

function PlayerActionBar(props: { player: T.Player; ptui: M.PTUI }): JSX.Element {
  if (props.ptui.app.current_game.current_combat) {
    const combat = props.ptui.app.current_game.current_combat;
    const cid = props.ptui.getCurrentCombatCreatureID(combat);
    if (LD.includes(props.player.creatures, cid)) {
      const creature = props.ptui.getCreature(cid);
      if (creature) {
        return <CommonView.ActionBar combat={combat} creature={creature} ptui={props.ptui} />;
      }
    }
  }
  return <noscript />;
}
