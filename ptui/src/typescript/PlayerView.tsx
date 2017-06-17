import * as LD from "lodash";
import * as React from "react";
import * as ReactDOM from "react-dom";
import { Provider } from 'react-redux';
import { WindowResizeListener } from 'react-window-resize-listener';
import * as Redux from 'redux';

import * as CommonView from "./CommonView";
import * as Grid from './Grid';
import { PTUI } from './Model';
import * as M from './Model';
import * as T from './PTTypes';

interface PlayerMainProps {
  app?: object;
  rpi_url: string;
}
export class PlayerMain extends React.Component<PlayerMainProps,
  {
    typing_player_id: string;
    store: Redux.Store<M.PTUI> | undefined;
  }> {
  constructor(props: PlayerMainProps) {
    super(props);
    const ptui = props.app
      ? new M.PTUI(props.rpi_url, T.decodeApp.decodeAny(props.app))
      : undefined;
    const store = ptui ? Redux.createStore(M.update, ptui) : undefined;
    this.state = { typing_player_id: "", store };
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
          const ptui = new M.PTUI(
            nextProps.rpi_url, T.decodeApp.decodeAny(nextProps.app));
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
    return <Provider store={this.state.store}>{this.renderGame(this.state.store, ptui)}</Provider>;
  }

  renderGame(store: Redux.Store<M.PTUI>, ptui: M.PTUI): JSX.Element {
    if (ptui.state.player_id) {
      const player = M.get(ptui.app.players, ptui.state.player_id);
      if (player) {
        return <PlayerGameView player={player} />;
      } else {
        return <div>Couldn't find player {ptui.state.player_id}</div>;
      }
    } else {
      return <div style={{ display: "flex", justifyContent: "space-around" }}>
        <div style={{ width: "600px" }}>
          <h1>P&T</h1>
          <p>Welcome to P&T!</p>
          {LD.keys(ptui.app.players).length > 0
            ? <div>
              <p>You can rejoin a session if you've already registered as a player.</p>
              {LD.keys(ptui.app.players).map(pid =>
                <button key={pid}
                  style={{ height: "40px", width: "80px" }}
                  onClick={() => {
                    store.dispatch({ type: "SetPlayerID", pid });
                    // TODO FIXME: This whole component's lifecycle/props/state is GARBAGE
                    this.forceUpdate();
                  }}>
                  {pid}
                </button>)}
            </div>
            : <noscript />}
          <p>You can register a new player. Enter your name (not your character's name) here:</p>
          <input style={{ fontSize: "20px" }} type="text" value={this.state.typing_player_id}
            onChange={e => this.setState({ typing_player_id: e.currentTarget.value })} />
          <button style={{ height: "40px", width: "80px" }}
            onClick={() => this.registerPlayer(store)}>
            Register</button>
        </div>
      </div>;
    }
  }
  registerPlayer(store: Redux.Store<M.PTUI>) {
    const ptui = store.getState();
    ptui.sendCommand(store.dispatch,
      { t: "RegisterPlayer", player_id: this.state.typing_player_id });
    store.dispatch({ type: "SetPlayerID", pid: this.state.typing_player_id });
  }
}

interface PlayerGameViewProps { player: T.Player; }
class PlayerGameViewComp extends React.Component<PlayerGameViewProps & M.ReduxProps,
  { width: number; height: number }> {

  constructor(props: PlayerGameViewProps & M.ReduxProps) {
    super(props);
    this.state = { width: window.innerWidth, height: window.innerHeight };
  }

  render(): JSX.Element {
    const { player, ptui, dispatch } = this.props;
    const scene = player.scene ? M.get(ptui.app.current_game.scenes, player.scene) : undefined;

    const grid = scene
      ? <Grid.Grid scene={scene} creatures={selectMapCreatures(ptui, player, scene, dispatch)} />
      : <div>No scene loaded</div>;

    console.log("[PlayerGameView.render]");

    const contents = this.state.width >= 880 ? wideView() : narrowView(this.state.width);

    return <div style={{ height: "100%", width: "100%" }} >
      <WindowResizeListener
        onResize={({ windowWidth, windowHeight }) =>
          this.setState({ width: windowWidth, height: windowHeight })} />
      {contents}
      <PlayerModal />
    </div >;


    function wideView() {
      return <div style={{ width: "100%", height: "100%", display: "flex" }}>
        <div style={{ flex: "1" }}>{grid}</div>
        <div style={{ width: 450, height: "100%", border: "1px solid black" }}>
          <PlayerSideBar player={player} current_scene={player.scene} />
        </div>
      </div>;
    }

    function narrowView(width: number) {
      const scale = width / 450;
      return <div style={{
        height: "100%",
        width: "450px",
        // transformOrigin: "top left",
        // transform: `scale(${scale})`,
        // overflow: "hidden",
        zoom: `${scale * 100}%`,
      }}>
        <div style={{ width: 450}}>
          <PlayerSideBar player={player} current_scene={player.scene} is_narrow={grid} />
        </div>
      </div>;
    }
  }
}

export const PlayerGameView = M.connectRedux(PlayerGameViewComp);


function playerModal({ ptui, dispatch }: M.ReduxProps): JSX.Element {
  if (ptui.state.error) {
    return <div style={{
      position: "fixed", top: "50%", left: "50%", transform: "translate(-50%, -50%)",
      backgroundColor: "white",
      border: "1px solid black",
      minHeight: "30%",
      minWidth: "30%",
      borderRadius: "5px",
      display: "flex",
      flexDirection: "column",
    }}>
      <h1>Error</h1>
      <div style={{ flex: "1 0 auto" }}>{ptui.state.error}</div>
      <div style={{ display: "flex", justifyContent: "space-around" }}>
        <div>
          <button style={{ minHeight: "40px", minWidth: "80px" }}
            onClick={() => dispatch({ type: "ClearError" })}>Ok</button>
        </div>
      </div>
    </div>;
  }
  return <noscript />;
}
export const PlayerModal = M.connectRedux(playerModal);

/**
 * Figure out which creatures to display, and create [[Grid.MapCreature]] for each of them.
 * This involves figuring out what kind of actions this player can perform on each creature, such as
 * moving them (if the player has control of them), or targeting them for an ability that has been
 * selected.
 */
function selectMapCreatures(
  ptui: M.PTUI, player: T.Player, scene: T.Scene, dispatch: M.Dispatch)
  : { [index: string]: Grid.MapCreature } {
  const creatures = M.filterMap(
    ptui.getCreatures(LD.keys(scene.creatures)),
    creature => {
      const [pos, vis] = scene.creatures[creature.id]; // map over keys -> [] is okay
      const class_ = M.get(ptui.app.current_game.classes, creature.class_);
      if (class_ && vis.t === "AllPlayers") {
        const actions = creatureMenuActions(ptui, dispatch, player, creature);
        return { creature, pos, class_, actions };
      }
    });
  const result: { [index: string]: Grid.MapCreature } = {};
  for (const creature of creatures) {
    result[creature.creature.id] = creature;
  }
  return result;
}

function creatureMenuActions(
  ptui: M.PTUI, dispatch: M.Dispatch, player: T.Player, creature: T.Creature) {
  const actions: { [index: string]: (cid: T.CreatureID) => void } = {};
  const move = moveAction();
  if (move) {
    actions["Move this creature"] = move;
  }
  const target = targetAction();
  if (target) {
    actions[target.name] = target.action;
  }
  return actions;

  function moveAction(): ((cid: T.CreatureID) => void) | undefined {
    if (!LD.includes(player.creatures, creature.id)) { return undefined; }
    const combat = ptui.app.current_game.current_combat;
    if (combat) {
      if (ptui.getCurrentCombatCreatureID(combat) === creature.id) {
        return cid => ptui.requestCombatMovement(dispatch);
      } else {
        return undefined;
      }
    } else {
      return cid => ptui.requestMove(dispatch, cid);
    }
  }

  function targetAction(): { name: string, action: ((cid: T.CreatureID) => void) } | undefined {
    if (ptui.state.grid.target_options) {
      const { ability_id, options } = ptui.state.grid.target_options;
      if (options.t !== "CreatureIDs") { return undefined; }
      if (LD.includes(options.cids, creature.id)) {
        const ability = M.get(ptui.app.current_game.abilities, ability_id);
        if (ability) {
          return {
            name: `${ability.name} this creature`,
            action: cid => { ptui.executeCombatAbility(dispatch, cid); },
          };
        }
      }
    }
  }

}


export const PlayerSideBar = M.connectRedux(
  (props: {
    player: T.Player; current_scene: string | undefined;
    is_narrow?: JSX.Element;
  } & M.ReduxProps): JSX.Element => {
    return <div style={{ display: "flex", flexDirection: "column", height: "100%", width: "100%" }}>
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
            <PlayerNote player_id={props.player.player_id} />
          </CommonView.Tab>
          {props.is_narrow ? <CommonView.Tab name="Map">{props.is_narrow}</CommonView.Tab> : null}
        </CommonView.TabbedView>
      </div>
      <PlayerActionBar player={props.player} ptui={props.ptui} />
    </div>;
  });

function PlayerCreatures(
  props: { current_scene: T.SceneID | undefined; player: T.Player; ptui: M.PTUI; })
  : JSX.Element {
  const cids = props.player.creatures;
  const creatures = props.ptui.getCreatures(cids);
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

function playerNote({ player_id, ptui, dispatch }: { player_id: T.PlayerID; } & M.ReduxProps)
  : JSX.Element {
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
    ptui.sendCommand(dispatch, cmd);
  }
}
const PlayerNote = M.connectRedux(playerNote);

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
