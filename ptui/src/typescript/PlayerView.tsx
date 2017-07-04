import * as I from 'immutable';
import * as LD from "lodash";
import * as React from "react";
import * as ReactDOM from "react-dom";
import { Provider } from 'react-redux';
import * as Redux from 'redux';

import * as CV from "./CommonView";
import * as Grid from './Grid';
import { PTUI } from './Model';
import * as M from './Model';
import * as T from './PTTypes';

import { Button, Input, Menu } from 'semantic-ui-react';


export class PlayerMainComp extends React.Component<M.ReduxProps, { typing_player_id: string; }> {
  constructor(props: M.ReduxProps) {
    super(props);
    this.state = { typing_player_id: "" };
  }

  render(): JSX.Element {
    const { ptui, dispatch } = this.props;
    if (ptui.state.player_id) {
      const player = ptui.app.players.get(ptui.state.player_id);
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
          {ptui.app.players.count() > 0
            ? <div>
              <p>You can rejoin a session if you've already registered as a player.</p>
              <Menu compact={true}>
                {ptui.app.players.keySeq().toArray().map(pid =>
                  <Menu.Item key={pid}
                    onClick={() => dispatch({ type: "SetPlayerID", pid })}
                    name={pid} />)
                }
              </Menu>
            </div>
            : null}
          <p>Or you can register a new player.</p>
          <Input type="text" value={this.state.typing_player_id}
            onKeyDown={(e: KeyboardEvent) => {
              if (e.keyCode === 13) { this.registerPlayer(ptui, dispatch); }
            }}
            action={<Button
              type="submit"
              onClick={() => this.registerPlayer(ptui, dispatch)}>
              Register</Button>}
            onChange={e => this.setState({ typing_player_id: e.currentTarget.value })} />
        </div>
      </div>;
    }
  }
  registerPlayer(ptui: M.PTUI, dispatch: M.Dispatch) {
    ptui.sendCommand(dispatch,
      { t: "RegisterPlayer", player_id: this.state.typing_player_id });
    dispatch({ type: "SetPlayerID", pid: this.state.typing_player_id });
  }
}
export const PlayerMain = M.connectRedux(PlayerMainComp);

export const PlayerGameView = M.connectRedux((
  { player, ptui, dispatch }: { player: T.Player; } & M.ReduxProps): JSX.Element => {
  const scene = player.scene ? M.get(ptui.app.current_game.scenes, player.scene) : undefined;
  const map = scene
    ? <Grid.SceneGrid scene={scene} creatures={selectMapCreatures(ptui, player, scene, dispatch)} />
    : <div>No scene loaded</div>;
  const combat = ptui.app.current_game.current_combat;
  const tabs = [
    <CV.Tab key="Creatures" name="Creatures">
      <PlayerCreatures player={player} />
    </CV.Tab>,
    <CV.Tab key="Combat" name="Combat">
      <div>
        {combat ? <CV.Combat combat={combat} /> : <div>There is no combat.</div>}
      </div>
    </CV.Tab>,
    <CV.Tab key="Notes" name="Notes">
      <PlayerNote player_id={player.player_id} />
    </CV.Tab>
  ];
  return <CV.TheLayout map={map} tabs={tabs} bar_width={325} menu_size="large"
    top_bar={<CV.TopBar />} bottom_bar={<PlayerActionBar player={player} combat={combat} />} />;
});


/**
 * Figure out which creatures to display, and create [[Grid.MapCreature]] for each of them.
 * This involves figuring out what kind of actions this player can perform on each creature, such as
 * moving them (if the player has control of them), or targeting them for an ability that has been
 * selected.
 */
function selectMapCreatures(
  ptui: M.PTUI, player: T.Player, scene: T.Scene, dispatch: M.Dispatch)
  : { [index: string]: Grid.MapCreature } {
  return M.filterMapValues(Grid.mapCreatures(ptui, dispatch, scene),
    mapc => {
      // !: must exist in filterMapValues()
      if (scene.creatures.get(mapc.creature.id)![1].t === "AllPlayers") {
        const actions = creatureMenuActions(ptui, dispatch, player, mapc.creature);
        return { ...mapc, actions: mapc.actions.merge(actions) };
      }
    }
  );
}

function creatureMenuActions(
  ptui: M.PTUI, dispatch: M.Dispatch, player: T.Player, creature: T.Creature) {
  let actions: I.Map<string, (cid: T.CreatureID) => void> = I.Map();
  const move = moveAction();
  if (move) {
    actions = actions.set("Move this creature", move);
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
}

const PlayerCreatures = M.connectRedux((props: { player: T.Player } & M.ReduxProps): JSX.Element => {
  const cids = props.player.creatures;
  const creatures = props.ptui.getCreatures(cids);
  if (creatures.length === 0) {
    return <div>You have no creatures in your control yet.</div>;
  }
  return <div>
    {creatures.map(creature =>
      <div key={creature.id}>
        <CV.CreatureCard creature={creature} />
        <div style={{ marginLeft: "1em" }}>
          <CV.CollapsibleInventory creature={creature} />
        </div>
      </div>
    )}
  </div>;
});

function PlayerNote({ player_id }: { player_id: T.PlayerID; }): JSX.Element {
  const path = ["Players", player_id];
  return <CV.NoteEditor path={path} name="Scratch" disallow_rename={true} />;
}

export const PlayerActionBar = M.connectRedux((
  props: { player: T.Player; combat: T.Combat | undefined } & M.ReduxProps): JSX.Element => {
  if (props.combat) {
    const cid = props.ptui.getCurrentCombatCreatureID(props.combat);
    const creature = props.ptui.getCreature(cid);
    if (creature) {
      if (LD.includes(props.player.creatures, cid)) {
        return <CV.ActionBar combat={props.combat} creature={creature} />;
      } else {
        return <div>{creature.name} is acting</div>;
      }
    } else {
      return <div>Creature disappeared!</div>;
    }
  } else {
    return <div>TODO: out-of-combat actions</div>;
  }
});
