import * as I from 'immutable';
import * as LD from "lodash";
import * as React from "react";

import * as CV from "./CommonView";
import * as Grid from './Grid';
import * as M from './Model';
import * as T from './PTTypes';

import { Menu } from 'semantic-ui-react';


export function PlayerMain() {
  const playerId = M.useState(s => s.playerId);
  if (playerId) {
    return <PlayerGameView playerId={playerId} />;
  } else {
    return <PlayerLogin />
  }
}

function PlayerLogin() {
  const players = M.useState(s => s.getGame().players);
  return <div style={{ display: "flex", justifyContent: "space-around" }}>
    <div style={{ width: "600px" }}>
      <h1>P&T</h1>
      <p>Welcome to P&T!</p>
      {players.count() > 0
        ? <div>
          <p>You can rejoin a session if you've already registered as a player.</p>
          <Menu compact={true}>
            {players.keySeq().toArray().map(pid =>
              <Menu.Item key={pid}
                onClick={() => M.getState().setPlayerId(pid)}
                name={pid} />)
            }
          </Menu>
        </div>
        : <div>There are no players yet!</div>}
      <p>You can register a new player.</p>
      <CV.SingleInputForm buttonText="Register"
        onSubmit={input => registerPlayer(input)} />
    </div>
  </div>;

  function registerPlayer(name: string) {
    M.sendCommand({ t: "RegisterPlayer", player_id: name });
    M.getState().setPlayerId(name);
  }
}

export function PlayerGameView({ playerId }: { playerId: T.PlayerID }) {
  const {player, scene, mapCreatures} = M.useState(s => {
    const player = s.getGame().players.get(playerId);
    const scene = player?.scene ? s.getScene(player.scene) : undefined;
    const mapCreatures = player?.scene && scene ? selectMapCreatures(s, player, scene) : {};
    return {player, scene, mapCreatures};
  });
  const combat = M.useState(s => s.getCombat());
  if (!player) {
    return <div>Player {playerId} not found</div>;
  }
  const map = scene
    ? <Grid.SceneGrid scene={scene} creatures={mapCreatures} />
    : <div>No scene loaded</div>;
  const tabs = [
    <CV.Tab key="Creatures" name="Creatures">
      <PlayerCreatures player={player} />
    </CV.Tab>,
    <CV.Tab key="Notes" name="Notes">
      <PlayerNote player_id={player.player_id} />
    </CV.Tab>
  ];
  return <CV.TheLayout map={map} bottom_right={<CV.PlayerChat player_id={player.player_id} />}
    tabs={tabs} bar_width={325} menu_size="large"
    bottom_bar={<PlayerActionBar player={player} combat={combat} />} />;
}


/**
 * Figure out which creatures to display, and create [[Grid.MapCreature]] for each of them.
 * This involves figuring out what kind of actions this player can perform on each creature, such as
 * moving them (if the player has control of them), or targeting them for an ability that has been
 * selected.
 */
function selectMapCreatures(state: M.AllStates, player: T.Player, scene: T.Scene): { [index: string]: Grid.MapCreature } {
  return M.filterMapValues(Grid.mapCreatures(state, scene),
    mapc => {
      // !: must exist in filterMapValues()
      if (scene.creatures.get(mapc.creature.id)![1].t === "AllPlayers") {
        const actions = creatureMenuActions(state, player, mapc.creature);
        return { ...mapc, actions: mapc.actions.merge(actions) };
      }
    }
  );
}

function creatureMenuActions(state: M.AllStates, player: T.Player, creature: T.Creature) {
  let actions: I.Map<string, (cid: T.CreatureID) => void> = I.Map();
  const combat = state.getCombat();
  const move = moveAction();
  if (move) {
    actions = actions.set("Walk", move);
  }
  return actions;

  function moveAction(): ((cid: T.CreatureID) => void) | undefined {
    if (!LD.includes(player.creatures, creature.id)) { return undefined; }
    if (combat) {
      if (state.getCurrentCombatCreatureID() === creature.id) {
        return _ => M.requestCombatMovement();
      } else {
        return undefined;
      }
    } else {
      return cid => M.requestMove(cid);
    }
  }
}

function PlayerCreatures(props: { player: T.Player }) {
  const cids = props.player.creatures;
  const creatures = M.useState(s => s.getCreatures(cids));
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
}

function PlayerNote({ player_id }: { player_id: T.PlayerID }): JSX.Element {
  const path = ["Players", player_id];
  return <CV.NoteEditor path={path} name="Scratch" disallow_rename={true} />;
}

function PlayerActionBar(props: { player: T.Player; combat: T.Combat | undefined }) {
  const cid = M.useState(s => s.getCurrentCombatCreatureID());
  const creature = M.useState(s => cid ? s.getCreature(cid) : undefined);
  if (props.combat) {
    if (creature) {
      if (LD.includes(props.player.creatures, cid)) {
        return <CV.ActionBar combat={props.combat} creatureId={creature.id} />;
      } else {
        return <div>{creature.name} is acting</div>;
      }
    } else {
      return <div>Creature disappeared!</div>;
    }
  } else {
    return <div>TODO: out-of-combat actions</div>;
  }
}
