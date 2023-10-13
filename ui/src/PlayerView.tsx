import { Map } from 'immutable';
import * as React from "react";

import * as CV from "./CommonView";
import * as Grid from './Grid';
import * as M from './Model';
import * as A from './Actions';
import * as T from './PTTypes';


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
  return <CV.TheLayout
    map={map}
    bottom_right={<></>}// <CV.PlayerChat player_id={player.player_id} />}
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
      if (scene.creatures.get(mapc.creature.id)![1] === "AllPlayers") {
        const actions = creatureMenuActions(state, player, mapc.creature);
        return { ...mapc, actions: mapc.actions.concat(actions) };
      }
    }
  );
}

function creatureMenuActions(state: M.AllStates, player: T.Player, creature: T.Creature): Grid.MapCreature["actions"] {
  // Well, this function is *definitely* not returning identity-stable or even equatable return
  // values, so this is going to force re-renders constantly.
  const actions = [];
  const combat = state.getCombat();
  if (combat) {
    if (state.getCurrentCombatCreatureID() === creature.id) {
      actions.push({actionName: "Combat Move", action: () => A.requestCombatMovement()});
    }
  } else if (player.creatures.includes(creature.id)) {
    actions.push({actionName: "Walk", action: A.requestMove});
  }
  return actions;
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
  const folder = M.useState(s => s.getFolder(path));
  if (!folder) {
    return <div>Sorry, you don't have a player folder right now (TODO: auto-create Player folders)</div>;
  }
  // React.useEffect(() => {
  //   if (!folder) {
  //     A.sendGMCommand({CreateFolder: path});
  //   }
  // }, []);
  return <CV.NoteEditor path={path} name="Scratch" disallow_rename={true} />;
}

function PlayerActionBar(props: { player: T.Player; combat: T.Combat | null }) {
  const creature = M.useState(s => {
    const cid = s.getCurrentCombatCreatureID();
    if (cid) return s.getCreature(cid);
  });
  if (props.combat) {
    if (creature) {
      if (props.player.creatures.includes(creature.id)) {
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
