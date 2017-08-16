import Flexbox from 'flexbox-react';
import * as I from 'immutable';
import * as React from "react";

import * as Comp from './Component';
import * as M from './Model';
import * as T from './PTTypes';

export const History = Comp.connect(
  Comp.createDeepEqualSelector([ptui => ptui.app.snapshots, ptui => ptui.app.current_game.creatures],
    (snapshots, creatures) => ({ snapshots, creatures }))
)(function History(props): JSX.Element {
  const { snapshots, creatures, dispatch } = props;
  console.log("[EXPENSIVE:History.render]");
  return <div>{
    snapshots.map(
      ({ logs }, snapshot_index) =>
        logs.map((log: T.GameLog, log_index) =>
          <div style={{ display: "flex", flexDirection: "row", justifyContent: "space-between" }}
            key={snapshot_index.toString() + "-" + log_index.toString()}>
            <GameLog log={log} creatures={creatures} />
            <button className="material-icons"
              onClick={() => dispatch(M.sendCommand({ t: "Rollback", snapshot_index, log_index }))}
            >history</button>
          </div>)
    )
  }</div>;
});


export function GameLog(props: { log: T.GameLog, creatures: I.Map<T.CreatureID, T.Creature> }):
  JSX.Element | null {
  const { log, creatures } = props;
  switch (log.t) {
    case "ChatFromGM":
      return <Flexbox>&lt;GM&gt;&nbsp;{log.message}</Flexbox>;
    case "ChatFromPlayer":
      return <Flexbox>&lt;{log.player_id}&gt;&nbsp;{log.message}</Flexbox>;
    case "AttributeCheckResult":
      return <Flexbox>
        <div>Creature ID: {log.cid}</div>
        <div>Success? {log.success.toString()}</div>
      </Flexbox>;
    case "CreateFolder":
      return <Flexbox><div>Created Folder</div><div>{M.folderPathToString(log.path)}</div></Flexbox>;
    case "RenameFolder":
      return <Flexbox>Renamed Folder</Flexbox>;
    case "DeleteFolderItem":
      return <Flexbox>Deleted folder item in {M.folderPathToString(log.path)}</Flexbox>;
    case "MoveFolderItem":
      return <Flexbox>Moved folder item from {M.folderPathToString(log.path)}
        to {M.folderPathToString(log.newPath)}</Flexbox>;
    case "CopyFolderItem":
      return <Flexbox>Copied folder item from {M.folderPathToString(log.source)}
        to {M.folderPathToString(log.dest)}</Flexbox>;
    case "CreateItem":
      return <Flexbox>Created item {log.item.name} in {M.folderPathToString(log.path)}</Flexbox>;
    case "EditItem":
      return <Flexbox>Edited item {log.item.name}</Flexbox>;
    case "CreateNote":
      return <Flexbox>Created note {log.note.name}</Flexbox>;
    case "EditNote":
      return <Flexbox>Edited note {log.name}</Flexbox>;
    case "TransferItem":
      return <Flexbox>Transferred an item</Flexbox>;
    case "RemoveItem":
      return <Flexbox>Removed item from a creature's inventory</Flexbox>;
    case "SetItemCount":
      return <Flexbox>Set count on a creature's inventory</Flexbox>;
    case "CreateScene":
      return <Flexbox>Created scene {log.scene.name}</Flexbox>;
    case "EditSceneDetails":
      return <Flexbox>Edited details of scene {log.details.name}</Flexbox>;
    case "SetSceneCreatureVisibility":
      return <Flexbox>Changed visibility of a creature in a scene</Flexbox>;
    case "AddCreatureToScene":
      return <Flexbox>Added a creature to a scene</Flexbox>;
    case "RemoveCreatureFromScene":
      return <Flexbox>Removed a creature from a scene</Flexbox>;
    case "AddSceneChallenge":
      return <Flexbox>Added a challenge to a scene: {log.description}</Flexbox>;
    case "RemoveSceneChallenge":
      return <Flexbox>Removed challenge from a scene: {log.description}</Flexbox>;
    case "SetFocusedSceneCreatures":
      return <Flexbox>Changed focused creatures in a scene</Flexbox>;
    case "CreateMap":
      return <Flexbox>Created a map {log.map.name}</Flexbox>;
    case "EditMap":
      return <Flexbox>Edited a map {log.map.name}</Flexbox>;
    case "EditMapDetails":
      return <Flexbox>Edited map details {log.details.name}</Flexbox>;
    case "EditMapTerrain":
      return <Flexbox>Edited a map's terrain</Flexbox>;
    case "SetCreaturePos":
      return <Flexbox>Set a creature position to {log.pos.toString()}</Flexbox>;
    case "PathCreature":
      const last = log.path.length > 0
        ? log.path[log.path.length - 1].toString()
        : "Nowhere";
      return <Flexbox>Creature followed a path to {last}</Flexbox>;
    case "CreateCreature":
      return <Flexbox>Created a creature {log.creature.name}</Flexbox>;
    case "EditCreatureDetails":
      return <Flexbox>Edited a creature {log.details.name}</Flexbox>;
    case "StartCombat":
      return <Flexbox>Started combat</Flexbox>;
    case "AddCreatureToCombat":
      return <Flexbox>Added a creature to combat</Flexbox>;
    case "RemoveCreatureFromCombat":
      return <Flexbox>Removed a creature from combat</Flexbox>;
    case "CombatLog":
      return combat_log(log.log);
    case "StopCombat":
      return <Flexbox>Combat stopped.</Flexbox>;
    case "CreatureLog":
      return creature_log(creatures, log.creature_id, log.log);
    case "Rollback":
      return <Flexbox>Rolled back to {log.snapshot_index}/{log.log_index}</Flexbox>;
  }
}


function combat_log(log: T.CombatLog): JSX.Element | null {
  switch (log.t) {
    case "ConsumeMovement":
      return null;
    case "ChangeCreatureInitiative":
      return <Flexbox>Creature initiative changed</Flexbox>;
    case "EndTurn":
      return <Flexbox>Turn ended.</Flexbox>;
    case "ForceNextTurn":
      return <Flexbox>Forced move to next creature in combat</Flexbox>;
    case "ForcePrevTurn":
      return <Flexbox>Forced move to previous creature in combat</Flexbox>;
    case "RerollInitiative":
      return <Flexbox>Rerolled initiative for all creatures</Flexbox>;
  }
}

export function creature_log(
  creatures: I.Map<T.CreatureID, T.Creature>,
  creature_id: T.CreatureID,
  log: T.CreatureLog): JSX.Element {
  const creature = creatures.get(creature_id);
  const creature_name = <strong>{creature ? creature.name : "a creature"}</strong>;
  switch (log.t) {
    case "Damage":
      return <div>{creature_name} took {log.hp} damage. Rolls: {JSON.stringify(log.rolls)}</div>;
    case "Heal":
      return <div>
        {creature_name} was healed for {log.hp}. Rolls: {JSON.stringify(log.rolls)}
      </div>;
    case "GenerateEnergy":
      return <div>{creature_name} received {log.energy} energy.</div>;
    case "ReduceEnergy":
      return <div>{creature_name} lost {log.energy} energy.</div>;
    case "ApplyCondition":
      return <div>{creature_name} gained a condition</div>;
    case "DecrementConditionRemaining":
      return <div>{creature_name} ticked a condition.</div>;
    case "RemoveCondition":
      return <div>{creature_name} lost a condition.</div>;
  }
}
