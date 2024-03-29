import { Map } from "immutable";
import * as React from "react";

import * as A from "./Actions";
import * as M from "./Model";
import * as T from "./PTTypes";

export function History(): JSX.Element {
  const logs = M.useState(s => s.recentLogs).map(l => l[1]);
  const creatures = M.useState((s) => s.game.creatures);
  console.log("[EXPENSIVE:History.render]");
  return (
    <div>
      {logs.map((log: T.GameLog, log_index) => (
        <div
          style={{
            display: "flex",
            flexDirection: "row",
            justifyContent: "space-between",
          }}
          key={log_index.toString()}
        >
          <GameLog log={log} creatures={creatures} />
          {
            /* We need to reimplement Rollback. This used to be implemented at the "App" level,
            * which no longer exists. Rollback should not be a GameCommand, but rather an
            * RPIGameRequest variant.
            */
          }
          {
            /* <button
            className="material-icons"
            onClick={
              A.sendGMCommand({ Rollback: [log_index] })
            }
          >
            history
          </button> */
          }
        </div>
      ))}
    </div>
  );
}

export function GameLog(props: {
  log: T.GameLog;
  creatures: Map<T.CreatureID, T.Creature>;
}) {
  const { log, creatures } = props;
  switch (log.t) {
    case "SetActiveScene":
      return <div>Set the active scene</div>;
    case "RegisterPlayer": {
      return <div>Registered player {log.id}</div>;
    }
    case "UnregisterPlayer": {
      return <div>Unregistered player {log.id}</div>;
    }
    case "GiveCreaturesToPlayer": {
      return <div>Granted creatures to {log.player_id}</div>;
    }
    case "RemoveCreaturesFromPlayer": {
      return <div>Removed creatures from {log.player_id}</div>;
    }
    case "SetPlayerScene": {
      return <div>Moved {log.player_id} to a scene</div>;
    }
    case "ChatFromGM":
      return <div>&lt;GM&gt;&nbsp;{log.message}</div>;
    case "ChatFromPlayer": {
      return (
        <div>
          &lt;{log.player_id}&gt;&nbsp;{log.message}
        </div>
      );
    }
    case "AttributeCheckResult": {
      return (
        <div>
          <div>Creature ID: {log.creature_id}</div>
          <div>Success? {log.success.toString()}</div>
        </div>
      );
    }
    case "CreateFolder": {
      return (
        <div>
          <div>Created Folder</div>
          <div>{T.folderPathToString(log.path)}</div>
        </div>
      );
    }
    case "RenameFolder": {
      return (
        <div>
          Renamed Folder {T.folderPathToString(log.path)} to {log.new_name}
        </div>
      );
    }
    case "DeleteFolderItem": {
      return (
        <div>
          Deleted folder item in {T.folderPathToString(log.path)}
        </div>
      );
    }
    case "RenameFolderItem": {
      return (
        <div>
          Renamed folder item in {T.folderPathToString(log.path)} to {log.new_name}
        </div>
      );
    }
    case "MoveFolderItem": {
      return (
        <div>
          Moved folder item from {T.folderPathToString(log.source)}
          to {T.folderPathToString(log.destination)}
        </div>
      );
    }
    case "CopyFolderItem": {
      return (
        <div>
          Copied folder item from {T.folderPathToString(log.source)}
          to {T.folderPathToString(log.dest)}
        </div>
      );
    }
    case "CreateItem": {
      return (
        <div>
          Created item {log.item.name} in {T.folderPathToString(log.path)}
        </div>
      );
    }
    case "EditItem":
      return <div>Edited item {log.item.name}</div>;
    case "CreateNote": {
      return <div>Created note {log.note.name}</div>;
    }
    case "EditNote":
      return <div>Edited note {log.note.name}</div>;
    case "TransferItem":
      return <div>Transferred an item</div>;
    case "RemoveItem": {
      return <div>Removed item from a creature's inventory</div>;
    }
    case "SetItemCount": {
      return <div>Set count on a creature's inventory</div>;
    }
    case "CreateScene": {
      return <div>Created scene {log.scene.name}</div>;
    }
    case "EditSceneDetails": {
      return <div>Edited details of scene {log.details.name}</div>;
    }
    case "SetSceneCreatureVisibility": {
      return <div>Changed visibility of a creature in a scene</div>;
    }
    case "AddCreatureToScene": {
      return <div>Added a creature to a scene</div>;
    }
    case "RemoveCreatureFromScene": {
      return <div>Removed a creature from a scene</div>;
    }
    case "AddSceneChallenge": {
      return (
        <div>
          Added a challenge to a scene: {log.description}
        </div>
      );
    }
    case "RemoveSceneChallenge": {
      return (
        <div>
          Removed challenge from a scene: {log.description}
        </div>
      );
    }
    case "SetFocusedSceneCreatures": {
      return <div>Changed focused creatures in a scene</div>;
    }
    case "RemoveSceneVolumeCondition": {
      return <div>Removed a volume condition from a scene</div>;
    }
    case "EditSceneTerrain":
      return <div>Edited a scene's terrain</div>;
    case "EditSceneHighlights": {
      return <div>Edited a scene's highlights</div>;
    }
    case "EditSceneAnnotations": {
      return <div>Edited a scene's annotations</div>;
    }
    case "EditSceneRelatedScenes": {
      return <div>Edited a scene's related scenes</div>;
    }
    case "EditSceneSceneHotspots": {
      return <div>Edited a scene's hotspots that linked to scenes</div>;
    }
    case "SetCreaturePos": {
      return <div>Set a creature position to {log.pos.toString()}</div>;
    }
    case "PathCreature": {
      let last = log.path.at(-1);
      const msg = last ? last.toString() : "Nowhere";
      return <div>Creature followed a path to {msg}</div>;
    }
    case "CreateCreature": {
      return <div>Created a creature {log.creature.name}</div>;
    }
    case "EditCreatureDetails": {
      return <div>Edited a creature {log.details.name}</div>;
    }
    case "EditCreature": {
      return <div>Edited a creature {log.creature.name}</div>;
    }
    case "StartCombat":
      return <div>Started combat</div>;
    case "AddCreatureToCombat": {
      return <div>Added a creature to combat</div>;
    }
    case "RemoveCreatureFromCombat": {
      return <div>Removed a creature from combat</div>;
    }
    case "CombatLog":
      return combat_log(log.log);
    case "StopCombat":
      return <div>Combat stopped.</div>;
    case "CreatureLog": {
      return <CreatureLog creatureId={log.creature_id} log={log.log} />;
    }
    case "Rollback": {
      return (
        <div>
          Rolled back to {log.snapshot_index}/{log.log_index}
        </div>
      );
    }
    case "LoadModule": {
      return <div>Loaded module {log.name}</div>;
    }
    case "AddVolumeCondition": {
      return <div>Added volume condition</div>;
    }
    case "CreateClass": {
      return <div>Created class</div>;
    }
    case "EditClass": {
      return <div>Edited class</div>;
    }
    case "CreateAbility": {
      return <div>Created ability</div>;
    }
    case "EditAbility": {
      return <div>Edited ability</div>;
    }
  }
  M.assertNever(log);
}

function combat_log(log: T.CombatLog) {
  switch (log.t) {
    case "ForceNextTurn":
      return <div>Forced move to next creature in combat</div>;
    case "ForcePrevTurn": {
      return <div>Forced move to previous creature in combat</div>;
    }
    case "ConsumeMovement":
      return null;
    case "ChangeCreatureInitiative": {
      return <div>Creature initiative changed</div>;
    }
    case "EndTurn":
      return <div>Turn ended.</div>;
    case "RerollInitiative": {
      return <div>Rerolled initiative for all creatures</div>;
    }
  }
  M.assertNever(log);
}

export function CreatureLog(props: { creatureId: T.CreatureID; log: T.CreatureLog }) {
  const { creatureId, log } = props;
  const creature = M.useState(s => s.getCreature(creatureId));
  const creature_name = <strong>{creature ? creature.name : "a creature"}</strong>;
  switch (log.t) {
    case "Damage": {
      return (
        <div>
          {creature_name} took {log.hp} damage. Rolls: {JSON.stringify(log.rolls)}
        </div>
      );
    }
    case "Heal": {
      return (
        <div>
          {creature_name} was healed for {log.hp}. Rolls: {JSON.stringify(log.rolls)}
        </div>
      );
    }
    case "GenerateEnergy": {
      return (
        <div>
          {creature_name} received {log.energy} energy.
        </div>
      );
    }
    case "ReduceEnergy": {
      return (
        <div>
          {creature_name} lost {log.energy} energy.
        </div>
      );
    }
    case "ApplyCondition": {
      return <div>{creature_name} gained a condition</div>;
    }
    case "DecrementConditionRemaining": {
      return <div>{creature_name} ticked a condition.</div>;
    }
    case "RemoveCondition": {
      return <div>{creature_name} lost a condition.</div>;
    }
  }
  M.assertNever(log);
}
