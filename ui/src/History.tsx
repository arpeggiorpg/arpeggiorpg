import { Map } from "immutable";
import * as React from "react";

import * as M from "./Model";
import * as A from "./Actions";
import * as T from "./PTTypes";

export function History(): JSX.Element {
  // const snapshots = M.useState(s => s.app.snapshots);
  const logs: T.GameLog[] = [];
  const creatures = M.useState((s) => s.getGame().creatures);
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
          <button
            className="material-icons"
            onClick={() =>
              console.error("NYI")
              // A.sendCommand({ Rollback: [log_index] })
            }
          >
            history
          </button>
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
  if (log === "StopCombat") return <div>Combat stopped.</div>;
  if ("SetActiveScene" in log) return <div>Set the active scene</div>;
  if ("RegisterPlayer" in log)
    return <div>Registered player {log.RegisterPlayer}</div>;
  if ("UnregisterPlayer" in log)
    return <div>Unregistered player {log.UnregisterPlayer}</div>;
  if ("GiveCreaturesToPlayer" in log)
    return <div>Granted creatures to {log.GiveCreaturesToPlayer[0]}</div>;
  if ("RemoveCreaturesFromPlayer" in log)
    return <div>Removed creatures from {log.RemoveCreaturesFromPlayer[0]}</div>;
  if ("SetPlayerScene" in log)
    return <div>Moved {log.SetPlayerScene[0]} to a scene</div>;
  if ("ChatFromGM" in log) return <div>&lt;GM&gt;&nbsp;{log.ChatFromGM}</div>;
  if ("ChatFromPlayer" in log)
    return (
      <div>
        &lt;{log.ChatFromPlayer[0]}&gt;&nbsp;{log.ChatFromPlayer[1]}
      </div>
    );
  if ("AttributeCheckResult" in log)
    return (
      <div>
        <div>Creature ID: {log.AttributeCheckResult.creature_id}</div>
        <div>Success? {log.AttributeCheckResult.success.toString()}</div>
      </div>
    );
  if ("CreateFolder" in log)
    return (
      <div>
        <div>Created Folder</div>
        <div>{T.folderPathToString(log.CreateFolder)}</div>
      </div>
    );
  if ("RenameFolder" in log)
    return (
      <div>
        Renamed Folder {T.folderPathToString(log.RenameFolder[0])} to{" "}
        {log.RenameFolder[1]}
      </div>
    );
  if ("DeleteFolderItem" in log)
    return (
      <div>
        Deleted folder item in {T.folderPathToString(log.DeleteFolderItem[0])}
      </div>
    );
  if ("MoveFolderItem" in log)
    return (
      <div>
        Moved folder item from {T.folderPathToString(log.MoveFolderItem[0])}
        to {T.folderPathToString(log.MoveFolderItem[2])}
      </div>
    );
  if ("CopyFolderItem" in log)
    return (
      <div>
        Copied folder item from{" "}
        {T.folderPathToString(log.CopyFolderItem.source)}
        to {T.folderPathToString(log.CopyFolderItem.dest)}
      </div>
    );
  if ("CreateItem" in log)
    return (
      <div>
        Created item {log.CreateItem[1].name} in{" "}
        {T.folderPathToString(log.CreateItem[0])}
      </div>
    );
  if ("EditItem" in log) return <div>Edited item {log.EditItem.name}</div>;
  if ("CreateNote" in log)
    return <div>Created note {log.CreateNote[1].name}</div>;
  if ("EditNote" in log) return <div>Edited note {log.EditNote[2].name}</div>;
  if ("TransferItem" in log) return <div>Transferred an item</div>;
  if ("RemoveItem" in log)
    return <div>Removed item from a creature's inventory</div>;
  if ("SetItemCount" in log)
    return <div>Set count on a creature's inventory</div>;
  if ("CreateScene" in log)
    return <div>Created scene {log.CreateScene[1].name}</div>;
  if ("EditSceneDetails" in log)
    return (
      <div>Edited details of scene {log.EditSceneDetails.details.name}</div>
    );
  if ("SetSceneCreatureVisibility" in log)
    return <div>Changed visibility of a creature in a scene</div>;
  if ("AddCreatureToScene" in log)
    return <div>Added a creature to a scene</div>;
  if ("RemoveCreatureFromScene" in log)
    return <div>Removed a creature from a scene</div>;
  if ("AddSceneChallenge" in log)
    return (
      <div>
        Added a challenge to a scene: {log.AddSceneChallenge.description}
      </div>
    );
  if ("RemoveSceneChallenge" in log)
    return (
      <div>
        Removed challenge from a scene: {log.RemoveSceneChallenge.description}
      </div>
    );
  if ("SetFocusedSceneCreatures" in log)
    return <div>Changed focused creatures in a scene</div>;
  if ("RemoveSceneVolumeCondition" in log)
    return <div>Removed a volume condition from a scene</div>;
  if ("EditSceneTerrain" in log) return <div>Edited a scene's terrain</div>;
  if ("EditSceneHighlights" in log)
    return <div>Edited a scene's highlights</div>;
  if ("EditSceneAnnotations" in log)
    return <div>Edited a scene's annotations</div>;
  if ("EditSceneRelatedScenes" in log)
    return <div>Edited a scene's related scenes</div>;
  if ("EditSceneSceneHotspots" in log)
    return <div>Edited a scene's hotspots that linked to scenes</div>;
  if ("SetCreaturePos" in log)
    return (
      <div>Set a creature position to {log.SetCreaturePos[2].toString()}</div>
    );
  if ("PathCreature" in log) {
    let last = log.PathCreature[2].at(-1);
    const msg = last ? last.toString() : "Nowhere";
    return <div>Creature followed a path to {msg}</div>;
  }
  if ("CreateCreature" in log)
    return <div>Created a creature {log.CreateCreature[1].name}</div>;
  if ("EditCreatureDetails" in log)
    return <div>Edited a creature {log.EditCreatureDetails.details.name}</div>;
  if ("StartCombat" in log) return <div>Started combat</div>;
  if ("AddCreatureToCombat" in log)
    return <div>Added a creature to combat</div>;
  if ("RemoveCreatureFromCombat" in log)
    return <div>Removed a creature from combat</div>;
  if ("CombatLog" in log) return combat_log(log.CombatLog);
  if ("StopCombat" in log) return <div>Combat stopped.</div>;
  if ("CreatureLog" in log)
    return creature_log(creatures, log.CreatureLog[0], log.CreatureLog[1]);
  if ("Rollback" in log)
    return (
      <div>
        Rolled back to {log.Rollback[0]}/{log.Rollback[1]}
      </div>
    );
  if ("LoadModule" in log)
    return <div>Loaded module {log.LoadModule.name}</div>;
}

function combat_log(log: T.CombatLog) {
  if (log === "ForceNextTurn")
    return <div>Forced move to next creature in combat</div>;
  if (log === "ForcePrevTurn")
    return <div>Forced move to previous creature in combat</div>;
  if ("ConsumeMovement" in log) return null;
  if ("ChangeCreatureInitiative" in log)
    return <div>Creature initiative changed</div>;
  if ("EndTurn" in log) return <div>Turn ended.</div>;
  if ("RerollInitiative" in log)
    return <div>Rerolled initiative for all creatures</div>;
  M.assertNever(log);
}

export function creature_log(
  creatures: Map<T.CreatureID, T.Creature>,
  creature_id: T.CreatureID,
  log: T.CreatureLog
): JSX.Element {
  const creature = creatures.get(creature_id);
  const creature_name = (
    <strong>{creature ? creature.name : "a creature"}</strong>
  );
  if ("Damage" in log)
    return (
      <div>
        {creature_name} took {log.Damage.hp} damage. Rolls:{" "}
        {JSON.stringify(log.Damage.rolls)}
      </div>
    );
  if ("Heal" in log)
    return (
      <div>
        {creature_name} was healed for {log.Heal.hp}. Rolls:{" "}
        {JSON.stringify(log.Heal.rolls)}
      </div>
    );
  if ("GenerateEnergy" in log)
    return (
      <div>
        {creature_name} received {log.GenerateEnergy} energy.
      </div>
    );
  if ("ReduceEnergy" in log)
    return (
      <div>
        {creature_name} lost {log.ReduceEnergy} energy.
      </div>
    );
  if ("ApplyCondition" in log)
    return <div>{creature_name} gained a condition</div>;
  if ("DecrementConditionRemaining" in log)
    return <div>{creature_name} ticked a condition.</div>;
  if ("RemoveCondition" in log)
    return <div>{creature_name} lost a condition.</div>;
  M.assertNever(log);
}
