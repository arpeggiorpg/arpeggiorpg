import * as React from "react";
import * as ReactDOM from "react-dom";
import Flexbox from 'flexbox-react';

import * as T from './PTTypes';

export function renderHistory(app: any, [id, data]: [string, Array<Array<[any, Array<any>]>>]) {
  let onRollback = (si: number, li: number) => app.ports.historyRollback.send([si, li]);
  ReactDOM.render(
    <History data={data} onRollback={onRollback} />,
    document.getElementById(id)
  );
}

class History extends React.Component<{ data: any, onRollback: (snapshot_index: number, log_index: number) => void }, any> {
  render(): JSX.Element {
    let app = T.decodeApp.decodeAny(this.props.data);
    console.log("rendering history. successfully decoded app.", app.snapshots);
    return <Flexbox flexDirection="column">{
      app.snapshots.map(
        ({ snapshot, logs }, snapshot_index) =>
          logs.map((log: T.GameLog, log_index) =>
            <Flexbox key={snapshot_index.toString() + "-" + log_index.toString()}
                     justifyContent="space-between">
              {this.gameLog(log)}
              <button className="material-icons"
                      onClick={() => this.props.onRollback(snapshot_index, log_index)}
                      >history</button>
            </Flexbox>)
      )
    }</Flexbox>;
  }

  gameLog(log: T.GameLog): JSX.Element | null {
    switch (log.t) {
      case "AttributeCheckResult":
        return <Flexbox>
          <div>Creature ID: {log.cid}</div>
          <div>Success? {log.success.toString()}</div>
        </Flexbox>;
      case "CreateFolder":
        return <Flexbox><div>Created Folder</div><div>{log.path}</div></Flexbox>;
      case "RenameFolder":
        return <Flexbox>Renamed Folder</Flexbox>
      case "DeleteFolder":
        return <Flexbox>Deleted folder {log.path}</Flexbox>
      case "DeleteFolderItem":
        return <Flexbox>Deleted folder item in {log.path}</Flexbox>
      case "MoveFolderItem":
        return <Flexbox>Moved folder item from {log.path} to {log.newPath}</Flexbox>
      case "CreateItem":
        return <Flexbox>Created item {log.item.name} in {log.path}</Flexbox>
      case "EditItem":
        return <Flexbox>Edited item {log.item.name}</Flexbox>
      case "CreateNote":
        return <Flexbox>Created note {log.note.name}</Flexbox>
      case "EditNote":
        return <Flexbox>Edited note {log.name}</Flexbox>
      case "DeleteNote":
        return <Flexbox>Deleted note {log.name}</Flexbox>
      case "CreateScene":
        return <Flexbox>Created scene {log.scene.name}</Flexbox>
      case "EditScene":
        return <Flexbox>Edited scene {log.scene.name}</Flexbox>
      case "DeleteScene":
        return <Flexbox>Deleted a scene</Flexbox>
      case "CreateMap":
        return <Flexbox>Created a map {log.map.name}</Flexbox>
      case "EditMap":
        return <Flexbox>Edited a map {log.map.name}</Flexbox>
      case "DeleteMap":
        return <Flexbox>Deleted a map</Flexbox>
      case "SetCreaturePos":
        return <Flexbox>Set a creature position to {log.pos.toString()}</Flexbox>
      case "PathCreature":
        let last;
        if (log.path.length > 0) {
          last = log.path[log.path.length - 1].toString();
        } else { last = "Nowhere" }
        return <Flexbox>Creature followed a path to {last}</Flexbox>
      case "CreateCreature":
        return <Flexbox>Created a creature {log.creature.name}</Flexbox>
      case "EditCreature":
        return <Flexbox>Edited a creature {log.creature.name}</Flexbox>
      case "DeleteCreature":
        return <Flexbox>Deleted a creature</Flexbox>
      case "StartCombat":
        return <Flexbox>Started combat</Flexbox>
      case "AddCreatureToCombat":
        return <Flexbox>Added a creature to combat</Flexbox>
      case "RemoveCreatureFromCombat":
        return <Flexbox>Removed a creature from combat</Flexbox>
      case "CombatLog":
        return combat_log(log.log)
      case "StopCombat":
        return <Flexbox>Combat stopped.</Flexbox>;
      case "CreatureLog":
        return creature_log(log.log)
      case "Rollback":
        return <Flexbox>Rolled back to {log.snapshot_index}/{log.log_index}</Flexbox>
    }
  }
}

function combat_log(log: T.CombatLog): JSX.Element | null {
  switch (log.t) {
    case "ConsumeMovement":
      return null;
    case "ChangeCreatureInitiative":
      return <Flexbox>Creature initiative changed</Flexbox>
    case "EndTurn":
      return <Flexbox>Creature's turn ended.</Flexbox>
    case "ForceNextTurn":
      return <Flexbox>Forced move to next creature in combat</Flexbox>
    case "ForcePrevTurn":
      return <Flexbox>Forced move to previous creature in combat</Flexbox>
    case "RerollInitiative":
      return <Flexbox>Rerolled initiative for all creatures</Flexbox>
  }
}

function creature_log(log: T.CreatureLog): JSX.Element | null {
  switch (log.t) {
    case "Damage":
      return <Flexbox>A creature took {log.hp} damage. Rolls: {JSON.stringify(log.rolls)}</Flexbox>
    case "Heal":
      return <Flexbox>A creature was healed for {log.hp}. Rolls: {JSON.stringify(log.rolls)}</Flexbox>
    case "GenerateEnergy":
      return <Flexbox>A creature received {log.energy} energy.</Flexbox>
    case "ReduceEnergy":
      return <Flexbox>A creature's energy was reduced by {log.energy}</Flexbox>
    case "ApplyCondition":
      return <Flexbox>A creature gained a condition</Flexbox>
    case "DecrementConditionRemaining":
      return <Flexbox>A condition was reduced in duration.</Flexbox>
    case "RemoveCondition":
      return <Flexbox>A condition was removed from a creature.</Flexbox>
  }
}
