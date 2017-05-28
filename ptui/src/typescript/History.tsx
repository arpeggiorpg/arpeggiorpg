import * as React from "react";
import * as ReactDOM from "react-dom";
import Flexbox from 'flexbox-react';

import * as PTTypes from './PTTypes';

export function renderHistory([id, data]: [string, Array<Array<[any, Array<any>]>>]) {
  console.log("Rendering History", id, data);
  ReactDOM.render(
    <History data={data} />,
    document.getElementById(id)
  );
}

class History extends React.Component<{ data: any }, any> {
  render(): JSX.Element {
  let snaps = PTTypes.decodeAppSnapshots.decodeAny(this.props.data);
    return <Flexbox flexDirection="column">{
      snaps.map(
        ({snapshot, logs}) =>
          logs.map((log, i) => <Flexbox key={i}>{this.gameLog(log)}</Flexbox>)
      )
    }</Flexbox>;
  }

  gameLog(log: PTTypes.GameLog): JSX.Element {
    switch (log.t) {
      case "AttributeCheckResult":
        return <Flexbox>
          <div>Creature ID: {log.cid}</div>
          <div>Success? {log.success.toString()}</div>
        </Flexbox>;
      case "CreateFolder":
        return <Flexbox><div>Created Folder</div><div>{log.path}</div></Flexbox>;
      case "StartCombat":
        return <Flexbox>Combat started. Combatants: <div>{log.creatures.toString()}</div></Flexbox>
      case "StopCombat":
        return <Flexbox>Combat stopped.</Flexbox>;
    }
  }
}

interface AttributeCheckResult { t: "AttributeCheckResult"; }

