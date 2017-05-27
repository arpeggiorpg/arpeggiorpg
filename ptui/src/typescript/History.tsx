import * as React from "react";
import * as ReactDOM from "react-dom";

import * as PTTypes from './PTTypes';

export function renderHistory([id, data]: [string, Array<Array<[any, Array<any>]>>]) {
  ReactDOM.render(
    <History data={data} />,
    document.getElementById(id)
  );
}

class History extends React.Component<{ data: any }, any> {
  render(): JSX.Element {
    return <ul>{
      this.props.data[0][1].map(
        (l: any, i: number) => <li key={i}>{this.gameLog(PTTypes.decodeGameLog(l))}</li>
      )
    }</ul>;
  }

  gameLog(log: PTTypes.GameLog): JSX.Element {
    switch (log.t) {
      case "AttributeCheckResult": return <div>Creature ID: {log.cid}<br />Success? {log.success.toString()}</div>;
      case "CreateFolder": return <div>Created Folder {log.path}</div>
    }
  }
}

interface AttributeCheckResult { t: "AttributeCheckResult"; }
