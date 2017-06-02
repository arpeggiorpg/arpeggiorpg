import * as React from "react";
import * as ReactDOM from "react-dom";
import Flexbox from 'flexbox-react';

import * as PTTypes from './PTTypes';

export function renderPlayers(app: any, [id, data]: [string, any]) {
  console.log("Rendering Players", id, data);
  let onRemoveFromScene = (pid: string) => app.ports.playersRemoveFromScene.send(pid);
  ReactDOM.render(
    <Players data={data} onRemoveFromScene={onRemoveFromScene} />,
    document.getElementById(id)
  );
}

class Players extends React.Component<{ data: any, onRemoveFromScene: (pid: string) => void }, any> {
  render(): JSX.Element {
    let players = PTTypes.decodeAppPlayers.decodeAny(this.props.data);
    return <Flexbox flexDirection="column">
      Foo
    </Flexbox>;
  }

}
