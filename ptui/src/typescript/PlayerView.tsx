import * as React from "react";
import * as ReactDOM from "react-dom";
import Flexbox from 'flexbox-react';

import * as T from './PTTypes';

export function renderPlayerUI(elmApp: any, [id, playerID, currentScene, data]: [string, T.PlayerID, T.SceneID | undefined, any]) {
  let element = document.getElementById(id);
  console.log("[renderPlayerUI] Rendering Player component", id, element, playerID, currentScene);
  let app = T.decodeApp.decodeAny(data);
  ReactDOM.render(
    <PlayerUI app={app} playerID={playerID} currentScene={currentScene} />,
    element
  );
}

interface PlayerUIProps {
  playerID: T.PlayerID;
  currentScene: string | undefined;
  app: T.App;
};

class PlayerUI extends React.Component<PlayerUIProps, undefined> {

  render(): JSX.Element {
    return <div style={{display: "flex", flexDirection: "column"}}>
      Player: {this.props.playerID}
    </div>;
  }
}
