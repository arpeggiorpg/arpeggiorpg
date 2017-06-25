import * as React from "react";
import * as ReactDOM from "react-dom";

import * as M from './Model';
import * as T from './PTTypes';

export function renderPlayers(app: any, [id, currentScene, data]: [string, string, any]) {
  console.log("sorry elm");
}

export const Players = M.connectRedux(
  function Players({ ptui, dispatch }: M.ReduxProps): JSX.Element {
    const scene = ptui.focused_scene();
    const app = ptui.app;

    return <div>{
      Object.keys(app.players).map(pid => {
        const player = app.players[pid];
        const sceneButtons = [];
        if (player.scene) {
          sceneButtons.push(setSceneButton(pid, "Remove from Scene", undefined));
        }
        if (scene && player.scene !== scene.id) {
          sceneButtons.push(setSceneButton(pid, "Move to this scene", scene.id));
        }

        return <div key={pid}
          style={{ display: "flex", alignItems: "center", justifyContent: "space-between" }} >
          <div>{pid}</div>
          <div>
            <ul>
              {player.creatures.map(cid => {
                const creature = ptui.getCreature(cid);
                if (creature) {
                  return <li key={pid + "-" + cid}>{creature.name}</li>;
                }
              })}
            </ul>
          </div>
          {sceneButtons}
          <button onClick={() => console.log("Grant creatures plz")}>Grant creatures</button>
        </div>;
      })
    }</div>;

    function setSceneButton(player_id: T.PlayerID, text: string, scene_id: T.SceneID | undefined)
      : JSX.Element {
      return <button key={"set-" + player_id + scene_id}
        onClick={() => ptui.sendCommand(dispatch, { t: "SetPlayerScene", player_id, scene_id })} >
        {text}
      </button >;
    }
  });
