import * as React from "react";
import * as ReactDOM from "react-dom";

import { Button, List, Table } from 'semantic-ui-react';

import * as M from './Model';
import * as T from './PTTypes';

export function renderPlayers(app: any, [id, currentScene, data]: [string, string, any]) {
  console.log("sorry elm");
}

export const Players = M.connectRedux(
  function Players({ ptui, dispatch }: M.ReduxProps): JSX.Element {
    const scene = ptui.focused_scene();
    const app = ptui.app;

    return <Table celled={true}>
      <Table.Header>
        <Table.Row>
          <Table.HeaderCell>Player</Table.HeaderCell>
          <Table.HeaderCell>Creatures</Table.HeaderCell>
          <Table.HeaderCell>Actions</Table.HeaderCell>
        </Table.Row>
      </Table.Header>
      <Table.Body>
        {
          app.players.entrySeq().toArray().map(([pid, player]) => {
            const sceneButtons = [];
            if (player.scene) {
              sceneButtons.push(setSceneButton(pid, "Remove from Scene", undefined));
            }
            if (scene && player.scene !== scene.id) {
              sceneButtons.push(setSceneButton(pid, "Move to this scene", scene.id));
            }

            return <Table.Row key={pid}>
              <Table.Cell>{pid}</Table.Cell>
              <Table.Cell>
                <List>
                  {player.creatures.map(cid => {
                    const creature = ptui.getCreature(cid);
                    if (creature) {
                      return <List.Item key={pid + "-" + cid}>{creature.name}</List.Item>;
                    }
                  })}
                </List>
              </Table.Cell>
              <Table.Cell>
                <Button.Group vertical={true}>
                  {sceneButtons}
                  <Button onClick={() => console.log("Grant creatures plz")}>Grant creatures</Button>
                </Button.Group>
              </Table.Cell>
            </Table.Row>;
          })
        }
      </Table.Body>
    </Table>;

    function setSceneButton(player_id: T.PlayerID, text: string, scene_id: T.SceneID | undefined)
      : JSX.Element {
      return <Button key={"set-" + player_id + scene_id}
        onClick={() => ptui.sendCommand(dispatch, { t: "SetPlayerScene", player_id, scene_id })} >
        {text}
      </Button >;
    }
  });
