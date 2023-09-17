import * as I from "immutable";
import * as React from "react";

import { Button, List, Table } from "semantic-ui-react";

import * as Campaign from "./Campaign";
import * as CV from "./CommonView";
import * as M from "./Model";
import * as T from "./PTTypes";


export function Players() {
  const players = M.useState((s) => s.getGame().players);
  const gm_scene = M.useState((s) => s.getFocusedScene());
  const player_creatures = M.useState((s) =>
    players.valueSeq().toArray()
      .map((player) => {
        const scene = player.scene ? s.getScene(player.scene) : undefined;
        return {
          player,
          id: player.player_id,
          creatures: M.filterMap(player.creatures, s.getCreature),
          scene_name: scene ? scene.name : "No scene",
        };
      })
  );
  return (
    <Table celled={true}>
      <Table.Header>
        <Table.Row>
          <Table.HeaderCell>Player</Table.HeaderCell>
          <Table.HeaderCell>Creatures</Table.HeaderCell>
          <Table.HeaderCell>Scene</Table.HeaderCell>
          <Table.HeaderCell>Actions</Table.HeaderCell>
        </Table.Row>
      </Table.Header>
      <Table.Body>
        {player_creatures.map((playa) => {
          const sceneButtons = [];
          if (playa.player.scene) {
            sceneButtons.push(
              setSceneButton(playa.id, "Remove from Scene", undefined)
            );
          }
          if (gm_scene && playa.player.scene !== gm_scene.id) {
            sceneButtons.push(
              setSceneButton(playa.id, "Move to this scene", gm_scene.id)
            );
          }

          return (
            <Table.Row key={playa.id}>
              <Table.Cell>{playa.id}</Table.Cell>
              <Table.Cell>
                <List>
                  {playa.creatures.map((creature) => {
                    return (
                      <List.Item key={playa.id + "-" + creature.id}>
                        {creature.name}
                      </List.Item>
                    );
                  })}
                </List>
              </Table.Cell>
              <Table.Cell>{playa.scene_name}</Table.Cell>
              <Table.Cell>
                <Button.Group vertical={true}>
                  {sceneButtons}
                  <CV.ModalMaker
                    button={(open) => (
                      <Button onClick={open}>Grant creatures</Button>
                    )}
                    header={<span>Grant creatures to {playa.id}</span>}
                    content={(close) => (
                      <GrantCreaturesToPlayer
                        player={playa.player}
                        onDone={close}
                      />
                    )}
                  />
                </Button.Group>
              </Table.Cell>
            </Table.Row>
          );
        })}
      </Table.Body>
    </Table>
  );

  function setSceneButton(
    player_id: T.PlayerID,
    text: string,
    scene_id: T.SceneID | undefined
  ): JSX.Element {
    return (
      <Button
        key={"set-" + player_id + scene_id}
        onClick={() =>
          M.sendCommand({ t: "SetPlayerScene", player_id, scene_id })
        }
      >
        {text}
      </Button>
    );
  }
}

export function GrantCreaturesToPlayer(props: { player: T.Player; onDone: () => void }) {
  const { player, onDone } = props;
  return (
    <Campaign.MultiCreatureSelector
      already_selected={I.Set(player.creatures)}
      on_cancel={onDone}
      on_selected={cids => {
        M.sendCommand({
          t: "GiveCreaturesToPlayer",
          player_id: player.player_id,
          creature_ids: cids.toArray(),
        });
        onDone();
      }}
    />
  );
}

