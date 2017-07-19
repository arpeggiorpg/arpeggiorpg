import * as I from 'immutable';
import * as React from "react";

import { Button, List, Table } from 'semantic-ui-react';

import * as Campaign from './Campaign';
import * as CV from './CommonView';
import * as Comp from './Component';
import * as M from './Model';
import * as T from './PTTypes';

export function renderPlayers() {
  console.log("sorry elm");
}

export const Players = Comp.connect(
  Comp.createDeepEqualSelector(
    [ptui => ptui.app,
    ptui => ptui.app.players,
    ptui => ptui.focused_scene(),
    ],
    (app, players, gm_scene) => ({
      gm_scene,
      player_creatures: players.valueSeq().toArray().map(
        player => {
          const scene = player.scene ? app.current_game.scenes[player.scene] : undefined;
          return {
            player, id: player.player_id,
            creatures: M.filterMap(player.creatures, cid => M.getCreature(app, cid)),
            scene_name: scene ? scene.name : "No scene",
          };
        }
      ),
    }))
)(
  function Players({ gm_scene, player_creatures, dispatch }): JSX.Element {
    return <Table celled={true}>
      <Table.Header>
        <Table.Row>
          <Table.HeaderCell>Player</Table.HeaderCell>
          <Table.HeaderCell>Creatures</Table.HeaderCell>
          <Table.HeaderCell>Scene</Table.HeaderCell>
          <Table.HeaderCell>Actions</Table.HeaderCell>
        </Table.Row>
      </Table.Header>
      <Table.Body>
        {
          player_creatures.map(playa => {
            const sceneButtons = [];
            if (playa.player.scene) {
              sceneButtons.push(setSceneButton(playa.id, "Remove from Scene", undefined));
            }
            if (gm_scene && playa.player.scene !== gm_scene.id) {
              sceneButtons.push(setSceneButton(playa.id, "Move to this scene", gm_scene.id));
            }

            return <Table.Row key={playa.id}>
              <Table.Cell>{playa.id}</Table.Cell>
              <Table.Cell>
                <List>
                  {playa.creatures.map(creature => {
                    return <List.Item key={playa.id + "-" + creature.id}>
                      {creature.name}
                    </List.Item>;
                  })}
                </List>
              </Table.Cell>
              <Table.Cell>
                {playa.scene_name}
              </Table.Cell>
              <Table.Cell>
                <Button.Group vertical={true}>
                  {sceneButtons}
                  <CV.ModalMaker
                    button={open => <Button onClick={open}>Grant creatures</Button>}
                    header={<span>Grant creatures to {playa.id}</span>}
                    content={close =>
                      <GrantCreaturesToPlayer player={playa.player} onDone={close} />}
                  />
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
        onClick={() => dispatch(M.sendCommand({ t: "SetPlayerScene", player_id, scene_id }))} >
        {text}
      </Button >;
    }
  });


export const GrantCreaturesToPlayer = M.connectRedux(
  function GrantCreaturesToPlayer(props: { player: T.Player; onDone: () => void; } & M.ReduxProps)
    : JSX.Element {
    const { player, onDone, ptui, dispatch } = props;
    return <Campaign.MultiCreatureSelector
      already_selected={I.Set(player.creatures)}
      on_cancel={onDone}
      on_selected={cids => {
        ptui.sendCommand(dispatch,
          { t: 'GiveCreaturesToPlayer', player_id: player.player_id, creature_ids: cids.toArray() });
        onDone();
      }}
    />;
  });
