import I from 'immutable';
import mapValues from 'lodash/mapValues';
import * as React from 'react';

import * as Campaign from './Campaign';
import * as CV from './CommonView';
import * as GM from './GMComponents';
import * as Grid from './Grid';
import * as History from './History';
import * as M from './Model';
import * as A from './Actions';
import * as Players from './Players';
import * as T from './PTTypes';

export function GMMain() {
  const scene = M.useState(s => s.getFocusedScene());
  const creaturesInMap = M.useState(s => mapCreatures(s))
  const grid = scene
    ? <Grid.SceneGrid scene={scene} creatures={creaturesInMap} />
    : <div>No scene yet!</div>;

  const tabs = [
    <CV.Tab key="Campaign" name="Campaign"><Campaign.Campaign /></CV.Tab>,
    <CV.Tab key="Combat" name="Combat"><GM.GMCombat /></CV.Tab>,
    <CV.Tab key="Players" name="Players"><Players.Players /></CV.Tab>,
    <CV.Tab key="History" name="History"><History.History /></CV.Tab>,
    <CV.Tab key="SavedGames" name="Saved Games"><GM.SavedGames /></CV.Tab>,
  ];

  const combat = M.useState(s => s.getCombat());
  const currentCreatureInCombat = M.useState(s => s.getCurrentCombatCreatureID());

  const bottom_bar = combat && currentCreatureInCombat ?
    <CV.ActionBar creatureId={currentCreatureInCombat} combat={combat} />
    : undefined;

  return <CV.TheLayout map={grid} tabs={tabs}
    bottom_left={<Secondary />}
    top_left={scene ? <GM.GMScene scene={scene} /> : <div>Select a scene</div>}
    bottom_right={<CV.GMChat />}
    bar_width={450} menu_size='tiny' bottom_bar={bottom_bar} />;
}

function Secondary() {
  const focus2 = M.useState(s => s.secondaryFocus);
  if (!focus2) { return undefined; }
  switch (focus2.t) {
    case "Note":
      return <CV.NoteEditor path={focus2.path} name={focus2.name}
        // We need to refocus with the new name after a note gets renamed:
        afterSave={(path, note) =>
          M.getState().setSecondaryFocus({ t: "Note", path, name: note.name })}/>;
    case "Creature":
      return <GM.CreatureFocus creatureId={focus2.creature_id} />;
    case "Item":
      return <GM.GMViewItem itemId={focus2.item_id} />;
  }
}

/** Create `MapCreature`s for all creatures in a scene, and annotate them with GM-specific actions.
 */
function mapCreatures(state: M.AllStates): { [index: string]: Grid.MapCreature } {
  const scene = state.getFocusedScene();
  if (!scene) return {};

  return mapValues(Grid.mapCreatures(state, scene),
    mapc => ({
      ...mapc,
      actions: mapc.actions.merge(creatureMenuActions(state, scene, state.getGame().current_combat, mapc.creature)),
    }));
}

function creatureMenuActions(state: M.AllStates, scene: T.Scene, combat: T.Combat | undefined, creature: T.Creature): I.Map<string, (cid: T.CreatureID) => void> {
  let actions: I.Map<string, (cid: T.CreatureID) => void> = I.Map({
    "Walk": (cid: T.CreatureID) => A.requestMove(cid),
    "Teleport": (cid: T.CreatureID) => Grid.requestTeleport(scene, cid),
  });
  if (combat && state.getCurrentCombatCreatureID() === creature.id) {
    actions = actions.merge({
      "Combat-move": (_: T.CreatureID) => A.requestCombatMovement(),
    });
  }
  return actions;
}


