import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from 'react';

import * as Campaign from './Campaign';
import * as CV from './CommonView';
import * as GM from './GMComponents';
import * as Grid from './Grid';
// import * as History from './History';
import * as M from './Model';
// import * as Players from './Players';
import * as T from './PTTypes';

export function GMMain() {
  const scene = M.useState(s => s.getFocusedScene());
  const app = M.useState(s => s.app);
  const gridModel = M.useState(s => s.grid);
  const grid = scene
    ? <Grid.SceneGrid scene={scene} creatures={mapCreatures(app, gridModel, scene)} />
    : <div>No scene yet!</div>;

  const tabs = [
    <CV.Tab key="Campaign" name="Campaign"><Campaign.Campaign /></CV.Tab>,
    <CV.Tab key="Combat" name="Combat"><GM.GMCombat /></CV.Tab>,
  //   <CV.Tab key="Players" name="Players"><Players.Players /></CV.Tab>,
  //   <CV.Tab key="History" name="History"><History.History /></CV.Tab>,
    <CV.Tab key="SavedGames" name="Saved Games"><GM.SavedGames /></CV.Tab>,
  ];

  const tertiary = <div>Tertiary!</div>; // renderTertiary();
  const bottom_bar = <div>Bottom!</div>;
  // const bottom_bar = combat ?
  //   <CV.ActionBar creature={ptui.getCurrentCombatCreature(combat)} combat={combat} />
  //   : undefined;

  return <CV.TheLayout map={grid} tabs={tabs}
    bottom_left={<Secondary />}
    top_left={tertiary}
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
      const item = ptui.getItem(focus2.item_id);
      return item ? <GM.GMViewItem item={item} /> : undefined;
  }
}

function renderTertiary(ptui: M.PTUI): JSX.Element | undefined {
  const scene = ptui.focused_scene();
  if (scene) {
    return <GM.GMScene scene={scene} />;
  }
}

/** Create `MapCreature`s for all creatures in a scene, and annotate them with GM-specific actions.
 */
function mapCreatures(app: T.App, grid: M.GridModel, scene: T.Scene): { [index: string]: Grid.MapCreature } {
  return LD.mapValues(Grid.mapCreatures(app, grid, scene),
    mapc => ({
      ...mapc,
      actions: mapc.actions.merge(creatureMenuActions(scene, app.current_game.current_combat, mapc.creature)),
    }));
}

function creatureMenuActions(scene: T.Scene, combat: T.Combat | undefined, creature: T.Creature): I.Map<string, (cid: T.CreatureID) => void> {
  let actions: I.Map<string, (cid: T.CreatureID) => void> = I.Map({
    "Walk": (cid: T.CreatureID) => M.requestMove(cid),
    "Teleport": (cid: T.CreatureID) => Grid.requestTeleport(scene, cid),
  });
  if (combat && M.getCurrentCombatCreatureID(combat) === creature.id) {
    actions = actions.merge({
      "Combat-move": (_: T.CreatureID) => M.requestCombatMovement(),
    });
  }
  return actions;
}


