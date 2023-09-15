import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from 'react';

// import * as Campaign from './Campaign';
import * as CV from './CommonView';
// import * as GM from './GMComponents';
// import * as Grid from './Grid';
// import * as History from './History';
import * as M from './Model';
// import * as Players from './Players';
import * as T from './PTTypes';

export function GMMain() {
  const scene = M.useFocusedScene();
  const grid = scene
    ? <div>GRID {JSON.stringify(scene)}</div> // </div><Grid.SceneGrid scene={scene} creatures={mapCreatures(scene)} />
    : <div>No scene yet!</div>;

  const tabs = [
    <CV.Tab key="Cool" name="Cool"><div>Cool!</div></CV.Tab>
  //   <CV.Tab key="Campaign" name="Campaign"><Campaign.Campaign /></CV.Tab>,
  //   <CV.Tab key="Combat" name="Combat"><GM.GMCombat /></CV.Tab>,
  //   <CV.Tab key="Players" name="Players"><Players.Players /></CV.Tab>,
  //   <CV.Tab key="History" name="History"><History.History /></CV.Tab>,
  //   <CV.Tab key="SavedGames" name="Saved Games"><GM.SavedGames /></CV.Tab>,
  ];

  const secondary = <div>Secondary!</div>; // renderSecondary();
  const tertiary = <div>Tertiary!</div>; // renderTertiary();
  const bottom_bar = <div>Bottom!</div>;
  // const combat = M.useCombat();
  // const bottom_bar = combat ?
  //   <CV.ActionBar creature={ptui.getCurrentCombatCreature(combat)} combat={combat} />
  //   : undefined;

  return <CV.TheLayout map={grid} tabs={tabs}
    bottom_left={secondary}
    top_left={tertiary}
    bottom_right={<CV.GMChat />}
    bar_width={450} menu_size='tiny' bottom_bar={bottom_bar} />;
}

function renderSecondary(ptui: M.PTUI, dispatch: M.Dispatch): JSX.Element | undefined {
  if (!ptui.state.secondary_focus) { return undefined; }
  const focus2 = ptui.state.secondary_focus;
  switch (focus2.t) {
    case "Note":
      return <CV.NoteEditor path={focus2.path} name={focus2.name}
        // We need to refocus with the new name after a note gets renamed:
        afterSave={(path, note) =>
          dispatch({ type: "FocusSecondary", focus: { t: "Note", path, name: note.name } })} />;
    case "Creature":
      const creature = ptui.getCreature(focus2.creature_id);
      return creature
        ? <GM.CreatureFocus creature={creature} />
        : undefined;
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
function mapCreatures(ptui: M.PTUI, dispatch: M.Dispatch, scene: T.Scene)
  : { [index: string]: Grid.MapCreature } {
  return LD.mapValues(Grid.mapCreatures(ptui, dispatch, scene),
    mapc => ({
      ...mapc,
      actions: mapc.actions.merge(creatureMenuActions(ptui, dispatch, scene, mapc.creature)),
    }));
}

function creatureMenuActions(
  ptui: M.PTUI, dispatch: M.Dispatch, scene: T.Scene, creature: T.Creature):
  I.Map<string, (cid: T.CreatureID) => void> {
  let actions: I.Map<string, (cid: T.CreatureID) => void> = I.Map({
    "Walk": (cid: T.CreatureID) => ptui.requestMove(dispatch, cid),
    "Teleport": (cid: T.CreatureID) => Grid.requestTeleport(dispatch, scene, cid),
  });
  const combat = ptui.app.current_game.current_combat;
  if (combat && ptui.getCurrentCombatCreatureID(combat) === creature.id) {
    actions = actions.merge({
      "Combat-move": (_: T.CreatureID) => ptui.requestCombatMovement(dispatch),
    });
  }
  return actions;
}

