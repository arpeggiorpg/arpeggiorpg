import * as LD from 'lodash';
import * as React from 'react';
import * as Redux from 'redux';

import * as Campaign from './Campaign';
import * as CV from './CommonView';
import * as GM from './GMComponents';
import * as Grid from './Grid';
import * as History from './History';
import * as M from './Model';
import * as Players from './Players';
import * as T from './PTTypes';

export const GMMain = M.connectRedux(({ ptui, dispatch }: M.ReduxProps): JSX.Element => {
  const focus = gridFocus(ptui, dispatch);
  const tabs = [
    <CV.Tab key="Campaign" name="Campaign"><Campaign.Campaign /></CV.Tab>,
    <CV.Tab key="Combat" name="Combat"><GM.GMCombat /></CV.Tab>,
    <CV.Tab key="Players" name="Players"><Players.Players /></CV.Tab>,
    <CV.Tab key="History" name="History"><History.History /></CV.Tab>,
    <CV.Tab key="SavedGames" name="Saved Games"><div>Saved Games!</div></CV.Tab>,
  ];

  const secondary = renderSecondary(ptui, dispatch);
  const tertiary = renderTertiary(ptui, dispatch);

  return <CV.TheLayout map={focus} tabs={tabs} secondary={secondary} tertiary={tertiary}
    bar_width={450} menu_size='tiny' />;
});

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
        ? <div>
          <GM.GMCreatureCard creature={creature} />
          <CV.CreatureInventory creature={creature} />
        </div>
        : undefined;
    case "Item":
      const item = ptui.getItem(focus2.item_id);
      return item ? <GM.GMViewItem item={item} /> : undefined;
  }
}

function renderTertiary(ptui: M.PTUI, dispatch: M.Dispatch): JSX.Element | undefined {
  const scene = ptui.focused_scene();
  if (scene) {
    return <GM.GMScene scene={scene} />;
  }
}

function mapCreatures(ptui: M.PTUI, dispatch: M.Dispatch, scene: T.Scene)
  : { [index: string]: Grid.MapCreature } {
  return LD.mapValues(Grid.mapCreatures(ptui, scene),
    mapc => ({ ...mapc, actions: creatureMenuActions(ptui, dispatch, mapc.creature) }));
}

function creatureMenuActions(ptui: M.PTUI, dispatch: M.Dispatch, creature: T.Creature) {
  const actions = { "Move this creature": ((cid: T.CreatureID) => ptui.requestMove(dispatch, cid)) };
  const combat = ptui.app.current_game.current_combat;
  if (combat && ptui.getCurrentCombatCreatureID(combat) === creature.id) {
    LD.assign(actions, {
      "Combat-move this creature": (cid: T.CreatureID) =>
        ptui.requestCombatMovement(dispatch),
    });
  }
  return actions;
}

function gridFocus(ptui: M.PTUI, dispatch: M.Dispatch): JSX.Element {
  if (!ptui.state.grid_focus) { return <div>No focus!</div>; }
  switch (ptui.state.grid_focus.t) {
    case "Scene":
      const scene = ptui.focused_scene();
      return scene
        ? <Grid.SceneGrid scene={scene} creatures={mapCreatures(ptui, dispatch, scene)} />
        : <div>No scene yet!</div>;
    case "Map":
      const map = ptui.getMap(ptui.state.grid_focus.map_id);
      if (!map) { return <div>Couldn't find map!</div>; }
      return <Grid.MapGrid map={map} />;
  }
}
