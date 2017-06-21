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
    <CV.Tab key="Combat" name="Combat"><CV.Combat card={GM.GMCreatureCard}/></CV.Tab>,
    <CV.Tab key="Players" name="Players"><Players.Players /></CV.Tab>,
    <CV.Tab key="History" name="History"><History.History /></CV.Tab>,
    <CV.Tab key="SavedGames" name="Saved Games"><div>Saved Games!</div></CV.Tab>,
  ];

  const secondary = renderSecondary(ptui, dispatch);

  return <CV.TheLayout map={focus} tabs={tabs} secondary={secondary} />;
});

function renderSecondary(ptui: M.PTUI, dispatch: M.Dispatch) {
  if (ptui.state.focused_note) {
    return <CV.NoteEditor path={ptui.state.focused_note.path} name={ptui.state.focused_note.name} />;
  }
}

function mapCreatures(ptui: M.PTUI, dispatch: M.Dispatch, scene: T.Scene)
  : { [index: string]: Grid.MapCreature } {
  return LD.mapValues(Grid.mapCreatures(ptui, scene),
    mapc => ({ ...mapc, actions: creatureMenuActions(ptui, dispatch, mapc.creature) }));
}

function creatureMenuActions(ptui: M.PTUI, dispatch: M.Dispatch, creature: T.Creature) {
  return { "Move this creature": ((cid: T.CreatureID) => ptui.requestMove(dispatch, cid)) };
}

function gridFocus(ptui: M.PTUI, dispatch: M.Dispatch): JSX.Element {
  if (!ptui.state.main_focus) { return <div>No focus!</div>; }
  switch (ptui.state.main_focus.t) {
    case "Scene":
      const scene = ptui.focused_scene();
      return scene
        ? <Grid.SceneGrid scene={scene} creatures={mapCreatures(ptui, dispatch, scene)} />
        : <div>No scene yet!</div>;
    case "Map":
      const map = ptui.getMap(ptui.state.main_focus.map_id);
      if (!map) { return <div>Couldn't find map!</div>; }
      return <Grid.MapGrid map={map} />;
  }
}
