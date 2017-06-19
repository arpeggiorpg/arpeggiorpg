import * as LD from 'lodash';
import * as React from 'react';
import * as Redux from 'redux';

import * as Campaign from './Campaign';
import * as CV from './CommonView';
import * as Grid from './Grid';
import * as History from './History';
import * as M from './Model';
import * as Players from './Players';
import * as T from './PTTypes';

export const GMMain = M.connectRedux(({ ptui, dispatch }: M.ReduxProps): JSX.Element => {
  const scene = ptui.focused_scene();
  const map = scene
    ? <Grid.Grid scene={scene} creatures={mapCreatures(ptui, dispatch, scene)} />
    : <div>No scene yet!</div>;
  const tabs = [
    <CV.Tab key="Campaign" name="Campaign"><Campaign.Campaign /></CV.Tab>,
    <CV.Tab key="Combat" name="Combat"><CV.Combat /></CV.Tab>,
    <CV.Tab key="Players" name="Players"><Players.Players /></CV.Tab>,
    <CV.Tab key="History" name="History"><History.History /></CV.Tab>,
    <CV.Tab key="SavedGames" name="Saved Games"><div>Saved Games!</div></CV.Tab>,
  ];
  return <CV.TheLayout map={map} tabs={tabs} under={<div>Hello!</div>} />;
});

function mapCreatures(ptui: M.PTUI, dispatch: M.Dispatch, scene: T.Scene)
  : { [index: string]: Grid.MapCreature } {
  return LD.mapValues(Grid.mapCreatures(ptui, scene),
    mapc => ({ ...mapc, actions: creatureMenuActions(ptui, dispatch, mapc.creature) }));
}

function creatureMenuActions(ptui: M.PTUI, dispatch: M.Dispatch, creature: T.Creature) {
  return { "Move this creature": ((cid: T.CreatureID) => ptui.requestMove(dispatch, cid)) };
}
