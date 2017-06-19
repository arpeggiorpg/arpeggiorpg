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

export const GMMain = M.connectRedux(({ ptui }: M.ReduxProps): JSX.Element => {
  const scene = ptui.focused_scene();
  const map = scene ? <Grid.Grid scene={scene} creatures={{}} /> : <div>No scene yet!</div>;
  const tabs = [
    <CV.Tab key="Campaign" name="Campaign"><Campaign.Campaign /></CV.Tab>,
    <CV.Tab key="Combat" name="Combat"><CV.Combat /></CV.Tab>,
    <CV.Tab key="Players" name="Players"><Players.Players /></CV.Tab>,
    <CV.Tab key="History" name="History"><History.History /></CV.Tab>,
    <CV.Tab key="SavedGames" name="Saved Games"><div>Saved Games!</div></CV.Tab>,
  ];
  return <CV.TheLayout map={map} tabs={tabs} under={<div>Hello!</div>} />;
});
