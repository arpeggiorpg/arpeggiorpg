import * as LD from 'lodash';
import * as React from 'react';
import * as Redux from 'redux';

import * as CV from './CommonView';
import * as M from './Model';
import * as T from './PTTypes';

export class GMMain extends React.Component<{}, undefined> {
  render(): JSX.Element {
    const map = <div>No scene yet!</div>;
    const tabs = [
      <CV.Tab key="Campaign" name="Campaign"><div>Campaign!</div></CV.Tab>,
      <CV.Tab key="Combat" name="Combat"><CV.Combat /></CV.Tab>,
      <CV.Tab key="Players" name="Players"><div>Players go here!</div></CV.Tab>,
      <CV.Tab key="History" name="History"><div>History!</div></CV.Tab>,
      <CV.Tab key="SavedGames" name="Saved Games"><div>Saved Games!</div></CV.Tab>,
    ];
    return <CV.TheLayout map={map} tabs={tabs} under={<div>Hello!</div>} />;
  }
}
