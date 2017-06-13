import * as React from "react";
import * as ReactDOM from "react-dom";
import * as T from './PTTypes';
import * as CommonView from './CommonView';
import { PTUI } from './Model';
import * as M from './Model';
import * as LD from 'lodash';

interface GridProps {
  ptui: M.PTUI;
  scene: T.SceneID;
}


export class Grid extends React.Component<GridProps, undefined> {
  render(): JSX.Element {
    return <svg>
      <circle cx={50} cy={50} r={10} fill="red" />
    </svg>;
  }
}
