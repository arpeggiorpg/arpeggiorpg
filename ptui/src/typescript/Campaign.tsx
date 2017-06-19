import Flexbox from 'flexbox-react';
import * as React from "react";
import * as ReactDOM from "react-dom";

import * as M from './Model';
import * as T from './PTTypes';

export const Campaign = M.connectRedux(({ ptui, dispatch }: M.ReduxProps): JSX.Element => {
  return <div>Hello, campaign!</div>;
});
