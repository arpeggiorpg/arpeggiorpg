import * as LD from 'lodash';
import * as React from 'react';
import { createSelectorCreator, defaultMemoize } from 'reselect';

import * as M from './Model';

export const createDeepEqualSelector = createSelectorCreator(defaultMemoize, M.isEqual);

function debugIsEqual<T>(a: T, b: T): boolean {
  const result = M.isEqual(a, b);
  console.log("[debugIsEqual]", result ? "EQUAL" : "UNEQUAL", a, b);
  return result;
}
export const createDebugDeepEqualSelector = createSelectorCreator(defaultMemoize, debugIsEqual);

/** A component which deep-compares props to determine whether it should update. */
export class Component<P, S> extends React.Component<P, S> {
  shouldComponentUpdate(nextProps: P, nextState: S) {
    return !(
      M.isEqual(
        LD.omit(this.props, 'dispatch', 'sendCommand'),
        LD.omit(nextProps, 'dispatch', 'sendCommand'))
      && M.isEqual(this.state, nextState));
  }
}
