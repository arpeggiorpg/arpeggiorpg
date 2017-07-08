import * as LD from 'lodash';
import * as React from 'react';
import * as ReactRedux from 'react-redux';
import { createSelectorCreator, defaultMemoize } from 'reselect';
import * as JD from 'type-safe-json-decoder';

import * as M from './Model';
import * as T from './PTTypes';

export const createDeepEqualSelector = createSelectorCreator(defaultMemoize, M.isEqual);

function debugIsEqual<T>(a: T, b: T): boolean {
  const result = M.isEqual(a, b);
  console.log("[debugIsEqual]", result ? "EQUAL" : "UNEQUAL", a, b);
  return result;
}
export const createDebugDeepEqualSelector = createSelectorCreator(defaultMemoize, debugIsEqual);

export function connect<BaseProps extends object, DerivedProps extends object>(
  mapState: (ptui: M.PTUI, props: BaseProps) => DerivedProps
): ReactRedux.ComponentDecorator<DerivedProps & M.DispatchProps, BaseProps> {
  // TODO: get better at typescript and get rid of all the `as any` in this function
  return ReactRedux.connect<DerivedProps, M.DispatchProps, BaseProps>(
    (ptui, props): DerivedProps => {
      return mapState(ptui, props as any);
    },
    (dispatch: M.Dispatch) => ({ dispatch })
  );
}


/** A component which deep-compares props to determine whether it should update. */
export class Component<P, S> extends React.Component<P, S> {
  shouldComponentUpdate(nextProps: P, nextState: S, nextContext: any) {
    const sCU = super.shouldComponentUpdate;
    return !(
      M.isEqual(
        LD.omit(this.props, 'dispatch', 'sendCommand'),
        LD.omit(nextProps, 'dispatch', 'sendCommand'))
      && M.isEqual(this.state, nextState));
  }
}
