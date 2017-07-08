import * as LD from 'lodash';
import * as React from 'react';
import * as ReactRedux from 'react-redux';
import { createSelectorCreator, defaultMemoize } from 'reselect';
import * as JD from 'type-safe-json-decoder';

import * as M from './Model';
import * as T from './PTTypes';

export const createDeepEqualSelector = createSelectorCreator(defaultMemoize, M.isEqual);

interface SCProps { sendCommand: (dispatch: M.Dispatch, cmd: T.GameCommand) => void; }

export type PTProps = SCProps & M.DispatchProps;

export function connect<BaseProps extends object, DerivedProps extends object>(
  mapState: (ptui: M.PTUI, props: BaseProps) => DerivedProps
): ReactRedux.ComponentDecorator<DerivedProps & SCProps & M.DispatchProps, BaseProps> {
  // TODO: get better at typescript and get rid of all the `as any` in this function
  return ReactRedux.connect<DerivedProps & SCProps, M.DispatchProps, BaseProps>(
    (ptui, props): DerivedProps & SCProps => {
      const mapped: DerivedProps = mapState(ptui, props as any);
      return {
        ...mapped as any,
        sendCommand: (dispatch: M.Dispatch, cmd: T.GameCommand) => ptui.sendCommand(dispatch, cmd),
      };
    }
    ,
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
