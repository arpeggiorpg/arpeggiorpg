import * as React from 'react';

import * as CV from './CommonView';
import * as M from './Model';
import * as T from './PTTypes';
import * as TextInput from './TextInput';


export function GMCreatureCard(props: { creature: T.Creature }): JSX.Element {
  return <CV.CreatureCard creature={props.creature}>
    <CreatureNote creature={props.creature} />
  </CV.CreatureCard>;
}


class CreatureNoteComp
  extends React.Component<{ creature: T.Creature } & M.ReduxProps, { editing: boolean }> {
  constructor(props: { creature: T.Creature } & M.ReduxProps) {
    super(props);
    this.state = { editing: false };
  }

  render(): JSX.Element {
    const { creature } = this.props;
    if (this.state.editing) {
      return <TextInput.TextInput defaultValue={creature.note} styles={{}}
        onCancel={() => this.setState({ editing: false })}
        onSubmit={input => this.submitNote(creature, input)} />;
    } else {
      return <div onClick={() => this.setState({ editing: true })}>{creature.note}</div>;
    }
  }

  submitNote(creature: T.Creature, note: string) {
    const { ptui, dispatch } = this.props;
    const new_creature = { ...creature, note };
    ptui.sendCommand(dispatch, { t: "EditCreature", creature: new_creature });
    this.setState({ editing: false });
  }
}

export const CreatureNote = M.connectRedux(CreatureNoteComp);
