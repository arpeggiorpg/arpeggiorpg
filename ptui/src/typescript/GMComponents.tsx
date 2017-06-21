/// A grab-bag of GM-only components
import * as React from 'react';

import * as CV from './CommonView';
import * as M from './Model';
import * as T from './PTTypes';
import * as TextInput from './TextInput';


/// A customized CreatureCard that renders an editable note in the content area.
export function GMCreatureCard(props: { creature: T.Creature }): JSX.Element {
  return <CV.CreatureCard creature={props.creature}>
    <CreatureNote creature={props.creature} />
  </CV.CreatureCard>;
}

export const GMCombatCreatureCard = M.connectRedux(
  function GMCombatCreatureCard(props: { creature: T.Creature } & M.ReduxProps): JSX.Element {
    const { creature, ptui, dispatch } = props;
    return <div style={{ display: "flex" }}>
      <GMCreatureCard creature={creature} />
      <CV.IconMenu>
        <CV.MenuHeader>{creature.name}</CV.MenuHeader>
        <CV.MenuItem onClick={removeFromCombat}>Remove from Combat</CV.MenuItem>
      </CV.IconMenu>
    </div>;

    function removeFromCombat() {
      ptui.sendCommand(dispatch, { t: "RemoveCreatureFromCombat", creature_id: creature.id });
    }
  });


/// A single-line editable creature note
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
