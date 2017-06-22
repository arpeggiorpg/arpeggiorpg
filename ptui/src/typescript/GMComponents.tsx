/// A grab-bag of GM-only components
import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from 'react';

import { Dropdown } from 'semantic-ui-react';

import * as CV from './CommonView';
import * as M from './Model';
import * as T from './PTTypes';
import * as TextInput from './TextInput';


export const GMCombat = M.connectRedux(
  function GMCombat({ ptui, dispatch }: M.ReduxProps): JSX.Element {
    const combat = ptui.app.current_game.current_combat;
    if (!combat) {
      const scene = ptui.focused_scene();
      const startCombat = scene
        ? <StartCombat scene={scene} />
        : <div>Load a scene to start a combat.</div>;
      return startCombat;
    }
    const cur_creature = ptui.getCurrentCombatCreature(combat);

    return <div>
      <GMCombatHeader combat={combat} />
      <CV.Combat combat={combat} card={GMCombatCreatureCard} initiative={initiative} />
      <CV.ActionBar creature={cur_creature} combat={combat} />
    </div>;

    function initiative(creature: T.Creature, init: number) {
      return <CV.Toggler a={view} b={edit} />;

      function view(toggle: CV.ToggleFunc) {
        return <div style={{ cursor: "pointer", textDecoration: "underline dotted" }}
          onClick={toggle}
        >{init}</div>;
      }
      function edit(toggle: CV.ToggleFunc) {
        return <TextInput.TextInput defaultValue={init.toString()} style={{ width: "25px" }}
          numbersOnly={true}
          onCancel={toggle}
          onSubmit={input => { toggle(); changeInit(creature, input); }} />;
      }
    }

    function changeInit(creature: T.Creature, new_init: string) {
      const new_init_num = Number(new_init);
      ptui.sendCommand(dispatch, {
        t: "ChangeCreatureInitiative", creature_id: creature.id,
        init: new_init_num,
      });
    }
  });

class StartCombatComp
  extends React.Component<{ scene: T.Scene } & M.ReduxProps, { selected: I.Set<T.CreatureID> }> {
  constructor(props: { scene: T.Scene } & M.ReduxProps) {
    super(props);
    const selected = I.Set(props.ptui.getCreatures(LD.keys(props.scene.creatures)).map(c => c.id));
    this.state = { selected };
  }
  render(): JSX.Element {
    const { scene, ptui, dispatch } = this.props;
    const creatures = ptui.getCreatures(LD.keys(scene.creatures));
    const self = this;
    return <div>
      <button
        onClick={() => ptui.sendCommand(dispatch,
          { t: "StartCombat", scene_id: scene.id, creature_ids: this.state.selected.toArray() })}
      >Start combat</button>
      {creatures.map(creature =>
        <div key={creature.id} style={{ display: "flex", flexDirection: "row" }}>
          <input type="checkbox" checked={this.state.selected.includes(creature.id)}
            onChange={nv => handleChange(nv.currentTarget.checked, creature.id)} />
          {creature.name}
        </div>)}
    </div>;

    function handleChange(add: boolean, creature_id: T.CreatureID) {
      const new_selected = add
        ? self.state.selected.add(creature_id)
        : self.state.selected.remove(creature_id);
      self.setState({ selected: new_selected });
    }
  }
}

const StartCombat = M.connectRedux(StartCombatComp);

const GMCombatHeader = M.connectRedux(
  function GMCombatHeader({ combat, ptui, dispatch }: { combat: T.Combat } & M.ReduxProps) {
    const scene = ptui.getScene(combat.scene);

    return <div style={{ border: "1px solid black", borderRadius: "5px", padding: '3px' }}>
      {
        scene
          ?
          <div><span style={{ fontWeight: "bold" }}>Scene:</span>&nbsp;
          <a href="#"
              onClick={() => dispatch({ type: "Focus", focus: { t: "Scene", scene_id: scene.id } })}>
              {scene.name}
            </a>
          </div>
          :
          <div>Lost scene!</div>
      }
      <div>
        <button>Add creature</button>
        <button onClick={() => ptui.sendCommand(dispatch, { t: "StopCombat" })}>Stop combat</button>
      </div>
    </div>;
  });

/// A customized CreatureCard that renders an editable note in the content area.
export function GMCreatureCard(props: { creature: T.Creature, menu?: JSX.Element }): JSX.Element {
  return <CV.CreatureCard creature={props.creature} menu={props.menu}>
    <CreatureNote creature={props.creature} />
  </CV.CreatureCard>;
}

export const GMCombatCreatureCard = M.connectRedux(
  function GMCombatCreatureCard(props: { creature: T.Creature } & M.ReduxProps): JSX.Element {
    const { creature, ptui, dispatch } = props;
    const menu = <Dropdown icon="caret down" className="right" floating={true} pointing={true}>
      <Dropdown.Menu>
        <Dropdown.Header content={creature.name} />
        <Dropdown.Item onClick={removeFromCombat} text="Remove from Combat" />
      </Dropdown.Menu>
    </Dropdown>;

    return <GMCreatureCard creature={creature} menu={menu} />;

    function removeFromCombat() {
      ptui.sendCommand(dispatch, { t: "RemoveCreatureFromCombat", creature_id: creature.id });
    }
  });


/// A single-line editable creature note
const CreatureNote = M.connectRedux(
  function CreatureNote({ creature, ptui, dispatch }: { creature: T.Creature } & M.ReduxProps) {
    function view(toggle: CV.ToggleFunc) {
      return <div
        style={{ cursor: "pointer", textDecoration: "underline dotted" }}
        onClick={toggle}>
        {creature.note}
      </div>;
    }
    function edit(toggle: CV.ToggleFunc) {
      return <TextInput.TextInput defaultValue={creature.note}
        onCancel={toggle}
        onSubmit={input => { toggle(); submitNote(input); }} />;
    }

    return <CV.Toggler a={view} b={edit} />;

    function submitNote(note: string) {
      const new_creature = { ...creature, note };
      ptui.sendCommand(dispatch, { t: "EditCreature", creature: new_creature });
    }
  });
