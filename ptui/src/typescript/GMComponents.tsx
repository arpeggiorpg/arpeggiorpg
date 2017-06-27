/// A grab-bag of GM-only components
import * as I from 'immutable';
import * as React from 'react';

import * as Dice from './Dice';

import {
  Accordion, Button, Card, Dropdown, Form, Header, Icon, Input, List, Message, Modal, Popup, Segment
} from 'semantic-ui-react';

import * as Campaign from './Campaign';
import * as CV from './CommonView';
import * as M from './Model';
import * as T from './PTTypes';
import * as TextInput from './TextInput';

export const GMScene = M.connectRedux(
  function GMScene({ scene, ptui, dispatch }: { scene: T.Scene } & M.ReduxProps): JSX.Element {
    return <Segment>
      <Header>{scene.name}</Header>
      <Accordion panels={[{
        title: "Creatures",
        content: <List>
          <List.Item key="add">
            <List.Content>
              <CV.ModalMaker
                button={toggler => <Button icon="edit" onClick={toggler} size="small" />}
                modal={toggler =>
                  <Campaign.MultiCreatureSelector
                    already_selected={scene.creatures.keySeq().toSet()}
                    on_cancel={toggler}
                    on_selected={cids => {
                      let new_creatures = cids.reduce(
                        (acc: I.Map<T.CreatureID, [T.Point3, T.Visibility]>, cid: T.CreatureID
                        ) => {
                          if (acc.has(cid)) {
                            return acc;
                          } else {
                            return acc.set(cid, [[0, 0, 0], { t: "AllPlayers" }]);
                          }
                        }, scene.creatures);
                      const removed_cids = I.Set(scene.creatures.keySeq()).subtract(cids);
                      new_creatures = new_creatures.deleteAll(removed_cids);
                      const new_scene = { ...scene, creatures: new_creatures };
                      ptui.sendCommand(dispatch, { t: "EditScene", scene: new_scene });
                      toggler();
                    }
                    } />
                } />
              />
            </List.Content>
          </List.Item>
          {ptui.getSceneCreatures(scene).map(creature => {
            const [pos, vis] = scene.creatures.get(creature.id)!; // !: must exist in map()
            const vis_desc = vis.t === 'GMOnly'
              ? 'Only visible to the GM' : 'Visible to all players';
            return <List.Item key={creature.id}>
              <List.Content floated='left'>{CV.classIcon(creature)}</List.Content>
              {creature.name}
              <List.Content floated='right'>
                <Popup
                  trigger={<Icon name='eye'
                    style={{ cursor: "pointer" }}
                    disabled={vis.t === 'GMOnly'}
                    onClick={() => {
                      const new_vis: T.Visibility =
                        vis.t === "GMOnly" ? { t: "AllPlayers" } : { t: "GMOnly" };

                      const new_scene = {
                        ...scene,
                        creatures: scene.creatures.set(creature.id, [pos, new_vis]),
                      };
                      ptui.sendCommand(dispatch, { t: "EditScene", scene: new_scene });
                    }} />}
                  content={vis_desc}
                />
              </List.Content>
            </List.Item>;
          }
          )}
        </List>,
      }]} />
    </Segment>;
  });

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
    const selected = I.Set(props.ptui.getSceneCreatures(props.scene).map(c => c.id));
    this.state = { selected };
  }
  render(): JSX.Element {
    const { scene, ptui, dispatch } = this.props;
    const creatures = ptui.getSceneCreatures(scene);
    const self = this;
    return <div>
      <Button
        onClick={() => ptui.sendCommand(dispatch,
          { t: "StartCombat", scene_id: scene.id, creature_ids: this.state.selected.toArray() })}
      >Start combat</Button>
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

    return <Segment>
      {
        scene
          ?
          <div><span style={{ fontWeight: "bold" }}>Scene:</span>&nbsp;
          <a href="#"
              onClick={() =>
                dispatch({ type: "FocusGrid", focus: { t: "Scene", scene_id: scene.id } })}>
              {scene.name}
            </a>
            <Button onClick={() => ptui.sendCommand(dispatch, { t: "StopCombat" })}>
              Stop combat
            </Button>
          </div>
          :
          <div>Lost scene!</div>
      }
    </Segment>;
  });

/// A customized CreatureCard that renders an editable note in the content area.
export function GMCreatureCard(props: { creature: T.Creature, menu_items?: Array<JSX.Element> }
): JSX.Element {
  const menu = <Dropdown icon="caret down" className="right" floating={true} pointing={true}>
    <Dropdown.Menu>
      <Dropdown.Header content={props.creature.name} />
      <CV.ModalMaker
        button={toggler => <Dropdown.Item onClick={toggler} content="Edit" />}
        modal={toggler => <GMEditCreature creature={props.creature} onClose={toggler} />}
      />
      {props.menu_items}
    </Dropdown.Menu>
  </Dropdown>;
  return <CV.CreatureCard creature={props.creature} menu={menu}>
    <CreatureNote creature={props.creature} />
  </CV.CreatureCard>;
}

export const GMCombatCreatureCard = M.connectRedux(
  function GMCombatCreatureCard(props: { creature: T.Creature } & M.ReduxProps): JSX.Element {
    const { creature, ptui, dispatch } = props;
    const menu_items = [
      <Dropdown.Item key="Remove from Combat" onClick={removeFromCombat} text="Remove from Combat" />
    ];


    return <GMCreatureCard creature={creature} menu_items={menu_items} />;

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


interface GMEditCreatureProps {
  creature: T.Creature;
  onClose: () => void;
}
class GMEditCreatureComp
  extends React.Component<GMEditCreatureProps & M.ReduxProps,
  { portrait_url: string; name: string, note: string, initiative_string: string }> {
  constructor(props: GMEditCreatureProps & M.ReduxProps) {
    super(props);
    this.state = {
      portrait_url: props.creature.portrait_url, name: props.creature.name,
      note: props.creature.note,
      initiative_string: Dice.format(props.creature.initiative),
    };
  }
  render(): JSX.Element {
    const { creature, onClose } = this.props;
    const parsed_initiative = Dice.maybeParse(this.state.initiative_string);
    return <div>
      <Header>{creature.name}</Header>
      <Form error={!parsed_initiative.status}>
        <Form.Input label="Name" value={this.state.name}
          onChange={(_, data) => this.setState({ name: data.value })} />
        <Form.Group style={{ width: "100%" }}>
          <Form.Field style={{ flex: "1" }}>
            <label>Portrait Image URL:</label>
            <Input fluid={true}
              value={this.state.portrait_url}
              onChange={(_, data) => this.setState({ portrait_url: data.value })} />
          </Form.Field>
          {this.state.portrait_url ? <CV.SquareImageIcon url={this.state.portrait_url} />
            : "Enter an URL for preview"}
        </Form.Group>
        <Form.Input label="Note" value={this.state.note}
          onChange={(_, data) => this.setState({ note: data.value })} />
        <Form.Input label="Initiative" error={!parsed_initiative.status}
          value={this.state.initiative_string}
          onChange={(_, data) => this.setState({ initiative_string: data.value })} />
        {
          parsed_initiative.status
            ? <Message>Parsed dice as {Dice.format(parsed_initiative.value)}</Message>
            :
            <Message error={true}>
              <Message.Header>Couldn't parse dice expression</Message.Header>
              <Message.Content>
                Expected {parsed_initiative.expected} at
              line {parsed_initiative.index.line}, column {parsed_initiative.index.column}
              </Message.Content>
            </Message>
        }
        <Form.Group>
          <Form.Button disabled={!parsed_initiative.status} onClick={() => this.save()}>
            Save
          </Form.Button>
          <Form.Button onClick={onClose}>Cancel</Form.Button>
        </Form.Group>
      </Form>
    </div >;
  }

  save() {
    const creature = {
      ...this.props.creature,
      name: this.state.name, note: this.state.note, portrait_url: this.state.portrait_url,
      initiative: Dice.parse(this.state.initiative_string),
    };
    this.props.ptui.sendCommand(this.props.dispatch, { t: "EditCreature", creature });
    this.props.onClose();
  }
}

const GMEditCreature = M.connectRedux(GMEditCreatureComp);


interface GMCreateItemProps { path: T.FolderPath; onClose: () => void; }
class GMCreateItemComp extends React.Component<GMCreateItemProps & M.ReduxProps, { name: string }> {
  constructor(props: GMCreateItemProps & M.ReduxProps) {
    super(props);
    this.state = { name: "" };
  }

  render(): JSX.Element {
    return <Form>
      <Form.Input label="Name" value={this.state.name}
        onChange={(_, data) => this.setState({ name: data.value })} />
      <Form.Button disabled={this.state.name === ""} onClick={() => this.save()}>
        Create
      </Form.Button>
      <Form.Button onClick={this.props.onClose}>Cancel</Form.Button>
    </Form>;
  }

  save() {
    this.props.ptui.sendCommand(this.props.dispatch,
      { t: "CreateItem", path: this.props.path, name: this.state.name });
    this.props.onClose();
  }
}
export const GMCreateItem = M.connectRedux(GMCreateItemComp);


export const GMViewItem = M.connectRedux(
  function GMViewItem({ item }: { item: T.Item } & M.ReduxProps): JSX.Element {
    return <Card>
      <Card.Content>
        <Card.Header>{item.name}</Card.Header>
      </Card.Content>
      <Card.Content extra={true}>
        <div className="ui buttons">
          <Button>Give to Creature</Button>
        </div>
      </Card.Content>
    </Card>;
  });
