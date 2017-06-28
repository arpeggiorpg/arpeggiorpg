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
        title: 'Creatures',
        content: <List>
          <List.Item key="add">
            <List.Content>
              <CV.ModalMaker
                button={toggler =>
                  <Icon name="edit" onClick={toggler} style={{ cursor: "pointer" }} />}
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


interface GMCreateCreatureProps {
  path: T.FolderPath;
  onClose: () => void;
}
export const CreateCreature = M.connectRedux(
  function CreateCreature(props: GMCreateCreatureProps & M.ReduxProps) {
    const { path, ptui, dispatch } = props;
    const init: T.Dice = { t: "Expr", num: 1, size: 20 };
    const creature_data = {
      name: "", note: "", portrait_url: "", initiative: init, class_: "",
    };
    return <div>
      <Header>Create new creature in {M.folderPathToString(path)}</Header>
      <EditCreatureData creature={creature_data}
        onSave={cdata => save(cdata)} onClose={props.onClose} />
    </div>;

    function save(cdata: T.CreatureCreation) {
      ptui.sendCommand(dispatch, { t: "CreateCreature", path, spec: cdata });
    }
  }
);


interface GMEditCreatureProps {
  creature: T.Creature;
  onClose: () => void;
}
const GMEditCreature = M.connectRedux(
  function GMEditCreature(props: GMEditCreatureProps & M.ReduxProps) {
    const { creature, onClose, ptui, dispatch } = props;
    return <div>
      <Header>Edit {creature.name}</Header>
      <EditCreatureData creature={creature} onSave={c => save(c)} onClose={onClose} />
    </div>;

    function save(creature_data: T.CreatureCreation) {
      const new_creature = {
        ...creature,
        name: creature_data.name, class_: creature_data.class_,
        note: creature_data.note, portrait_url: creature_data.portrait_url,
        initiative: creature_data.initiative,
      };
      ptui.sendCommand(dispatch, { t: "EditCreature", creature: new_creature });
      onClose();
    }
  }
);

interface EditCreatureDataProps {
  creature: T.CreatureCreation;
  onClose: () => void;
  onSave: (cdata: T.CreatureCreation) => void;
}
class EditCreatureDataComp
  extends React.Component<EditCreatureDataProps & M.ReduxProps,
  { name: string; portrait_url: string; note: string, initiative_string: string, class_: string }> {
  constructor(props: EditCreatureDataProps & M.ReduxProps) {
    super(props);
    this.state = {
      portrait_url: props.creature.portrait_url, name: props.creature.name,
      note: props.creature.note,
      initiative_string: Dice.format(props.creature.initiative),
      class_: props.creature.class_,
    };
  }

  render(): JSX.Element {
    const { creature, onClose, ptui } = this.props;
    const parsed_initiative = Dice.maybeParse(this.state.initiative_string);
    const classes = ptui.app.current_game.classes.keySeq().toArray().map((className: string) =>
      ({ key: className, text: className, value: className }));
    const form_ok = (
      parsed_initiative.status
      && ptui.app.current_game.classes.has(this.state.class_)
    );
    return <Form error={!parsed_initiative.status}>
      <Form.Group>
        <Form.Input label="Name" value={this.state.name}
          onChange={(_, data) => this.setState({ name: data.value })} />
        <Form.Select label='Class' value={this.state.class_}
          options={classes} placeholder='Class'
          onChange={(_, data) => this.setState({ class_: data.value as string })} />
      </Form.Group>
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
        <Form.Button disabled={!form_ok} onClick={() => this.save()}>
          Save
          </Form.Button>
        <Form.Button onClick={onClose}>Cancel</Form.Button>
      </Form.Group>
    </Form>;
  }

  save() {
    const creature = {
      name: this.state.name, class_: this.state.class_,
      portrait_url: this.state.portrait_url, note: this.state.note,
      initiative: Dice.parse(this.state.initiative_string),
    };
    this.props.onSave(creature);
    this.props.onClose();
  }
}

const EditCreatureData = M.connectRedux(EditCreatureDataComp);

interface GMCreateItemProps { path: T.FolderPath; onClose: () => void; }
class GMCreateItemComp extends React.Component<GMCreateItemProps & M.ReduxProps, { name: string }> {
  constructor(props: GMCreateItemProps & M.ReduxProps) {
    super(props);
    this.state = { name: "" };
  }

  render(): JSX.Element {
    return <div>
      <Header>Create item in {M.folderPathToString(this.props.path)}</Header>
      <Form>
        <Form.Input label="Name" value={this.state.name}
          onChange={(_, data) => this.setState({ name: data.value })} />
        <Form.Group>
          <Form.Button disabled={this.state.name === ""} onClick={() => this.save()}>
            Create
      </Form.Button>
          <Form.Button onClick={this.props.onClose}>Cancel</Form.Button>
        </Form.Group>
      </Form>
    </div>;
  }

  save() {
    this.props.ptui.sendCommand(this.props.dispatch,
      { t: "CreateItem", path: this.props.path, name: this.state.name });
    this.props.onClose();
  }
}
export const GMCreateItem = M.connectRedux(GMCreateItemComp);


export const GMViewItem = M.connectRedux(
  function GMViewItem({ item, ptui, dispatch }: { item: T.Item } & M.ReduxProps): JSX.Element {
    const viewName = (edit: CV.ToggleFunc) =>
      <Card.Header>
        {item.name} <Icon onClick={edit} style={{ cursor: 'pointer', float: 'right' }} name='edit' />
      </Card.Header>;
    const editName = (view: CV.ToggleFunc) =>
      <Card.Header><TextInput.TextInput defaultValue={item.name} onCancel={view}
        onSubmit={input => {
          ptui.sendCommand(dispatch, { t: "EditItem", item: { ...item, name: input } });
          view();
        }} />
      </Card.Header>;
    return <Card>
      <Card.Content>
        <CV.Toggler a={viewName} b={editName} />
      </Card.Content>
      <Card.Content extra={true}>
        <div className="ui buttons">
          <Button>Give to Creature</Button>
        </div>
      </Card.Content>
    </Card>;
  });
