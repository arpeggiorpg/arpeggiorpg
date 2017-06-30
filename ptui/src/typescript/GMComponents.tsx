/// A grab-bag of GM-only components
import * as I from 'immutable';
import * as React from 'react';

import * as Dice from './Dice';

import {
  Accordion, Button, Card, Dimmer, Dropdown, Form, Header, Icon, Input, Label, List, Loader,
  Menu, Message, Modal, Popup, Segment
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
      <Accordion exclusive={false} panels={[
        { title: 'Creatures', content: <GMSceneCreatures scene={scene} /> },
        { title: 'Items', content: <GMSceneInventory scene={scene} /> }
      ]} />
    </Segment>;
  });

export const GMSceneInventory = M.connectRedux(
  function GMSceneInventory({ scene, ptui, dispatch }: { scene: T.Scene } & M.ReduxProps) {
    return <div>
      <List relaxed={true}>
        <List.Item key="add">
          <List.Content>
            <CV.ModalMaker
              button={open => <Icon name="add" onClick={open} style={{ cursor: 'pointer' }} />}
              header={<span>Add items to {scene.name}</span>}
              content={close => <AddItemsToScene scene={scene} onClose={close} />} />
          </List.Content>
        </List.Item>
        {ptui.getSceneInventory(scene).map(([item, count]) =>
          <List.Item key={`item:${item.id}`}>
            {item.name}
            <div style={{ float: 'right', display: 'flex' }}>
              <SceneItemCountEditor scene={scene} item={item} count={count} />
              <Dropdown
                icon="caret down" className="right" floating={true} pointing={true}>
                <Dropdown.Menu>
                  <Dropdown.Header content={item.name} />
                  <CV.ModalMaker
                    button={toggler => <Dropdown.Item onClick={toggler} content="Give" />}
                    header={<span>Give {item.name} from {scene.name}</span>}
                    content={close =>
                      <GiveItemFromScene scene={scene} item={item} onClose={close} />} />
                </Dropdown.Menu>
              </Dropdown>
            </div>
          </List.Item>
        )}
      </List>
    </div>;
  });

export const GMCreatureInventory = M.connectRedux(
  function CreatureInventory({ creature, ptui }: { creature: T.Creature } & M.ReduxProps)
    : JSX.Element {
    const inv = creature.inventory;
    const items = ptui.getItems(inv.keySeq().toArray());

    return <List relaxed={true}>
      <List.Item key="add">
        <CV.ModalMaker
          button={pop => <Button onClick={pop}>Add</Button>}
          header={<span>Add items to {creature.name}</span>}
          content={close => <AddItemsToCreature creature={creature} onClose={close} />}
        />
      </List.Item>
      {items.map(item => {
        const count = inv.get(item.id);
        if (!count) { return; }
        return <List.Item key={`item:${item.id}`}>
          {item.name}
          <div style={{ float: 'right', display: 'flex' }}>
            <CreatureItemCountEditor creature={creature} item={item} count={count} />
            <Dropdown icon='caret down'
              className='right' pointing={true} floating={true}>
              <Dropdown.Menu>
                <Dropdown.Header content={item.name} />
                <CV.ModalMaker
                  button={open => <Dropdown.Item onClick={open} content='Give' />}
                  header={<span>Give {item.name}</span>}
                  content={close => <CV.GiveItem giver={creature} item={item} onClose={close} />} />
              </Dropdown.Menu>
            </Dropdown>
          </div>
        </List.Item>;
      }
      )}
    </List>;

  }
);


export const EditableNumericLabel = M.connectRedux(
  function EditableNumericLabel(props: { value: number, save: (num: number) => void; }) {
    const { value, save } = props;
    const edit = (to_view: CV.ToggleFunc) =>
      <TextInput.TextInput defaultValue={value.toString()} numbersOnly={true}
        onSubmit={input => { save(Number(input)); to_view(); }} onCancel={to_view}
      />;
    const view = (to_edit: CV.ToggleFunc) =>
      <Label circular={true} onClick={to_edit} style={{ cursor: "pointer" }}>{value}</Label>;
    return <CV.Toggler a={view} b={edit} />;
  }
);

export const SceneItemCountEditor = M.connectRedux(
  function SceneItemCountEditor(
    props: { scene: T.Scene, item: T.Item, count: number } & M.ReduxProps) {
    const { scene, item, count, ptui, dispatch } = props;
    return <EditableNumericLabel value={count} save={save} />;

    function save(num: number) {
      const new_inv = num <= 0 ? scene.inventory.delete(item.id) : scene.inventory.set(item.id, num);
      const newScene = { ...scene, inventory: scene.inventory.set(item.id, num) };
      ptui.sendCommand(dispatch, { t: 'EditScene', scene: { ...scene, inventory: new_inv } });
    }
  });

export const CreatureItemCountEditor = M.connectRedux(
  function CreatureItemCountEditor(
    props: { creature: T.Creature, item: T.Item, count: number } & M.ReduxProps) {
    const { creature, item, count, ptui, dispatch } = props;
    return <EditableNumericLabel value={count} save={save} />;

    function save(num: number) {
      const new_inv = num <= 0 ? creature.inventory.delete(item.id)
        : creature.inventory.set(item.id, num);
      const newCreature = { ...creature, inventory: creature.inventory.set(item.id, num) };
      ptui.sendCommand(dispatch, {
        t: 'EditCreature', creature: { ...creature, inventory: new_inv },
      });
    }
  });

export const GiveItemFromScene = M.connectRedux(
  function GiveItemFromScene(
    props: { scene: T.Scene; item: T.Item; onClose: () => void; } & M.ReduxProps) {
    const { scene, item, onClose, ptui, dispatch } = props;
    const available_count = scene.inventory.get(item.id);
    if (!available_count) { return <div>Lost item {item.name}!</div>; }
    return <CV.TransferItemsToRecipientForm item={item}
      available_count={available_count}
      available_recipients={ptui.getSceneCreatures(scene)}
      onGive={give}
      onClose={onClose} />;
    function give(recip: T.Creature, count: number) {
      const newScene = {
        ...scene,
        inventory: M.removeFromInventory(scene.inventory, item.id, count),
      };
      const newCreature = {
        ...recip,
        inventory: M.addToInventory(recip.inventory, item.id, count),
      };
      ptui.sendCommand(dispatch, { t: "EditScene", scene: newScene });
      ptui.sendCommand(dispatch, { t: "EditCreature", creature: newCreature });
      onClose();
    }

  });

export const AddItemsToScene = M.connectRedux(
  function AddItemsToScene(props: { scene: T.Scene, onClose: () => void } & M.ReduxProps) {
    const { scene, onClose, ptui, dispatch } = props;
    return <Campaign.MultiItemSelector require_selected={scene.inventory.keySeq().toSet()}
      on_selected={item_ids => {
        const inventory = scene.inventory.mergeWith((o, n) => o,
          I.Map(item_ids.map((iid): [T.ItemID, number] => [iid, 1])));
        ptui.sendCommand(dispatch, { t: "EditScene", scene: { ...scene, inventory } });
        onClose();
      }}
      on_cancel={onClose}
    />;
  });

export const AddItemsToCreature = M.connectRedux(
  function AddItemsToCreature(props: { creature: T.Creature, onClose: () => void } & M.ReduxProps) {
    const { creature, onClose, ptui, dispatch } = props;
    return <Campaign.MultiItemSelector require_selected={creature.inventory.keySeq().toSet()}
      on_selected={item_ids => {
        const inventory = creature.inventory.mergeWith((o, n) => o,
          I.Map(item_ids.map((iid): [T.ItemID, number] => [iid, 1])));
        ptui.sendCommand(dispatch, { t: "EditCreature", creature: { ...creature, inventory } });
        onClose();
      }}
      on_cancel={onClose}
    />;
  });

export const GMSceneCreatures = M.connectRedux(
  function GMSceneCreatures({ scene, ptui, dispatch }: { scene: T.Scene } & M.ReduxProps) {
    return <List relaxed={true}>
      <List.Item key="add">
        <List.Content>
          <CV.ModalMaker
            button={toggler =>
              <Icon name="edit" onClick={toggler} style={{ cursor: "pointer" }} />}
            header={<span>Change creatures in {scene.name}</span>}
            content={toggler => <Campaign.MultiCreatureSelector
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
              } />} />
        </List.Content>
      </List.Item>
      {ptui.getSceneCreatures(scene).map(creature => {
        const [pos, vis] = scene.creatures.get(creature.id)!; // !: must exist in map()
        const vis_desc = vis.t === 'GMOnly'
          ? 'Only visible to the GM' : 'Visible to all players';
        return <List.Item key={`cid:${creature.id}`}>
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
    </List>;
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
        header={<span>Edit {props.creature.name}</span>}
        content={toggler => <GMEditCreature creature={props.creature} onClose={toggler} />} />
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
    return <EditCreatureData creature={creature_data}
      onSave={cdata => save(cdata)} onClose={props.onClose} />;

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
    return <EditCreatureData creature={creature} onSave={c => save(c)} onClose={onClose} />;

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
    return <Form>
      <Form.Input label="Name" value={this.state.name}
        onChange={(_, data) => this.setState({ name: data.value })} />
      <Form.Group>
        <Form.Button disabled={this.state.name === ""} onClick={() => this.save()}>
          Create
      </Form.Button>
        <Form.Button onClick={this.props.onClose}>Cancel</Form.Button>
      </Form.Group>
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


export const SavedGames = M.connectRedux(
  function SavedGames(props: M.ReduxProps): JSX.Element {
    return <Button.Group>
      <CV.ModalMaker
        button={open => <Button onClick={open}>Load Game</Button>}
        header={<span>Load a Game</span>}
        content={close => <LoadGameForm onClose={close} />} />
      <CV.ModalMaker
        button={open => <Button onClick={open}>Save Game</Button>}
        header={<span>Save Game</span>}
        content={close => <SaveGameForm onClose={close} />} />
    </Button.Group>;
  });

export const LoadGameForm = M.connectRedux(
  function LoadGameForm(props: { onClose: () => void } & M.ReduxProps) {
    const { onClose, ptui, dispatch } = props;
    return <Form>
      <GameList onSelect={game => { ptui.loadGame(dispatch, game); onClose(); }} />
      <Form.Button onClick={onClose}>Cancel</Form.Button>
    </Form>;
  });


class SaveGameFormComp
  extends React.Component<{ onClose: () => void } & M.ReduxProps, { name: string }> {
  constructor(props: { onClose: () => void } & M.ReduxProps) {
    super(props);
    this.state = { name: "" };
  }
  render(): JSX.Element {
    const { onClose, ptui, dispatch } = this.props;
    return <Form>
      <Form.Input label="Name" value={this.state.name}
        onChange={(_, d) => this.setState({ name: d.value })} />
      <GameList onSelect={game => this.setState({ name: game })} />
      <Form.Group>
        <Form.Button disabled={this.state.name === ""}
          onClick={() => { ptui.saveGame(dispatch, this.state.name); onClose(); }}>Save</Form.Button>
        <Form.Button onClick={onClose}>Cancel</Form.Button>
      </Form.Group>
    </Form>;
  }
}
export const SaveGameForm = M.connectRedux(SaveGameFormComp);

class GameListComp
  extends React.Component<{ onSelect: (game: string) => void } & M.ReduxProps,
  { games: Array<string> | undefined }> {

  constructor(props: { onSelect: (game: string) => void } & M.ReduxProps) {
    super(props);
    this.state = { games: undefined };
  }

  componentDidMount() {
    this.props.ptui.fetchSavedGames(this.props.dispatch).then(games => this.setState({ games }));
  }

  render(): JSX.Element {
    const { onSelect } = this.props;
    if (this.state.games === undefined) {
      return <Dimmer active={true} inverted={true}>
        <Loader inverted={true}>Loading games...</Loader>
      </Dimmer>;
    } else {
      return <Menu vertical={true}>
        {this.state.games.map(name => <Menu.Item key={name} onClick={() => onSelect(name)}>
          {name}
        </Menu.Item>)}
      </Menu>;
    }
  }
}
const GameList = M.connectRedux(GameListComp);
