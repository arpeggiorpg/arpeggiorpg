/// A grab-bag of GM-only components
import * as I from 'immutable';
import * as LD from 'lodash';
import * as React from 'react';
import * as ReactRedux from 'react-redux';

import {
  Accordion, Button, Card, Dimmer, Dropdown, Form, Header, Icon, Input, Item, Label, List, Loader,
  Menu, Message, Popup, Segment, Table
} from 'semantic-ui-react';

import * as Campaign from './Campaign';
import * as CV from './CommonView';
import * as Comp from './Component';
import { CoolForm, NumericInput, PlaintextInput, Submit } from './CoolForm';
import * as Dice from './Dice';
import * as M from './Model';
import * as T from './PTTypes';
import * as TextInput from './TextInput';

export const GMMap = Comp.connect<{ map: T.Map }, {}>(_ => ({}))(
  function GMMap(props: { map: T.Map } & M.DispatchProps) {
    const { map, dispatch } = props;
    return <Segment>
      <Header> {map.name} </Header>
      <EditMap map={map} onDone={() => undefined} dispatch={dispatch} />
    </Segment>;
  });

export const GMScene = M.connectRedux(
  function GMScene({ scene, dispatch }: { scene: T.Scene } & M.ReduxProps): JSX.Element {
    return <Segment>
      <Header>
        {scene.name}
        <CV.ModalMaker
          button={open =>
            <Icon name='edit' style={{ float: 'right', cursor: 'pointer' }} onClick={open} />}
          header={<span>Edit {scene.name}</span>}
          content={close => <EditScene scene={scene} onDone={close} dispatch={dispatch} />}
        />
      </Header>
      <Accordion exclusive={false} panels={[
        { title: 'Creatures', content: <GMSceneCreatures scene={scene} /> },
        { title: 'Items', content: <GMSceneInventory scene={scene} /> },
        { title: 'Challenges', content: <GMSceneChallenges scene={scene} /> },
        { title: 'Players', content: <GMScenePlayers scene={scene} /> },
      ]} />
    </Segment>;
  });

interface CreateSceneProps { path: T.FolderPath; onDone: () => void; }
export const CreateScene = M.connectRedux(class CreateScene
  extends React.Component<CreateSceneProps & M.ReduxProps, { map?: [T.FolderPath, T.MapID] }> {
  constructor(props: CreateSceneProps & M.ReduxProps) {
    super(props);
    this.state = {};
  }
  render() {
    const { ptui } = this.props;
    const map = this.state.map ? ptui.getMap(this.state.map[1]) : undefined;
    return <div>
      <CoolForm>
        <PlaintextInput label="Name" name="name" default="" nonEmpty={true} />
        <PlaintextInput label="Background image URL" name="background_image_url" default="" />
        <CV.Toggler
          a={ok => <Campaign.MapSelector
            onSelect={(path, map_id) => { this.setState({ map: [path, map_id] }); ok(); }} />}

          b={ok => <Form.Input label="Map" onClick={ok} readOnly={true} icon='edit'
            value={`${M.folderPathToString(this.state.map![0])}/${map!.name}`} />}
        />
        <Submit disabled={!this.state.map} onClick={d => this.create(d as any)}>Create</Submit>
      </CoolForm>
    </div >;
  }
  create(data: { name: string; background_image_url: string }) {
    if (!this.state.map) { return; }
    const { path, onDone, dispatch } = this.props;
    const { name, background_image_url } = data;
    // since we don't have a visual response to setting image url/offset/scale, I'll just leave
    // default values here and the user can edit the map after creation
    dispatch(M.sendCommand({
      t: "CreateScene", path, spec: { name, background_image_url, map: this.state.map[1] },
    }));
    onDone();
  }
});

class EditScene
  extends React.Component<{ scene: T.Scene; onDone: () => void } & M.DispatchProps,
  { name: string; background_image_url: string }> {
  constructor(props: { scene: T.Scene; onDone: () => void } & M.DispatchProps) {
    super(props);
    this.state = { name: props.scene.name, background_image_url: props.scene.background_image_url };
  }
  render(): JSX.Element {
    return <Form>
      <Form.Input label="Name" value={this.state.name}
        onChange={(_, d) => this.setState({ name: d.value })} />
      <Form.Input label="Background Image URL" value={this.state.background_image_url}
        onChange={(_, d) => this.setState({ background_image_url: d.value })} />
      <Form.Button onClick={() => this.save()}>Save</Form.Button>
    </Form>;
  }
  save() {
    this.props.dispatch(
      M.sendCommand({
        t: "EditSceneDetails", scene_id: this.props.scene.id,
        details: { ...this.state, map: this.props.scene.map },
      }));
    this.props.onDone();
  }
}

class EditMap
  extends React.Component<{ map: T.Map; onDone: () => void } & M.DispatchProps,
  {
    name: string; background_image_url: string;
    scale_x: string; scale_y: string;
    offset_x: string; offset_y: string;
  }> {
  constructor(props: { map: T.Map; onDone: () => void } & M.DispatchProps) {
    super(props);
    const { map: { name, background_image_scale, background_image_offset } } = props;
    this.state = {
      name, background_image_url: props.map.background_image_url,
      scale_x: background_image_scale[0].toString(), scale_y: background_image_scale[1].toString(),
      offset_x: background_image_offset[0].toString(),
      offset_y: background_image_offset[1].toString(),
    };
  }
  render(): JSX.Element {
    const { map } = this.props;
    return <CoolForm>
      <PlaintextInput label="Name" name="name" default={map.name} nonEmpty={true} />
      <PlaintextInput label="Background Image URL" name="background_image_url"
        default={map.background_image_url} />
      <Form.Group>
        <NumericInput label="Scale X (cm)" name="scale_x" min={0}
          style={{ width: "100px" }}
          default={map.background_image_scale[0]} />
        <NumericInput label="Scale Y (cm)" name="scale_y" min={0}
          style={{ width: "100px" }}
          default={map.background_image_scale[1]} />
      </Form.Group>
      <Form.Group>
        <NumericInput label="Offset X (cm)" name="offset_x"
          style={{ width: "100px" }}
          default={map.background_image_offset[0]} />
        <NumericInput label="Offset Y (cm)" name="offset_y"
          style={{ width: "100px" }}
          default={map.background_image_offset[1]} />
      </Form.Group>
      <Submit onClick={data => this.save(data)}>Save</Submit>
    </CoolForm>;
  }
  save(data: any) {
    const { map } = this.props;
    const { name, background_image_url, scale_x, scale_y, offset_x, offset_y } = data;
    const background_image_scale: [number, number] = [scale_x, scale_y];
    const background_image_offset: [number, number] = [offset_x, offset_y];
    const details = { name, background_image_url, background_image_scale, background_image_offset };
    this.props.dispatch(M.sendCommand({ t: "EditMapDetails", id: map.id, details }));
    this.props.onDone();
  }
}

export const GMScenePlayers = M.connectRedux(
  function GMScenePlayers(props: { scene: T.Scene } & M.ReduxProps): JSX.Element {
    const { scene, ptui, dispatch } = props;
    const players_here = ptui.app.current_game.players.toArray().filter(
      player => player.scene === scene.id);
    return <List relaxed={true}>
      <List.Item>
        <Button onClick={() => moveAll()}>Set as Active Scene and move all players</Button>
      </List.Item>
      {players_here.map(player =>
        <List.Item key={player.player_id}>{player.player_id}</List.Item>)
      }
    </List>;

    function moveAll() {
      const pids = ptui.app.current_game.players.keySeq().toArray();
      const commands = pids.map(
        (player_id): T.GameCommand => ({ t: "SetPlayerScene", player_id, scene_id: scene.id }));
      commands.push({ t: "SetActiveScene", scene_id: scene.id });
      dispatch(M.sendCommands(commands));
    }
  });

export const GMSceneChallenges = M.connectRedux(
  function GMSCeneChallenges({ scene, ptui, dispatch }: { scene: T.Scene } & M.ReduxProps) {
    const challenges = scene.attribute_checks.entrySeq().sortBy(([desc, _]) => desc);
    return <List relaxed={true}>
      <List.Item key="add">
        <CV.ModalMaker
          button={open => <Icon name="add" onClick={open} style={{ cursor: 'pointer' }} />}
          header={<span>Add challenge to {scene.name}</span>}
          content={close => <AddChallengeToScene scene={scene} onClose={close} />}
        />
      </List.Item>
      {challenges.map(([description, challenge]) => {
        return <List.Item key={`challenge:${description}`}>
          <CV.ModalMaker
            button={open => <Item.Header style={{ cursor: 'pointer' }} onClick={open}>
              {description}
              <Dropdown style={{ float: 'right' }}
                icon="caret down" className="right" floating={true} pointing={true}>
                <Dropdown.Menu>
                  <Dropdown.Header content={description} />
                  <Dropdown.Item content="Delete"
                    onClick={() => ptui.sendCommand(dispatch,
                      { t: 'RemoveSceneChallenge', scene_id: scene.id, description })}
                  />
                </Dropdown.Menu>
              </Dropdown>
            </Item.Header>}
            header={<span>Challenge</span>}
            content={close => <GMChallenge scene={scene} description={description}
              challenge={challenge} onClose={close} />}
          />
          <Item.Description>{CV.describeChallenge(challenge)}</Item.Description>
        </List.Item>;
      }
      )}
    </List>;
  });

interface GMChallengeProps {
  scene: T.Scene;
  description: string;
  challenge: T.AttributeCheck;
  onClose: () => void;
}
interface GMChallengeState {
  creatures: I.Set<T.CreatureID>;
  results: I.Map<T.CreatureID, T.GameLog | string> | undefined;
}
class GMChallengeComp
  extends React.Component<GMChallengeProps & M.ReduxProps, GMChallengeState> {
  constructor(props: GMChallengeProps & M.ReduxProps) {
    super(props);
    this.state = { creatures: I.Set(), results: undefined };
  }
  render(): JSX.Element {
    const { scene, description, challenge, onClose, ptui } = this.props;
    return <div>
      <List>
        <List.Item>
          <Item.Header>{description}</Item.Header>
          <Item.Content>
            <Item.Description>{CV.describeChallenge(challenge)}</Item.Description>
          </Item.Content>
        </List.Item>
      </List>
      <Header>Select creatures to challenge</Header>
      <SelectSceneCreatures
        scene={scene}
        selections={this.state.creatures}
        add={cid => this.setState({ creatures: this.state.creatures.add(cid) })}
        remove={cid => this.setState({ creatures: this.state.creatures.delete(cid) })} />
      <Button.Group>
        <Button onClick={() => this.challenge()}>Challenge</Button>
        <Button onClick={onClose}>Cancel</Button>
      </Button.Group>
      {this.state.results === undefined ? null :

        this.state.results.count() !== this.state.creatures.count()
          ? <Loader />
          : <Table celled={true}>
            <Table.Header>
              <Table.Row>
                <Table.HeaderCell>Creature</Table.HeaderCell>
                <Table.HeaderCell>Creature's Skill</Table.HeaderCell>
                <Table.HeaderCell>Roll</Table.HeaderCell>
                <Table.HeaderCell>Success?</Table.HeaderCell>
              </Table.Row>
            </Table.Header>
            <Table.Body>
              {this.state.results.entrySeq().toArray().map(([creature_id, result]) => {
                const creature = ptui.getCreature(creature_id);
                const creature_name = creature ? creature.name : "Creature disappeared!";
                const creature_skill_level = creature
                  ? creature.attributes.get(challenge.attr)
                  : 'Creature does not have attribute';
                return <Table.Row key={creature_id}>
                  <Table.Cell>{creature_name}</Table.Cell>
                  <Table.Cell>{creature_skill_level}</Table.Cell>
                  {typeof result === 'string'
                    ? <Table.Cell colSpan={2}>{result.toString()}</Table.Cell>
                    : result.t !== 'AttributeCheckResult'
                      ? <Table.Cell colSpan={2}>BUG: Unexpected GameLog result</Table.Cell>
                      : [
                        <Table.Cell key='roll'>{result.actual}</Table.Cell>,
                        <Table.Cell key='success'>{result.success ? '😃' : '😡'}</Table.Cell>]
                  }
                </Table.Row>;
              }
              )}
            </Table.Body>
          </Table>
      }
    </div>;
  }
  challenge() {
    const { ptui } = this.props;
    const promises: Array<Promise<[T.CreatureID, T.GameLog | string]>> =
      this.state.creatures.toArray().map(
        creature_id => ptui.sendCommandWithResult(
          { t: 'AttributeCheck', creature_id, check: this.props.challenge })
          .then(
          (result): [T.CreatureID, T.GameLog | string] => {
            if (result.t !== 'Ok') {
              return [creature_id, result.error];
            } else {
              if (result.result.length !== 1) {
                return [creature_id, "Got unexpected results"];
              } else {
                return [creature_id, result.result[0]];
              }
            }
          }
          ));
    const gathered: Promise<Array<[T.CreatureID, T.GameLog | string]>> = Promise.all(promises);
    this.setState({ results: I.Map() as I.Map<T.CreatureID, T.GameLog> });
    gathered.then(rpi_results => this.setState({ results: I.Map(rpi_results) }));
  }
}

const GMChallenge = M.connectRedux(GMChallengeComp);

class AddChallengeToSceneComp extends React.Component<
  { scene: T.Scene; onClose: () => void } & M.ReduxProps,
  { description: string; attr: T.AttrID; reliable: boolean; target: T.SkillLevel }> {
  constructor(props: { scene: T.Scene; onClose: () => void } & M.ReduxProps) {
    super(props);
    this.state = { description: '', attr: 'strength', reliable: false, target: 'Inept' };
  }
  render() {
    const { onClose } = this.props;
    // TODO: Store attributes on the Game and stop hardcoding them here
    const attr_options = ['strength', 'finesse', 'magic', 'perception'].map(attr =>
      ({ key: attr, text: LD.capitalize(attr), value: attr }));
    const skill_level_options = T.SKILL_LEVELS.map(level =>
      ({ key: level, text: level, value: level }));
    return <Form>
      <Form.Input label="Description" onChange={(_, d) => this.setState({ description: d.value })} />
      <Form.Group>
        <Form.Select label="Attribute" options={attr_options}
          onChange={(_, d) => this.setState({ attr: d.value as string })} />
        <Form.Select label="Difficulty" options={skill_level_options}
          value={this.state.target}
          onChange={(_, d) => this.setState({ target: d.value as T.SkillLevel })} />
      </Form.Group>
      <Form.Checkbox label='Reliable?' checked={this.state.reliable}
        onChange={(_, d) => this.setState({ reliable: d.checked as boolean })} />
      <Form.Group>
        <Form.Button onClick={() => this.save()}>Save</Form.Button>
        <Form.Button onClick={onClose}>Cancel</Form.Button>
      </Form.Group>
    </Form >;
  }

  save() {
    const { scene, onClose, ptui, dispatch } = this.props;
    const challenge = {
      attr: this.state.attr, target: this.state.target, reliable: this.state.reliable,
    };
    ptui.sendCommand(dispatch,
      {
        t: 'AddSceneChallenge', scene_id: scene.id, description: this.state.description, challenge,
      });
    onClose();
  }
}

const AddChallengeToScene = M.connectRedux(AddChallengeToSceneComp);

export const GMSceneInventory = M.connectRedux(
  function GMSceneInventory({ scene, ptui }: { scene: T.Scene } & M.ReduxProps) {
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
  function EditableNumericLabel(props: { value: number; save: (num: number) => void }) {
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
    props: { scene: T.Scene; item: T.Item; count: number } & M.ReduxProps) {
    const { scene, item, count, ptui, dispatch } = props;
    return <EditableNumericLabel value={count} save={save} />;

    function save(num: number) {
      ptui.sendCommand(dispatch,
        { t: 'SetItemCount', owner: { Scene: scene.id }, item_id: item.id, count: num });
    }
  });

export const CreatureItemCountEditor = M.connectRedux(
  function CreatureItemCountEditor(
    props: { creature: T.Creature; item: T.Item; count: number } & M.ReduxProps) {
    const { creature, item, count, ptui, dispatch } = props;
    return <EditableNumericLabel value={count} save={save} />;

    function save(num: number) {
      ptui.sendCommand(dispatch,
        { t: 'SetItemCount', owner: { Creature: creature.id }, item_id: item.id, count: num });
    }
  });

export const GiveItemFromScene = M.connectRedux(
  function GiveItemFromScene(
    props: { scene: T.Scene; item: T.Item; onClose: () => void } & M.ReduxProps) {
    const { scene, item, onClose, ptui, dispatch } = props;
    const available_count = scene.inventory.get(item.id);
    if (!available_count) { return <div>Lost item {item.name}!</div>; }
    return <CV.TransferItemsToRecipientForm
      available_count={available_count}
      available_recipients={ptui.getSceneCreatures(scene)}
      onGive={give}
      onClose={onClose} />;
    function give(recip: T.Creature, count: number) {
      ptui.sendCommand(dispatch, {
        t: "TransferItem", from: { Scene: scene.id },
        to: { Creature: recip.id }, item_id: item.id, count,
      });
      onClose();
    }
  });

export const AddItemsToScene = M.connectRedux(
  function AddItemsToScene(props: { scene: T.Scene; onClose: () => void } & M.ReduxProps) {
    const { scene, onClose, ptui, dispatch } = props;
    return <Campaign.MultiItemSelector require_selected={scene.inventory.keySeq().toSet()}
      on_selected={item_ids => {
        const new_items = item_ids.subtract(scene.inventory.keySeq().toSet());
        for (const item_id of new_items.toArray()) {
          ptui.sendCommand(dispatch,
            { t: "SetItemCount", owner: { Scene: scene.id }, item_id, count: 1 });
        }
        onClose();
      }}
      on_cancel={onClose}
    />;
  });

export const AddItemsToCreature = M.connectRedux(
  function AddItemsToCreature(props: { creature: T.Creature; onClose: () => void } & M.ReduxProps) {
    const { creature, onClose, ptui, dispatch } = props;
    return <Campaign.MultiItemSelector require_selected={creature.inventory.keySeq().toSet()}
      on_selected={item_ids => {
        const new_items = item_ids.subtract(creature.inventory.keySeq().toSet());
        for (const item_id of new_items.toArray()) {
          ptui.sendCommand(dispatch,
            { t: "SetItemCount", owner: { Creature: creature.id }, item_id, count: 1 });
        }
        onClose();
      }}
      on_cancel={onClose}
    />;
  });


interface GMSceneCreaturesDerivedProps {
  creatures: Array<T.Creature>;
  combat: T.Combat | undefined;
}

export const GMSceneCreatures = ReactRedux.connect(
  Comp.createDeepEqualSelector(
    [(ptui: M.PTUI): T.App => ptui.app,
    (_: any, props: { scene: T.Scene }) => props.scene],
    (app: T.App, scene: T.Scene) => ({
      creatures: M.getSceneCreatures(app, scene),
      combat: app.current_game.current_combat,
    })
  ),
  (dispatch: M.Dispatch) => ({ dispatch }),
)(
  function GMSceneCreatures(
    props: { scene: T.Scene } & GMSceneCreaturesDerivedProps & M.DispatchProps) {
    const { scene, creatures, combat, dispatch } = props;
    console.log('[EXPENSIVE:GMSceneCreatures]');
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
                const existing_cids = I.Set(scene.creatures.keySeq());
                const new_cids = cids.subtract(existing_cids).toArray();
                const removed_cids = existing_cids.subtract(cids).toArray();
                const add_commands = new_cids.map(
                  (creature_id): T.GameCommand => ({
                    t: "AddCreatureToScene", scene_id: scene.id, creature_id,
                    visibility: { t: "AllPlayers" },
                  }));
                const rem_commands = removed_cids.map(
                  (creature_id): T.GameCommand =>
                    ({ t: "RemoveCreatureFromScene", scene_id: scene.id, creature_id }));
                dispatch(M.sendCommands(LD.concat(add_commands, rem_commands)));
                toggler();
              }
              } />} />
        </List.Content>
      </List.Item>
      {creatures.map(creature => {
        const vis = scene.creatures.get(creature.id)![1]; // !: must exist in map()
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
                  dispatch(M.sendCommand(
                    {
                      t: "SetSceneCreatureVisibility", scene_id: scene.id, creature_id: creature.id,
                      visibility: new_vis,
                    }));
                }} />}
              content={vis_desc}
            />
            <Dropdown icon="caret down" className="right" floating={true} pointing={true}>
              <Dropdown.Menu>
                <Dropdown.Header content={creature.name} />
                {combat
                  && combat.scene === scene.id
                  && !M.creatureIsInCombat(combat, creature.id)
                  ? <Dropdown.Item content="Add to Combat" onClick={() => addToCombat(creature)} />
                  : null
                }
              </Dropdown.Menu>
            </Dropdown>
          </List.Content>
        </List.Item>;
      }
      )}
    </List>;

    function addToCombat(creature: T.Creature) {
      dispatch(M.sendCommand({ t: 'AddCreatureToCombat', creature_id: creature.id }));
    }
  });

export const GMCombat = M.connectRedux(
  function GMCombat({ ptui, dispatch }): JSX.Element {
    const combat = ptui.app.current_game.current_combat;
    if (!combat) {
      const scene = ptui.focused_scene();
      const startCombat = scene
        ? <StartCombat scene={scene} />
        : <div>Load a scene to start a combat.</div>;
      return startCombat;
    }

    return <div>
      <GMCombatHeader combat={combat} />
      <CV.Combat combat={combat} card={GMCombatCreatureCard} initiative={initiative} />
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

  componentWillReceiveProps(nextProps: { scene: T.Scene } & M.ReduxProps) {
    const scene_creatures = nextProps.scene.creatures.keySeq().toSet();
    // Clear out old creatures that aren't in this scene
    this.setState({ selected: this.state.selected.intersect(scene_creatures) });
  }

  render(): JSX.Element {
    const { scene, ptui, dispatch } = this.props;
    return <div>
      <Button
        onClick={() => ptui.sendCommand(dispatch,
          { t: "StartCombat", scene_id: scene.id, creature_ids: this.state.selected.toArray() })}
      >Start combat</Button>
      <SelectSceneCreatures scene={scene}
        selections={this.state.selected}
        add={cid => this.setState({ selected: this.state.selected.add(cid) })}
        remove={cid => this.setState({ selected: this.state.selected.delete(cid) })} />
    </div>;

  }
}

const StartCombat = M.connectRedux(StartCombatComp);

interface SelectSceneCreaturesProps {
  scene: T.Scene;
  add: (cid: T.CreatureID) => void;
  remove: (cid: T.CreatureID) => void;
  selections: I.Set<T.CreatureID>;
}
const SelectSceneCreatures = M.connectRedux(
  function SelectSceneCreatures(props: SelectSceneCreaturesProps & M.ReduxProps): JSX.Element {
    const { scene, add, remove, selections, ptui } = props;
    const creatures = ptui.getSceneCreatures(scene);
    return <List relaxed={true}>
      {
        creatures.map(creature =>
          <List.Item key={creature.id} style={{ display: "flex", flexDirection: "row" }}>
            <input type="checkbox" checked={selections.includes(creature.id)}
              onChange={nv => nv.currentTarget.checked ? add(creature.id) : remove(creature.id)} />
            {CV.classIcon(creature)}
            {creature.name}
          </List.Item>)
      }</List>;
  });


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
export function GMCreatureCard(props: { creature: T.Creature; menu_items?: Array<JSX.Element> }
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
      const details = { ...M.getCreatureDetails(creature), note };
      ptui.sendCommand(dispatch, { t: "EditCreatureDetails", creature_id: creature.id, details });
    }
  });

export const CreateFolder = M.connectRedux(
  function CreateFolder(props: { path: T.FolderPath; onDone: () => void } & M.ReduxProps) {
    const { path, onDone, ptui, dispatch } = props;
    return <CV.SingleInputForm buttonText="Create Folder" onSubmit={create} />;

    function create(input: string) {
      const new_path = path.slice();
      new_path.push(input);
      onDone();
      return ptui.sendCommand(dispatch, { t: 'CreateFolder', path: new_path });
    }
  });

export function CreateMap(props: { path: T.FolderPath; onDone: () => void } & M.DispatchProps) {
  const { path, onDone, dispatch } = props;
  return <CV.SingleInputForm buttonText="Create Map" onSubmit={create} />;
  function create(input: string) {
    // since we don't have a visual response to setting image url/offset/scale, I'll just leave
    // default values here and the user can edit the map after creation
    const map = {
      name: input,
      background_image_url: "",
      background_image_offset: [0, 0] as [number, number],
      background_image_scale: [0, 0] as [number, number],
    };
    dispatch(M.sendCommand({ t: "CreateMap", path, map }));
    onDone();
  }
}


interface GMCreateCreatureProps {
  path: T.FolderPath;
  onClose: () => void;
}
export const CreateCreature = M.connectRedux(
  function CreateCreature(props: GMCreateCreatureProps & M.ReduxProps) {
    const { path, ptui, dispatch } = props;
    const init: T.Dice = { t: "Expr", num: 1, size: 20 };
    const creature_data = {
      name: "", note: "", bio: "", portrait_url: "", initiative: init, class_: "",
      size: { x: 1, y: 1, z: 1 }, icon_url: "",
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
      const details = {
        name: creature_data.name, class_: creature_data.class_,
        note: creature_data.note, bio: creature_data.bio,
        portrait_url: creature_data.portrait_url,
        icon_url: creature_data.icon_url,
        initiative: creature_data.initiative,
        size: creature_data.size,
      };
      ptui.sendCommand(dispatch, { t: "EditCreatureDetails", creature_id: creature.id, details });
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
  {
    name: string; portrait_url: string; note: string; bio: string;
    initiative_string: string; class_: string;
    size: number; icon_url: string;
  }> {
  constructor(props: EditCreatureDataProps & M.ReduxProps) {
    super(props);
    this.state = {
      portrait_url: props.creature.portrait_url, name: props.creature.name,
      note: props.creature.note,
      bio: props.creature.bio,
      initiative_string: Dice.format(props.creature.initiative),
      class_: props.creature.class_,
      size: props.creature.size.x,
      icon_url: props.creature.icon_url,
    };
  }

  render(): JSX.Element {
    const { onClose, ptui } = this.props;
    const parsed_initiative = Dice.maybeParse(this.state.initiative_string);
    const classes = ptui.app.current_game.classes.keySeq().toArray().map((className: string) =>
      ({ key: className, text: className, value: className }));
    const form_ok = (
      parsed_initiative.status
      && ptui.app.current_game.classes.has(this.state.class_)
    );
    return <Form error={!parsed_initiative.status}>
      <Form.Group>
        <Form.Field style={{ flex: "3" }}>
          <Form.Input label="Name" value={this.state.name}
            onChange={(_, data) => this.setState({ name: data.value })} />
        </Form.Field>
        <Form.Field style={{ flex: 2 }}>
          <Form.Select label='Class' value={this.state.class_}
            options={classes} placeholder='Class'
            onChange={(_, data) => this.setState({ class_: data.value as string })} />
        </Form.Field>
      </Form.Group>
      <Form.Group style={{ width: "100%" }}>
        <Form.Field style={{ flex: "1" }}>
          <label>Icon Image URL:</label>
          <Input fluid={true}
            value={this.state.icon_url}
            onChange={(_, data) => this.setState({ icon_url: data.value })} />
        </Form.Field>
        {this.state.icon_url ? <CV.SquareImageIcon url={this.state.icon_url} />
          : "Enter an URL for preview"}
      </Form.Group>
      <Form.Group style={{ width: "100%" }}>
        <Form.Field style={{ flex: "1" }}>
          <label>Portrait Image URL:</label>
          <Input fluid={true}
            value={this.state.portrait_url}
            onChange={(_, data) => this.setState({ portrait_url: data.value })} />
        </Form.Field>
        {this.state.portrait_url ? <img src={this.state.portrait_url} />
          : "Enter an URL for preview"}
      </Form.Group>
      <Form.Input label="Note" value={this.state.note}
        onChange={(_, data) => this.setState({ note: data.value })} />
      <Form.Group>
        <Form.Field style={{ flex: 3 }}>
          <Form.Input label="Initiative" error={!parsed_initiative.status}
            value={this.state.initiative_string}
            onChange={(_, data) => this.setState({ initiative_string: data.value })} />
        </Form.Field>
        <Form.Field style={{ flex: 2 }}>
          <Form.Select label="Size"
            value={this.state.size}
            onChange={(_, data) => this.setState({ size: Number(data.value) })}
            options={[{ key: 'medium', text: 'Medium', value: 1 },
            { key: 'large', text: 'Large', value: 2 },
            { key: 'huge', text: 'Huge', value: 3 },
            ]} />
        </Form.Field>
      </Form.Group>
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
      <Form.TextArea label="Bio" value={this.state.bio}
        onChange={(_, data) => data.value && this.setState({ bio: data.value })} />

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
      portrait_url: this.state.portrait_url,
      note: this.state.note, bio: this.state.bio,
      initiative: Dice.parse(this.state.initiative_string),
      size: { x: this.state.size, y: this.state.size, z: this.state.size },
      icon_url: this.state.icon_url,
    };
    this.props.onSave(creature);
    this.props.onClose();
  }
}

const EditCreatureData = M.connectRedux(EditCreatureDataComp);

interface GMCreateItemProps { path: T.FolderPath; onClose: () => void; }
class GMCreateItemComp extends React.Component<GMCreateItemProps & M.ReduxProps> {
  render(): JSX.Element {
    return <CV.SingleInputForm buttonText="Create" onSubmit={input => this.save(input)} />;
  }

  save(name: string) {
    this.props.ptui.sendCommand(this.props.dispatch,
      { t: "CreateItem", path: this.props.path, name });
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


export function SavedGames(): JSX.Element {
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
}

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

export function CreatureFocus({ creature }: { creature: T.Creature }) {
  return <div>
    <GMCreatureCard creature={creature} />
    <GMCreatureInventory creature={creature} />
    <Segment>{creature.bio}</Segment>
  </div>;
}
