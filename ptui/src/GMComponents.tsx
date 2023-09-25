/// GM-only components
import {Map, Set} from 'immutable';
import sortBy from 'lodash/sortBy';
import capitalize from 'lodash/capitalize';
import * as React from 'react';
import TwitterPicker from 'react-color/lib/components/twitter/Twitter';

import {
  Button, Card, Checkbox, Dimmer, Dropdown, Form, Header, Icon, Input, Item, Label, List, Loader,
  Menu, Message, Popup, Segment, Tab, Table
} from 'semantic-ui-react';

import * as Campaign from './Campaign';
import * as CV from './CommonView';
import { CoolForm, NumericInput, PlaintextInput, Submit } from './CoolForm';
import * as Dice from './Dice';
import * as M from './Model';
import * as A from './Actions';
import * as T from './PTTypes';
import * as TextInput from './TextInput';

export function GMScene({ scene }: { scene: T.Scene }) {
  const scenePlayers = M.useState(s => s.getGame().players.count(p => p.scene === scene.id));
  const totalPlayers = M.useState(s => s.getGame().players.count());
  const player_count = `${scenePlayers}/${totalPlayers}`;
  const linked_scenes_count = scene.related_scenes.count() + scene.scene_hotspots.count();

  const panes = [
    menuItem("Background", () =>
      <EditSceneBackground scene={scene} onDone={() => undefined} />),
    menuItem("Terrain", () =>
      <SceneTerrain scene={scene} />,
      "Terrain"),
    menuItem('Highlights', () => <SceneHighlights scene={scene} />,
      "Highlights"),
    menuItem('Volumes', () => <GMSceneVolumes scene={scene} />, "Volumes"),
    menuItem('Creatures', () => <GMSceneCreatures scene={scene} />,
      undefined, scene.creatures.count().toString()),
    menuItem("Players", () => <GMScenePlayers scene={scene} />, undefined, player_count),
    menuItem('Items', () => <GMSceneInventory scene={scene} />,
      undefined, scene.inventory.count().toString()),
    menuItem('Challenges', () => <GMSceneChallenges scene={scene} />,
      undefined, scene.attribute_checks.count().toString()),
    menuItem('Linked Scenes', () => <LinkedScenes scene={scene} />, "LinkedScenes",
      linked_scenes_count.toString())
  ];

  return <Segment>
    <CV.Toggler
      a={open => <Header>
        {scene.name}
        <Icon name='edit' style={{ float: 'right', cursor: 'pointer' }} onClick={open} />
      </Header>}
      b={close => <EditSceneName scene={scene} onDone={close} />}
    />

    <Tab panes={panes}
      defaultActiveIndex={-1}
      onTabChange={(_, data) => {
        const menuItem: { menuItem: string; layer?: M.SceneLayerType } =
          data.panes![data.activeIndex as number] as any;
        // unimplemented!: disable tab-switching when Terrain is unsaved
        M.getState().setGridFocus(scene.id, menuItem.layer);
      }}
      menu={{
        size: 'small',
        secondary: true,
        style: { justifyContent: "center", flexWrap: "wrap" },
      }} />
  </Segment>;

  function menuItem(
    name: string, content: () => JSX.Element, layer?: M.SceneLayerType, detail?: string) {
    const item = detail
      ? <Menu.Item key={name}>
        {name}
        <Label basic={true} color='grey' circular={true} size='mini'>{detail}</Label>
      </Menu.Item>
      : name;
    return { menuItem: item, layer, render: content };
  }
}

interface CreateSceneProps { path: T.FolderPath; onDone: () => void; }
export function CreateScene({path, onDone}: CreateSceneProps) {
  return <div>
    <CoolForm>
      <PlaintextInput label="Name" name="name" default="" nonEmpty={true} />
      <PlaintextInput label="Background image URL" name="background_image_url" default="" />
      <Submit onClick={d => create(d as any)}>Create</Submit>
    </CoolForm>
  </div >;

  function create(data: { name: string; background_image_url: string }) {
    const { name, background_image_url } = data;
    // since we don't have a visual response to setting image url/offset/scale, I'll just leave
    // default values here and the user can edit the map after creation
    const spec = {
      name, background_image_url, background_image_offset: null,
      background_image_scale: [0, 0] as [number, number],
    };
    A.sendCommand({ t: "CreateScene", path, spec });
    onDone();
  }
}

function EditSceneName(props: { scene: T.Scene; onDone: () => void }) {
  return <TextInput.TextInput
    onSubmit={name => save(name)}
    onCancel={props.onDone}
    defaultValue={props.scene.name} />;

    function save(name: string) {
    const scene = props.scene;
    A.sendCommand({
      t: "EditSceneDetails",
      scene_id: scene.id,
      details: {
        name,
        background_image_url: scene.background_image_url,
        background_image_offset: scene.background_image_offset,
        background_image_scale: scene.background_image_scale,
      },
    });
    props.onDone();
  }
}

function EditSceneBackground({scene, onDone}: { scene: T.Scene; onDone: () => void }) {
  const [pinned, setPinned] = React.useState(scene.background_image_offset !== undefined);
  return <CoolForm>
    <PlaintextInput label="Background Image URL" name="background_image_url"
      default={scene.background_image_url} />
    <Form.Checkbox label='Pin to map' checked={pinned}
      onChange={(_, d) => setPinned(d.checked as boolean)} />
    <Form.Group>
      <NumericInput label="Scale X (cm)" name="scale_x" min={0}
        style={{ width: "100px" }}
        default={scene.background_image_scale[0]} />
      <NumericInput label="Scale Y (cm)" name="scale_y" min={0}
        style={{ width: "100px" }}
        default={scene.background_image_scale[1]} />
    </Form.Group>
    <Form.Group>
      <NumericInput label="Offset X (cm)" name="offset_x"
        style={{ width: "100px" }}
        default={scene.background_image_offset ? scene.background_image_offset[0] : 0} />
      <NumericInput label="Offset Y (cm)" name="offset_y"
        style={{ width: "100px" }}
        default={scene.background_image_offset ? scene.background_image_offset[1] : 0} />
    </Form.Group>
    <Submit onClick={save}>Save</Submit>
  </CoolForm>;

  function save(data: any) {
    const { background_image_url, scale_x, scale_y, offset_x, offset_y } = data;
    const background_image_scale: [number, number] = [scale_x, scale_y];
    const background_image_offset: [number, number] | null = pinned
      ? [offset_x, offset_y] : null;
    const details = {
      name: scene.name, background_image_url, background_image_scale, background_image_offset,
    };
    A.sendCommand({ t: "EditSceneDetails", scene_id: scene.id, details });
    onDone();
  }
}

function SceneTerrain(props: { scene: T.Scene }) {
  const { scene } = props;
  return <div>Edit the terrain on the map and then
    <Button onClick={saveTerrain}>Save</Button> or
    <Button onClick={cancelTerrain}>Cancel</Button>
  </div>;

  function saveTerrain() {
    const {gridFocus, setGridFocus} = M.getState();
    if (gridFocus?.layer?.t !== "Terrain") { return; }
    const scene_id = gridFocus.scene_id;
    const terrain = gridFocus.layer.terrain;
    A.sendCommand({ t: "EditSceneTerrain", scene_id, terrain });
    // RADIX: why am I setting focus here?
    setGridFocus(scene.id);
  }
  function cancelTerrain() {
    M.getState().setGridFocus(scene.id, "Terrain");
  }
}

function SceneHighlights(props: { scene: T.Scene }) {
  const { scene } = props;
  const allPlayers = M.useState(s => s.grid.object_visibility === "AllPlayers");
  const highlightColor = M.useState(s => s.grid.highlight_color);

  const vis_checkbox = <Checkbox label="Visible to all players?" checked={allPlayers}
    onChange={
      (_, d) => M.getState().setObjectVisibility(d.checked ? "AllPlayers" : "GMOnly")
    } />;

  return <div>
    {vis_checkbox}
    <TwitterPicker
      triangle="hide"
      color={highlightColor}
      onChange={color => M.getState().setHighlightColor(color.hex)} />
    Edit the highlights on the map and then
  <Button onClick={saveObjects}>Save</Button> or
  <Button onClick={cancelObjects}>Cancel</Button>
  </div>;

  function saveObjects() {
    const {gridFocus, setGridFocus} = M.getState();
    if (gridFocus?.layer?.t !== "Highlights") { return; }
    const scene_id = gridFocus.scene_id;
    const highlights = gridFocus.layer.highlights;
    A.sendCommand({ t: "EditSceneHighlights", scene_id, highlights });
    // RADIX: Why am I setting focus here?
    setGridFocus(scene.id);
  }
  function cancelObjects() {
    M.getState().setGridFocus(scene.id, "Highlights");
  }
}

function GMScenePlayers(props: { scene: T.Scene }) {
  const { scene } = props;
  const players_here = M.useState(
    s => s.getGame().players.valueSeq().toArray().filter(player => player.scene === scene.id)
  );
  return <List relaxed={true}>
    <List.Item>
      <Button onClick={() => moveAll()}>Set as Active Scene and move all players</Button>
    </List.Item>
    {players_here.map(player =>
      <List.Item key={player.player_id}>{player.player_id}</List.Item>)
    }
  </List>;

  function moveAll() {
    const pids = M.getState().app.current_game.players.keySeq().toArray();
    const commands: T.GameCommand[] = pids.map(
      player_id => ({ t: "SetPlayerScene", player_id, scene_id: scene.id }));
    commands.push({ t: "SetActiveScene", scene_id: scene.id });
    A.sendCommands(commands);
  }
};

function GMSceneVolumes(_: { scene: T.Scene }) {
  // TODO: add volumes manually
  return <div>Interact with volumes on the grid.</div>;
}


interface LinkedScenesProps { scene: T.Scene; }
function LinkedScenes(props: LinkedScenesProps) {
  const { scene } = props;
  const relatedScenes = M.useState(s => s.getScenes(scene.related_scenes.toArray()));
  // TODO: this is inefficient without deep-equality on scenes (I don't *think* zustand "shallow" equality will work)
  const scenes = M.useState(s => s.getGame().scenes);
  const hotspotScenes = sortBy(
    M.filterMap(scene.scene_hotspots.entrySeq().toArray(),
      ([pos, scene_id]): [T.Scene, T.Point3] | undefined => {
        const hsScene = scenes.get(scene_id);
        if (hsScene) { return [hsScene, pos]; }
      }),
    ([s, _]) => s.name);
  const focusScene = (scene: T.Scene) => () => M.getState().setGridFocus(scene.id);
  return <List>
    <List.Item>
      <List.Header>
        Related Scenes
        <CV.ModalMaker
          button={open => <List.Icon name="edit" style={{ cursor: 'pointer' }} onClick={open} />}
          header={<>Add or Remove related scenes</>}
          content={close =>
            <Campaign.MultiSceneSelector already_selected={scene.related_scenes}
              on_cancel={close}
              on_selected={related_scenes => {
                A.sendCommand(
                  { t: "EditSceneRelatedScenes", scene_id: scene.id, related_scenes }
                );
                close();
              }}
            />}
        />
      </List.Header>
    </List.Item>
    {relatedScenes.map(scene =>
      <List.Item key={`r:${scene.id}`}
        style={{ cursor: 'pointer' }}
        onClick={focusScene(scene)}>
        {scene.name}
      </List.Item>)}
    <List.Item><List.Header>Hotspot Scenes</List.Header></List.Item>
    {hotspotScenes.map(([scene, point]) =>
      <List.Item key={`h:${scene.id}`}
        style={{ cursor: 'pointer' }}
        onClick={focusScene(scene)}>
        {scene.name} ({T.encodePoint3(point)})
    </List.Item>)}
  </List>;
}

function GMSceneChallenges({ scene }: { scene: T.Scene }) {
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
                  onClick={() => A.sendCommand(
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
  }

interface GMChallengeProps {
  scene: T.Scene;
  description: string;
  challenge: T.AttributeCheck;
  onClose: () => void;
}
function GMChallenge(props: GMChallengeProps) {
  const [creatureIds, setCreatureIds] = React.useState<Set<T.CreatureID>>(Set());
  const [results, setResults] = React.useState<Map<T.CreatureID, T.GameLog | string> | undefined>(undefined);
  const { scene, description, challenge, onClose } = props;
  const creatureResults = M.useState(s => results?.entrySeq().toArray().map(([cid, result]) => ({ creature: s.getCreature(cid), result })));
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
      selections={creatureIds}
      add={cid => setCreatureIds(creatureIds.add(cid))}
      remove={cid => setCreatureIds(creatureIds.delete(cid))} />
    <Button.Group>
      {results === undefined ? <Button onClick={() => performChallenge()}>Challenge</Button> : null}
      <Button onClick={onClose}>{results === undefined ? "Cancel" : "Close"}</Button>
    </Button.Group>

    {results === undefined ? null :
      results.count() !== creatureIds.count()
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
            {creatureResults?.map(({creature, result}) => {
              const creature_name = creature ? creature.name : "Creature disappeared!";
              const creature_skill_level = creature
                ? creature.attributes.get(challenge.attr)
                : 'Creature does not have attribute';
              return <Table.Row key={creature?.id}>
                <Table.Cell>{creature_name}</Table.Cell>
                <Table.Cell>{creature_skill_level}</Table.Cell>
                {typeof result === 'string'
                  ? <Table.Cell colSpan={2}>{result}</Table.Cell>
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

  async function performChallenge() {
    const promises: Array<Promise<[T.CreatureID, T.GameLog | string]>> =
      creatureIds.toArray().map(
        creatureId => A.sendCommandWithResult(
          { t: 'AttributeCheck', creature_id: creatureId, check: challenge }
        ).then(
            (result): [T.CreatureID, T.GameLog | string] => {
              if (result.t !== 'Ok') {
                return [creatureId, result.error];
              } else {
                if (M.hasAtLeast(result.result, 1)) {
                  return [creatureId, result.result[0]];
                } else {
                  return [creatureId, "Got unexpected results"];
                }
              }
            }
          ));
    setResults(Map());
    const gathered: Array<[T.CreatureID, T.GameLog | string]> = await Promise.all(promises);
    setResults(Map(gathered));
  }
}

function AddChallengeToScene(props: { scene: T.Scene; onClose: () => void }) {
  const [description, setDescription] = React.useState('');
  const [attr, setAttr] = React.useState<T.AttrID>('strength');
  const [reliable, setReliable] = React.useState(false);
  const [target, setTarget] = React.useState<T.SkillLevel>('Inept');
  const { onClose } = props;
  // TODO: Store attributes on the Game and stop hardcoding them here
  const attr_options = ['strength', 'finesse', 'magic', 'perception'].map(attr =>
    ({ key: attr, text: capitalize(attr), value: attr }));
  const skill_level_options = T.SKILL_LEVELS.map(level =>
    ({ key: level, text: level, value: level }));
  return <Form>
    <Form.Input label="Description" onChange={(_, d) => setDescription(d.value)} />
    <Form.Group>
      <Form.Select label="Attribute" options={attr_options}
        onChange={(_, d) => setAttr(d.value as T.AttrID)} />
      <Form.Select label="Difficulty" options={skill_level_options}
        value={target}
        onChange={(_, d) => setTarget(d.value as T.SkillLevel)} />
    </Form.Group>
    <Form.Checkbox label='Reliable?' checked={reliable}
      onChange={(_, d) => setReliable(!!d.checked)} />
    <Form.Group>
      <Form.Button onClick={save}>Save</Form.Button>
      <Form.Button onClick={onClose}>Cancel</Form.Button>
    </Form.Group>
  </Form >;

  function save() {
    const { scene } = props;
    const challenge = { attr, target, reliable };
    A.sendCommand(
      {
        t: 'AddSceneChallenge', scene_id: scene.id, description: description, challenge,
      });
    onClose();
  }
}

function GMSceneInventory({ scene }: { scene: T.Scene }) {
  const inventory = M.useState(s => s.getSceneInventory(scene));
  return <div>
    <List relaxed={true}>
      <List.Item key="add">
        <List.Content>
          <CV.ModalMaker
            button={open =>
              <Button onClick={open}>Add</Button>}
            header={<span>Add items to {scene.name}</span>}
            content={close => <AddItemsToScene scene={scene} onClose={close} />} />
        </List.Content>
      </List.Item>
      {inventory.map(([item, count]) =>
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
}

function GMCreatureInventory({ creature }: { creature: T.Creature }) {
  const inv = creature.inventory;
  const items = M.useState(s => s.getItems(inv.keySeq().toArray()));

  return <>
    <h3>Inventory - {creature.name}</h3>
    <List relaxed={true}>
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
    </List>
  </>;
}


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


function SceneItemCountEditor(props: { scene: T.Scene; item: T.Item; count: number }) {
  const { scene, item, count } = props;
  return <EditableNumericLabel value={count} save={save} />;

  function save(num: number) {
    A.sendCommand(
      { t: 'SetItemCount', owner: { Scene: scene.id }, item_id: item.id, count: num });
  }
}

function CreatureItemCountEditor(props: { creature: T.Creature; item: T.Item; count: number }) {
  const { creature, item, count } = props;
  return <EditableNumericLabel value={count} save={save} />;

  function save(num: number) {
    A.sendCommand(
      { t: 'SetItemCount', owner: { Creature: creature.id }, item_id: item.id, count: num });
  }
}

function GiveItemFromScene(props: { scene: T.Scene; item: T.Item; onClose: () => void }) {
  const { scene, item, onClose } = props;
  const available_count = scene.inventory.get(item.id);
  if (!available_count) { return <div>Lost item {item.name}!</div>; }
  const availableRecipients = M.useState(s => s.getSceneCreatures(scene));
  return <CV.TransferItemsToRecipientForm
    available_count={available_count}
    available_recipients={availableRecipients}
    onGive={give}
    onClose={onClose} />;
  function give(recip: T.Creature, count: number) {
    A.sendCommand( {
      t: "TransferItem", from: { Scene: scene.id },
      to: { Creature: recip.id }, item_id: item.id, count,
    });
    onClose();
  }
}

function AddItemsToScene(props: { scene: T.Scene; onClose: () => void }) {
  const { scene, onClose } = props;
  return <Campaign.MultiItemSelector require_selected={scene.inventory.keySeq().toSet()}
    on_selected={item_ids => {
      const new_items = item_ids.subtract(scene.inventory.keySeq().toSet());
      for (const item_id of new_items.toArray()) {
        A.sendCommand(
          { t: "SetItemCount", owner: { Scene: scene.id }, item_id, count: 1 });
      }
      onClose();
    }}
    on_cancel={onClose}
  />;
}

function AddItemsToCreature(props: { creature: T.Creature; onClose: () => void }) {
  const { creature, onClose } = props;
  return <Campaign.MultiItemSelector require_selected={creature.inventory.keySeq().toSet()}
    on_selected={item_ids => {
      const new_items = item_ids.subtract(creature.inventory.keySeq().toSet());
      for (const item_id of new_items.toArray()) {
        A.sendCommand(
          { t: "SetItemCount", owner: { Creature: creature.id }, item_id, count: 1 });
      }
      onClose();
    }}
    on_cancel={onClose}
  />;
}


interface GMSceneCreaturesDerivedProps {
  creatures: Array<T.Creature>;
  combat: T.Combat | undefined;
}

// const GMSceneCreaturesSelector = Comp.createDeepEqualSelector(
//   [
//     (ptui: M.PTUI): T.App => ptui.app,
//     (_: any, props: { scene: T.Scene }) => props.scene
//   ],
//   (app: T.App, scene: T.Scene) => ({
//     creatures: M.getSceneCreatures(app, scene),
//     combat: app.current_game.current_combat,
//   })
// );

function GMSceneCreatures(props: { scene: T.Scene }) {
  const { scene } = props;

  const creatures = M.useState(s => s.getSceneCreatures(scene).map(c => ({creature: c, inCombat: s.creatureIsInCombat(c.id)})));
  const isCombatScene = M.useState(s => s.getCombat()?.scene === scene.id);

  console.log('[EXPENSIVE:GMSceneCreatures]');
  return <List relaxed={true}>
    <List.Item key="add">
      <List.Content>
        <CV.ModalMaker
          button={toggler => <Button onClick={toggler}>Add or Remove</Button>}
          header={<span>Change creatures in {scene.name}</span>}
          content={toggler => <Campaign.MultiCreatureSelector
            already_selected={scene.creatures.keySeq().toSet()}
            on_cancel={toggler}
            on_selected={cids => {
              const existing_cids = Set(scene.creatures.keySeq());
              const new_cids = cids.subtract(existing_cids).toArray();
              const removed_cids = existing_cids.subtract(cids).toArray();
              const add_commands = new_cids.map(
                (creature_id): T.GameCommand => ({
                  t: "AddCreatureToScene", scene_id: scene.id, creature_id,
                  visibility: "AllPlayers",
                }));
              const rem_commands = removed_cids.map(
                (creature_id): T.GameCommand =>
                  ({ t: "RemoveCreatureFromScene", scene_id: scene.id, creature_id }));
              A.sendCommands(add_commands.concat(rem_commands));
              toggler();
            }
            } />} />
      </List.Content>
    </List.Item>
    {creatures.map(({creature, inCombat}) => {
      const vis = scene.creatures.get(creature.id)![1]; // !: must exist in map()
      const vis_desc = vis === 'GMOnly'
        ? 'Only visible to the GM' : 'Visible to all players';
      return <List.Item key={`cid:${creature.id}`}>
        <List.Content floated='left'><CV.ClassIcon class_id={creature.class_} /></List.Content>
        {creature.name}
        <List.Content floated='right'>
          <Popup
            trigger={<Icon name='eye'
              style={{ cursor: "pointer", color: vis === 'GMOnly' ? 'grey' : 'black' }}
              onClick={() => {
                const new_vis: T.Visibility =
                  vis === "GMOnly" ? "AllPlayers" : "GMOnly";
                  console.log("CLICK", new_vis);
                A.sendCommand({
                  t: "SetSceneCreatureVisibility", scene_id: scene.id, creature_id: creature.id,
                  visibility: new_vis,
                });
              }} />}
            content={vis_desc}
          />
          <Dropdown icon="caret down" className="right" floating={true} pointing={true}>
            <Dropdown.Menu>
              <Dropdown.Header content={creature.name} />
              {isCombatScene && inCombat
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
    A.sendCommand({ t: 'AddCreatureToCombat', creature_id: creature.id });
  }
}

export function GMCombat() {
  const combat = M.useState(s => s.getGame().current_combat);
  const scene = M.useState(s => s.getFocusedScene());
  if (!combat) {
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
    A.sendCommand( {
      t: "ChangeCreatureInitiative", creature_id: creature.id,
      init: new_init_num,
    });
  }
}


function StartCombat(props: { scene: T.Scene }) {
  const sceneCreatureIDs = M.useState(s => s.getSceneCreatures(props.scene).map(c => c.id));
  // TODO: clean up this mess. Should this be a useEffect? I hate useEffect!
  const [selected, setSelected] = React.useState<Set<T.CreatureID>>(Set(sceneCreatureIDs));


  // componentWillReceiveProps(nextProps: { scene: T.Scene } & M.ReduxProps) {
  //   const scene_creatures = nextProps.scene.creatures.keySeq().toSet();
  //   // Clear out old creatures that aren't in this scene
  //   this.setState({ selected: this.state.selected.intersect(scene_creatures) });
  // }

  const { scene } = props;
  return <div>
    <Button
      onClick={() => A.sendCommand(
        { t: "StartCombat", scene_id: scene.id, creature_ids: selected.toArray() })}
    >Start combat</Button>
    <SelectSceneCreatures scene={scene}
      selections={selected}
      add={cid => setSelected(selected.add(cid))}
      remove={cid => setSelected(selected.delete(cid))} />
  </div>;
}

interface SelectSceneCreaturesProps {
  scene: T.Scene;
  add: (cid: T.CreatureID) => void;
  remove: (cid: T.CreatureID) => void;
  selections: Set<T.CreatureID>;
}
function SelectSceneCreatures(props: SelectSceneCreaturesProps) {
  const { scene, add, remove, selections } = props;
  const creatures = M.useState(s => s.getSceneCreatures(scene));
  return <List relaxed={true}>
    {
      creatures.map(creature =>
        <List.Item key={creature.id} style={{ display: "flex", flexDirection: "row" }}>
          <input type="checkbox" checked={selections.includes(creature.id)}
            onChange={nv => nv.currentTarget.checked ? add(creature.id) : remove(creature.id)} />
          <CV.ClassIcon class_id={creature.class_} />
          {creature.name}
        </List.Item>)
    }</List>;
}


function GMCombatHeader({ combat }: { combat: T.Combat }) {
  const scene = M.useState(s => s.getScene(combat.scene));

  return <Segment>
    {
      scene
        ?
        <div><span style={{ fontWeight: "bold" }}>Scene:</span>&nbsp;
        <a href="#" onClick={() => M.getState().setGridFocus(scene.id)}>
            {scene.name}
          </a>
          <Button onClick={() => A.sendCommand( { t: "StopCombat" })}>
            Stop combat
          </Button>
        </div>
        :
        <div>Lost scene!</div>
    }
  </Segment>;
}

/// A customized CreatureCard that renders an editable note in the content area.
function GMCreatureCard(props: { creature: T.Creature; menu_items?: Array<JSX.Element> }) {
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

function GMCombatCreatureCard(props: { creature: T.Creature }) {
  const { creature } = props;
  const menu_items = [
    <Dropdown.Item key="Remove from Combat" onClick={removeFromCombat} text="Remove from Combat" />
  ];

  return <GMCreatureCard creature={creature} menu_items={menu_items} />;

  function removeFromCombat() {
    A.sendCommand( { t: "RemoveCreatureFromCombat", creature_id: creature.id });
  }
}


/// A single-line editable creature note
function CreatureNote({ creature }: { creature: T.Creature }) {
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
    const details = { ...creature.toCreation(), note };
    A.sendCommand( { t: "EditCreatureDetails", creature_id: creature.id, details });
  }
}

export function CreateFolder(props: { path: T.FolderPath; onDone: () => void }) {
  const { path, onDone } = props;
  return <CV.SingleInputForm buttonText="Create Folder" onSubmit={create} />;

  function create(input: string) {
    const new_path = path.slice();
    new_path.push(input);
    onDone();
    return A.sendCommand( { t: 'CreateFolder', path: new_path });
  }
}

interface GMCreateCreatureProps {
  path: T.FolderPath;
  onClose: () => void;
}
export function CreateCreature(props: GMCreateCreatureProps) {
  const { path } = props;
  const init: T.Dice = { Expr: {num: 1, size: 20 }};
  const creature_data = {
    name: "", note: "", bio: "", portrait_url: "", initiative: init, class: "",
    size: { x: 1, y: 1, z: 1 }, icon_url: "",
  };
  return <EditCreatureData creature={creature_data}
    onSave={cdata => save(cdata)} onClose={props.onClose} />;

  function save(cdata: T.CreatureCreation) {
    A.sendCommand( { t: "CreateCreature", path, spec: cdata });
  }
}

interface GMEditCreatureProps {
  creature: T.Creature;
  onClose: () => void;
}
function GMEditCreature(props: GMEditCreatureProps) {
  const { creature, onClose } = props;
  return <EditCreatureData creature={creature.toCreation()} onSave={c => save(c)} onClose={onClose} />;

  function save(details: T.CreatureCreation) {
    A.sendCommand( { t: "EditCreatureDetails", creature_id: creature.id, details });
    onClose();
  }
}

interface EditCreatureDataProps {
  creature: T.CreatureCreation;
  onClose: () => void;
  onSave: (cdata: T.CreatureCreation) => void;
}
function EditCreatureData(props: EditCreatureDataProps) {
  // TODO RADIX this is probably bad state management
  const [portrait_url, set_portrait_url] = React.useState(props.creature.portrait_url);
  const [name, set_name] = React.useState(props.creature.name);
  const [note, set_note] = React.useState(props.creature.note);
  const [bio, set_bio] = React.useState(props.creature.bio);
  const [initiative_string, set_initiative_string] = React.useState(Dice.format(props.creature.initiative));
  const [class_, set_class] = React.useState(props.creature.class);
  const [size, set_size] = React.useState<number>(props.creature.size.x);
  const [icon_url, set_icon_url] = React.useState(props.creature.icon_url);

  const { onClose } = props;
  const parsed_initiative = Dice.maybeParse(initiative_string);
  const classes = M.useState(s => s.getGame().classes.valueSeq().toArray().map(class_ =>
    ({
      key: class_.id,
      // it's probably not good to return elements from useState
      text: <><CV.ClassIcon class_id={class_.id} />{class_.name}</>,
      value: class_.id,
    })));
  const form_ok = parsed_initiative.status
  return (
    <Form error={!parsed_initiative.status}>
      <Form.Group>
        <Form.Field style={{flex: "3"}}>
          <Form.Input label="Name" value={name} onChange={(_, data) => set_name(data.value)} />
        </Form.Field>
        <Form.Field style={{ flex: 2 }}>
          <Form.Select label='Class' value={class_}
            options={classes} placeholder='Class'
            onChange={(_, data) => set_class(data.value as string)} />
        </Form.Field>
      </Form.Group>
      <Form.Group style={{ width: "100%" }}>
        <Form.Field style={{ flex: "1" }}>
          <label>Icon Image URL:</label>
          <Input fluid={true}
            value={icon_url}
            onChange={(_, data) => set_icon_url(data.value)} />
        </Form.Field>
        {icon_url ? <CV.SquareImageIcon url={icon_url} />
          : "Enter an URL for preview"}
      </Form.Group>
      <Form.Group style={{ width: "100%" }}>
        <Form.Field style={{ flex: "1" }}>
          <label>Portrait Image URL:</label>
          <Input fluid={true}
            value={portrait_url}
            onChange={(_, data) => set_portrait_url(data.value)} />
        </Form.Field>
        {portrait_url ? <img src={portrait_url} />
          : "Enter an URL for preview"}
      </Form.Group>
      <Form.Input label="Note" value={note}
        onChange={(_, data) => set_note(data.value)} />
      <Form.Group>
        <Form.Field style={{ flex: 3 }}>
          <Form.Input label="Initiative" error={!parsed_initiative.status}
            value={initiative_string}
            onChange={(_, data) => set_initiative_string(data.value)} />
        </Form.Field>
        <Form.Field style={{ flex: 2 }}>
          <Form.Select label="Size"
            value={size}
            onChange={(_, data) => set_size(Number(data.value))}
            options={[
              { key: 'medium', text: 'Medium', value: 1 },
              { key: 'large', text: 'Large', value: 2 },
              { key: 'huge', text: 'Huge', value: 3 },
            ]}
          />
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
      <Form.TextArea label="Bio" value={bio}
        onChange={(_, data) => data.value && set_bio(data.value as string)} />
      <Form.Group>
        <Form.Button disabled={!form_ok} onClick={save}>
          Save
        </Form.Button>
        <Form.Button onClick={onClose}>Cancel</Form.Button>
      </Form.Group>
    </Form>
  );

  function save() {
    const creature = {
      name, class: class_,
      portrait_url,
      note, bio,
      initiative: Dice.parse(initiative_string),
      size: { x: size, y: size, z: size },
      icon_url,
    };
    props.onSave(creature);
    props.onClose();
  }
}

interface GMCreateItemProps { path: T.FolderPath; onClose: () => void; }
export function GMCreateItem(props: GMCreateItemProps) {
  return <CV.SingleInputForm buttonText="Create" onSubmit={input => save(input)} />;

  function save(name: string) {
    A.sendCommand({ t: "CreateItem", path: props.path, name });
    props.onClose();
  }
}


export function GMViewItem({ itemId }: { itemId: T.ItemID }) {
  const item = M.useState(s => s.getItem(itemId));
  if (!item) return <div>Item {itemId} not found!</div>;
  const viewName = (edit: CV.ToggleFunc) =>
    <Card.Header>
      {item.name} <Icon onClick={edit} style={{ cursor: 'pointer', float: 'right' }} name='edit' />
    </Card.Header>;
  const editName = (view: CV.ToggleFunc) =>
    <Card.Header><TextInput.TextInput defaultValue={item.name} onCancel={view}
      onSubmit={input => {
        A.sendCommand( { t: "EditItem", item: { ...item, name: input } });
        view();
      }} />
    </Card.Header>;
  return <Card>
    <Card.Content>
      <CV.Toggler a={viewName} b={editName} />
    </Card.Content>
    <Card.Content extra={true}>
      <div className="ui buttons">
        <Button>Give to Creature (NYI)</Button>
      </div>
    </Card.Content>
  </Card>;
}

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
    <CV.ModalMaker
      button={open => <Button onClick={open}>New Game</Button>}
      header={<span>New Game</span>}
      content={close => <NewGame onClose={close} />}
    />
  </Button.Group>;
}

function NewGame(props: { onClose: () => void }) {
  const { onClose } = props;
  return <>
    <div>Are you sure? Your unsaved data will be lost.</div>
    <Button onClick={() => onClick()}>Do it!</Button>
  </>;

  async function onClick() {
    // TODO: spinner
    await A.newGame();
    onClose();
  }
}

function LoadGameForm(props: { onClose: () => void }) {
  const { onClose } = props;
  return <Form>
    <GameList onSelect={load} />
    <Form.Button onClick={onClose}>Cancel</Form.Button>
  </Form>;

  async function load(source: T.ModuleSource, name: string) {
    // TODO: spinner
    await A.loadGame(source, name);
    onClose();
  }
}

function SaveGameForm(props: { onClose: () => void }) {
  const { onClose } = props;
  return <SaveGameishForm onClose={onClose} save={save} />;

  async function save(name: string) {
    // TODO: spinner
    await A.saveGame(name);
    onClose();
  }
}

interface SaveGameishFormProps { onClose: () => void; save: (name: string) => void; }
function SaveGameishForm(props: SaveGameishFormProps) {
  const [name, setName] = React.useState("");
  const { onClose, save } = props;
  return <Form>
    <Form.Input label="Name" value={name}
      onChange={(_, d) => setName(d.value)} />
    <GameList gamesOnly={true} onSelect={(_, game) => setName(game)} />
    <Form.Group>
      <Form.Button disabled={name === ""}
        onClick={() => { save(name); onClose(); }}>Save</Form.Button>
      <Form.Button onClick={onClose}>Cancel</Form.Button>
    </Form.Group>
  </Form>;
}

interface GameListProps {
  onSelect: (type: T.ModuleSource, name: string) => void;
  gamesOnly?: boolean;
}

function GameList(props: GameListProps) {
  const [modules, setModules] = React.useState<string[] | undefined>(undefined);
  const [games, setGames] = React.useState<string[] | undefined>(undefined);

  // Gross :-( Should I use something like SWC or react-query?
  // I have very few of these kinds of things so maybe this is fine.
  React.useEffect(() => {
    const fetch = async () => {
      const { modules, games } = await A.fetchSavedGames();
      setGames(games);
      setModules(modules);
      console.log("saved games:", games, modules);
    };
    fetch();
  }, []);

  const { onSelect } = props;
  if (games === undefined || modules === undefined) {
    return <Dimmer active={true} inverted={true}>
      <Loader inverted={true}>Loading games...</Loader>
    </Dimmer>;
  } else {
    return <Menu vertical={true}>
      {!props.gamesOnly
        ?
        <>
          <Menu.Header>Modules</Menu.Header>
          {modules.map(name =>
            <Menu.Item key={name} onClick={() => onSelect('Module', name)}>{name}</Menu.Item>)}
        </>
        : null}
      <Menu.Header>Saved Games</Menu.Header>
      {games.map(name => <Menu.Item key={name}
        onClick={() => onSelect('SavedGame', name)}>
        {name}
      </Menu.Item>)}
    </Menu>;
  }
}

export function CreatureFocus({ creatureId }: { creatureId: T.CreatureID }) {
  const creature = M.useState(s => s.getCreature(creatureId));
  if (!creature) {
    return <div>Can't find creature {creatureId}</div>;
  }
  return <div>
    <GMCreatureCard creature={creature} />
    <GMCreatureInventory creature={creature} />
    <Segment>{creature.bio}</Segment>
  </div>;
}

export function ExportModule(props: { path: T.FolderPath; onDone: () => void }) {
  const { path, onDone } = props;
  return <div>
    <SaveGameishForm onClose={onDone} save={save} />
  </div>;
  function save(game: string) {
    A.exportModule(path, game);
  }
}

export function ImportModule(props: { path: T.FolderPath; onDone: () => void }) {
  const { path, onDone } = props;
  return <Form>
    <GameList onSelect={(source, name) => {
      const suffixed_path = path.concat(name);
      A.sendCommand({ t: "LoadModule", path: suffixed_path, name, source });
      onDone();
    }} />
    <Form.Button onClick={onDone}>Cancel</Form.Button>
  </Form>;
}


interface AddSceneHotspotProps {
  scene: T.Scene;
  pt: T.Point3;
  onClose: () => void;
}
export function AddSceneHotspot(props: AddSceneHotspotProps) {
  const { pt, onClose, scene } = props;
  return <Campaign.SceneSelector onCancel={onClose} onSelect={select} />;

  function select(sid: T.SceneID) {
    onClose();
    const scene_hotspots = scene.scene_hotspots.set(pt, sid);
    A.sendCommand({ t: "EditSceneSceneHotspots", scene_id: scene.id, scene_hotspots });
  }

}

interface AddAnnotationProps {
  scene: T.Scene;
  pt: T.Point3;
  onClose: () => void;
}

export function AddAnnotation(props: AddAnnotationProps) {
  const [allPlayers, setAllPlayers] = React.useState(false);
  const { pt, onClose, scene } = props;
  const annotate = (text: string) => {
    const vis: T.Visibility = allPlayers ?  "AllPlayers" : "GMOnly";
    const annotations = scene.annotations.set(pt, [text, vis]);
    A.sendCommand({ t: "EditSceneAnnotations", scene_id: scene.id, annotations });
    onClose();
  };
  return <>
    <Checkbox label="Visible to all players?" checked={allPlayers}
      onChange={(_, d) => setAllPlayers(d.checked === true)} />
    <CV.SingleInputForm buttonText="Annotate" onSubmit={annotate} />
  </>;
}
