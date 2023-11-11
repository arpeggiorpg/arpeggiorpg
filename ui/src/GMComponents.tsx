/// GM-only components
import { Map, Set } from "immutable";
import capitalize from "lodash/capitalize";
import sortBy from "lodash/sortBy";
import * as React from "react";
import TwitterPicker from "react-color/lib/components/twitter/Twitter";

import {
  Button,
  Card,
  Checkbox,
  Dropdown,
  Form,
  Icon,
  Input,
  List,
  Message,
  Segment,
} from "semantic-ui-react";
import * as Z from "zod";

import * as A from "./Actions";
import * as Campaign from "./Campaign";
import * as CV from "./CommonView";
import { CoolForm, PlaintextInput, Submit } from "./CoolForm";
import * as Dice from "./Dice";
import * as M from "./Model";
import * as T from "./PTTypes";
import { SelectSceneCreatures } from "./Scene";
import * as TextInput from "./TextInput";
import { EditableNumericLabel } from "./TextInput";
import { sendWSRequest } from "./wsrpi";

interface CreateSceneProps {
  path: T.FolderPath;
  onDone: () => void;
}
export function CreateScene({ path, onDone }: CreateSceneProps) {
  return (
    <div>
      <CoolForm>
        <PlaintextInput label="Name" name="name" default="" nonEmpty={true} />
        <PlaintextInput label="Background image URL" name="background_image_url" default="" />
        <Submit onClick={d => create(d as any)}>Create</Submit>
      </CoolForm>
    </div>
  );

  function create(data: { name: string; background_image_url: string }) {
    const { name, background_image_url } = data;
    // since we don't have a visual response to setting image url/offset/scale, I'll just leave
    // default values here and the user can edit the map after creation
    const spec = {
      name,
      background_image_url,
      background_image_offset: null,
      background_image_scale: [0, 0] as [number, number],
    };
    A.sendGMCommand({ t: "CreateScene", path, scene: spec });
    onDone();
  }
}

function GMCreatureInventory({ creature }: { creature: T.Creature }) {
  const inv = creature.inventory;
  const items = M.useState(s => s.getItems(inv.keySeq().toArray()));

  return (
    <>
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
          if (!count) return;
          return (
            <List.Item key={`item:${item.id}`}>
              {item.name}
              <div style={{ float: "right", display: "flex" }}>
                <CreatureItemCountEditor creature={creature} item={item} count={count} />
                <Dropdown icon="caret down" className="right" pointing={true} floating={true}>
                  <Dropdown.Menu>
                    <Dropdown.Header content={item.name} />
                    <CV.ModalMaker
                      button={open => <Dropdown.Item onClick={open} content="Give" />}
                      header={<span>Give {item.name}</span>}
                      content={close => (
                        <CV.GiveItem giver={creature} item={item} onClose={close} />
                      )}
                    />
                  </Dropdown.Menu>
                </Dropdown>
              </div>
            </List.Item>
          );
        })}
      </List>
    </>
  );
}

function CreatureItemCountEditor(props: { creature: T.Creature; item: T.Item; count: number }) {
  const { creature, item, count } = props;
  return <EditableNumericLabel value={count} save={save} />;

  function save(num: number) {
    A.sendGMCommand(
      { t: "SetItemCount", owner: { Creature: creature.id }, item_id: item.id, count: BigInt(num) },
    );
  }
}

function AddItemsToCreature(props: { creature: T.Creature; onClose: () => void }) {
  const { creature, onClose } = props;
  return (
    <Campaign.MultiItemSelector
      require_selected={creature.inventory.keySeq().toSet()}
      on_selected={item_ids => {
        const new_items = item_ids.subtract(creature.inventory.keySeq().toSet());
        for (const item_id of new_items.toArray()) {
          A.sendGMCommand(
            { t: "SetItemCount", owner: { Creature: creature.id }, item_id, count: 1n },
          );
        }
        onClose();
      }}
      on_cancel={onClose}
    />
  );
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

  return (
    <div>
      <GMCombatHeader combat={combat} />
      <CV.Combat combat={combat} card={GMCombatCreatureCard} initiative={initiative} />
    </div>
  );

  function initiative(creature: T.Creature, init: number) {
    return <CV.Toggler a={view} b={edit} />;

    function view(toggle: CV.ToggleFunc) {
      return (
        <div style={{ cursor: "pointer", textDecoration: "underline dotted" }} onClick={toggle}>
          {init}
        </div>
      );
    }
    function edit(toggle: CV.ToggleFunc) {
      return (
        <TextInput.TextInput
          defaultValue={init.toString()}
          style={{ width: "25px" }}
          numbersOnly={true}
          onCancel={toggle}
          onSubmit={input => {
            toggle();
            changeInit(creature, input);
          }}
        />
      );
    }
  }

  function changeInit(creature: T.Creature, new_init: string) {
    const initiative = Number(new_init);
    A.sendGMCommand(
      { t: "ChangeCreatureInitiative", creature_id: creature.id, initiative },
    );
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
  return (
    <div>
      <Button
        onClick={() =>
          A.sendGMCommand(
            { t: "StartCombat", scene_id: scene.id, combatants: selected.toArray() },
          )}
      >
        Start combat
      </Button>
      <SelectSceneCreatures
        scene={scene}
        selections={selected}
        add={cid => setSelected(selected.add(cid))}
        remove={cid => setSelected(selected.delete(cid))}
      />
    </div>
  );
}

function GMCombatHeader({ combat }: { combat: T.Combat }) {
  const scene = M.useState(s => s.getScene(combat.scene));

  return (
    <Segment>
      {scene
        ? (
          <div>
            <span style={{ fontWeight: "bold" }}>Scene:</span>&nbsp;
            <a href="#" onClick={() => M.getState().setGridFocus(scene.id)}>
              {scene.name}
            </a>
            <Button onClick={() => A.sendGMCommand({ t: "StopCombat" })}>
              Stop combat
            </Button>
          </div>
        )
        : <div>Lost scene!</div>}
    </Segment>
  );
}

/// A customized CreatureCard that renders an editable note in the content area.
function GMCreatureCard(props: { creature: T.Creature; menu_items?: Array<JSX.Element> }) {
  const menu = (
    <Dropdown icon="caret down" className="right" floating={true} pointing={true}>
      <Dropdown.Menu>
        <Dropdown.Header content={props.creature.name} />
        <CV.ModalMaker
          button={toggler => <Dropdown.Item onClick={toggler} content="Edit" />}
          header={<span>Edit {props.creature.name}</span>}
          content={toggler => <GMEditCreature creature={props.creature} onClose={toggler} />}
        />
        {props.menu_items}
      </Dropdown.Menu>
    </Dropdown>
  );
  return (
    <CV.CreatureCard creature={props.creature} menu={menu}>
      <CreatureNote creature={props.creature} />
    </CV.CreatureCard>
  );
}

function GMCombatCreatureCard(props: { creature: T.Creature }) {
  const { creature } = props;
  const menu_items = [
    <Dropdown.Item key="Remove from Combat" onClick={removeFromCombat} text="Remove from Combat" />,
  ];

  return <GMCreatureCard creature={creature} menu_items={menu_items} />;

  function removeFromCombat() {
    A.sendGMCommand({ t: "RemoveCreatureFromCombat", creature_id: creature.id });
  }
}

/// A single-line editable creature note
function CreatureNote({ creature }: { creature: T.Creature }) {
  function view(toggle: CV.ToggleFunc) {
    return (
      <div
        style={{ cursor: "pointer", textDecoration: "underline dotted" }}
        onClick={toggle}
      >
        {creature.note}
      </div>
    );
  }
  function edit(toggle: CV.ToggleFunc) {
    return (
      <TextInput.TextInput
        defaultValue={creature.note}
        onCancel={toggle}
        onSubmit={input => {
          toggle();
          submitNote(input);
        }}
      />
    );
  }

  return <CV.Toggler a={view} b={edit} />;

  function submitNote(note: string) {
    const details = { ...creature, note };
    A.sendGMCommand({ t: "EditCreatureDetails", creature_id: creature.id, details });
  }
}

export function CreateFolder(props: { path: T.FolderPath; onDone: () => void }) {
  const { path, onDone } = props;
  return <CV.SingleInputForm buttonText="Create Folder" onSubmit={create} />;

  function create(input: string) {
    const new_path = path.slice();
    new_path.push(input);
    onDone();
    return A.sendGMCommand({ t: "CreateFolder", path: new_path });
  }
}

interface GMCreateCreatureProps {
  path: T.FolderPath;
  onClose: () => void;
}
export function CreateCreature(props: GMCreateCreatureProps) {
  const { path } = props;
  const init: T.Dice = { Expr: { num: 1, size: 20 } };
  const creature_data = {
    name: "",
    note: "",
    bio: "",
    portrait_url: "",
    initiative: init,
    class: "",
    size: { x: 1, y: 1, z: 1 },
    icon_url: "",
  };
  return (
    <EditCreatureData
      creature={creature_data}
      onSave={cdata => save(cdata)}
      onClose={props.onClose}
    />
  );

  function save(creature: T.CreatureCreation) {
    A.sendGMCommand({ t: "CreateCreature", path, creature });
  }
}

interface GMEditCreatureProps {
  creature: T.Creature;
  onClose: () => void;
}
function GMEditCreature(props: GMEditCreatureProps) {
  const { creature, onClose } = props;
  return <EditCreatureData creature={creature} onSave={c => save(c)} onClose={onClose} />;

  function save(details: T.CreatureCreation) {
    A.sendGMCommand({ t: "EditCreatureDetails", creature_id: creature.id, details });
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
  const [initiative_string, set_initiative_string] = React.useState(
    Dice.format(props.creature.initiative),
  );
  const [class_, set_class] = React.useState(props.creature.class);
  const [size, set_size] = React.useState<number>(props.creature.size.x);
  const [icon_url, set_icon_url] = React.useState(props.creature.icon_url);

  const { onClose } = props;
  const parsed_initiative = Dice.maybeParse(initiative_string);
  const classes = M.useState(s =>
    s.getGame().classes.valueSeq().toArray().map(class_ => ({
      key: class_.id,
      // it's probably not good to return elements from useState
      text: (
        <>
          <CV.ClassIcon class_id={class_.id} />
          {class_.name}
        </>
      ),
      value: class_.id,
    }))
  );
  const form_ok = parsed_initiative.status;
  return (
    <Form error={!parsed_initiative.status}>
      <Form.Group>
        <Form.Field style={{ flex: "3" }}>
          <Form.Input label="Name" value={name} onChange={(_, data) => set_name(data.value)} />
        </Form.Field>
        <Form.Field style={{ flex: 2 }}>
          <Form.Select
            label="Class"
            value={class_}
            options={classes}
            placeholder="Class"
            onChange={(_, data) => set_class(data.value as string)}
          />
        </Form.Field>
      </Form.Group>
      <Form.Group style={{ width: "100%" }}>
        <Form.Field style={{ flex: "1" }}>
          <label>Icon Image URL:</label>
          <Input fluid={true} value={icon_url} onChange={(_, data) => set_icon_url(data.value)} />
        </Form.Field>
        {icon_url
          ? <CV.SquareImageIcon url={icon_url} />
          : "Enter an URL for preview"}
      </Form.Group>
      <Form.Group style={{ width: "100%" }}>
        <Form.Field style={{ flex: "1" }}>
          <label>Portrait Image URL:</label>
          <Input
            fluid={true}
            value={portrait_url}
            onChange={(_, data) => set_portrait_url(data.value)}
          />
        </Form.Field>
        {portrait_url
          ? <img src={portrait_url} />
          : "Enter an URL for preview"}
      </Form.Group>
      <Form.Input label="Note" value={note} onChange={(_, data) => set_note(data.value)} />
      <Form.Group>
        <Form.Field style={{ flex: 3 }}>
          <Form.Input
            label="Initiative"
            error={!parsed_initiative.status}
            value={initiative_string}
            onChange={(_, data) => set_initiative_string(data.value)}
          />
        </Form.Field>
        <Form.Field style={{ flex: 2 }}>
          <Form.Select
            label="Size"
            value={size}
            onChange={(_, data) => set_size(Number(data.value))}
            options={[
              { key: "medium", text: "Medium", value: 1 },
              { key: "large", text: "Large", value: 2 },
              { key: "huge", text: "Huge", value: 3 },
            ]}
          />
        </Form.Field>
      </Form.Group>
      {parsed_initiative.status
        ? <Message>Parsed dice as {Dice.format(parsed_initiative.value)}</Message>
        : (
          <Message error={true}>
            <Message.Header>Couldn't parse dice expression</Message.Header>
            <Message.Content>
              Expected {parsed_initiative.expected} at line {parsed_initiative.index.line}, column
              {" "}
              {parsed_initiative.index.column}
            </Message.Content>
          </Message>
        )}
      <Form.TextArea
        label="Bio"
        value={bio}
        onChange={(_, data) => data.value && set_bio(data.value as string)}
      />
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
      name,
      class: class_,
      portrait_url,
      note,
      bio,
      initiative: Dice.parse(initiative_string),
      size: { x: size, y: size, z: size },
      icon_url,
    };
    props.onSave(creature);
    props.onClose();
  }
}

interface GMCreateItemProps {
  path: T.FolderPath;
  onClose: () => void;
}
export function GMCreateItem(props: GMCreateItemProps) {
  return <CV.SingleInputForm buttonText="Create" onSubmit={input => save(input)} />;

  function save(name: string) {
    A.sendGMCommand({ t: "CreateItem", path: props.path, name });
    props.onClose();
  }
}

export function GMViewItem({ itemId }: { itemId: T.ItemID }) {
  const item = M.useState(s => s.getItem(itemId));
  if (!item) return <div>Item {itemId} not found!</div>;
  const viewName = (edit: CV.ToggleFunc) => (
    <Card.Header>
      {item.name} <Icon onClick={edit} style={{ cursor: "pointer", float: "right" }} name="edit" />
    </Card.Header>
  );
  const editName = (view: CV.ToggleFunc) => (
    <Card.Header>
      <TextInput.TextInput
        defaultValue={item.name}
        onCancel={view}
        onSubmit={input => {
          A.sendGMCommand({ t: "EditItem", item: { ...item, name: input } });
          view();
        }}
      />
    </Card.Header>
  );
  return (
    <Card>
      <Card.Content>
        <CV.Toggler a={viewName} b={editName} />
      </Card.Content>
      <Card.Content extra={true}>
        <div className="ui buttons">
          <Button>Give to Creature (NYI)</Button>
        </div>
      </Card.Content>
    </Card>
  );
}

export function CreatureFocus({ creatureId }: { creatureId: T.CreatureID }) {
  const creature = M.useState(s => s.getCreature(creatureId));
  if (!creature) {
    return <div>Can't find creature {creatureId}</div>;
  }
  return (
    <div>
      <GMCreatureCard creature={creature} />
      <GMCreatureInventory creature={creature} />
      <Segment>{creature.bio}</Segment>
    </div>
  );
}

export function ClassEditor({ classId }: { classId: T.ClassID }) {
  const class_ = M.useState(s => s.getClass(classId));
  const [classText, setClassText] = React.useState(JSON.stringify(class_, null, 2));

  // Ugh I hate useEffect :(
  React.useEffect(() => {
    setClassText(JSON.stringify(class_, null, 2));
  }, [classId]);

  return (
    <div style={{ display: "flex", flexDirection: "column" }}>
      <textarea
        style={{ height: "200px" }}
        onChange={e => setClassText(e.target.value)}
        value={classText}
      />
      <button onClick={saveClass}>Save</button>
    </div>
  );

  function saveClass() {
    A.sendGMCommand({ t: "EditClass", class: T.decodeClass.parse(JSON.parse(classText)) });
  }
}

export function AbilityEditor({ abilityId }: { abilityId: T.ClassID }) {
  const ability = M.useState(s => s.getAbility(abilityId));
  const [abilityText, setAbilityText] = React.useState(JSON.stringify(ability, null, 2));

  React.useEffect(() => {
    setAbilityText(JSON.stringify(ability, null, 2));
  }, [abilityId]);

  return (
    <div style={{ display: "flex", flexDirection: "column" }}>
      <textarea
        style={{ height: "200px" }}
        onChange={e => setAbilityText(e.target.value)}
        value={abilityText}
      />
      <button onClick={saveAbility}>Save</button>
    </div>
  );

  function saveAbility() {
    A.sendGMCommand({ t: "EditAbility", ability: T.decodeAbility.parse(JSON.parse(abilityText)) });
  }
}

export function ExportGame(props: { onDone?: () => void }) {
  const { onDone } = props;
  return (
    <div>
      <button onClick={getIt}>Download</button>
    </div>
  );

  async function getIt() {
    const name = M.getState().gameName || "Untitled";
    const filename = `${name}.arpeggiogame`;
    const response = await sendWSRequest({ t: "GMGetGame" }, Z.object({ game: Z.any() }));
    console.log("response!", response);
    if (!response.game) {
      console.error("couldn't find a game in response", response);
    }
    const file = new File([JSON.stringify(response.game, null, 2)], filename);
    downloadFile(file);
    onDone && onDone();
  }

  function downloadFile(file: File) {
    // Create a link and set the URL using `createObjectURL`
    const link = document.createElement("a");
    link.style.display = "none";
    link.href = URL.createObjectURL(file);
    link.download = file.name;

    // It needs to be added to the DOM so it can be clicked
    document.body.appendChild(link);
    link.click();

    // To make this work on Firefox we need to wait
    // a little while before removing it.
    setTimeout(() => {
      URL.revokeObjectURL(link.href);
      link.parentNode!.removeChild(link);
    }, 0);
  }
}

export function ImportModule(props: {
  path: T.FolderPath;
  onDone: () => void;
}) {
  const { path, onDone } = props;
  return (
    <div>
      <input type="file" id="input" accept=".arpeggiogame" onChange={upload} />
    </div>
  );

  function upload(event: React.ChangeEvent<HTMLInputElement>) {
    const file = event.currentTarget?.files?.[0];
    if (!file) {
      console.error("No file?");
      return;
    }
    console.log("UPLOAD", file.name);
    var reader = new FileReader();
    reader.readAsText(file, "UTF-8");
    reader.onload = (event: ProgressEvent<FileReader>) => {
      if (!event.target || !event.target.result) {
        console.error("Couldn't read file contents?");
        return;
      }
      // readAsText guarantees that event.target.result will be string.
      let content = event.target.result as string;
      let json = JSON.parse(content);
      console.log("uploading:", path.concat(file.name), json);
      A.sendGMCommand(
        {
          t: "LoadModule",
          name: file.name,
          source: "Module",
          path: path.concat(file.name),
          game: json,
        },
      );
      onDone();
    };
  }
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
    A.sendGMCommand({ t: "EditSceneSceneHotspots", scene_id: scene.id, scene_hotspots });
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
    const vis: T.Visibility = allPlayers ? "AllPlayers" : "GMOnly";
    const annotations = scene.annotations.set(pt, [text, vis]);
    A.sendGMCommand({ t: "EditSceneAnnotations", scene_id: scene.id, annotations });
    onClose();
  };
  return (
    <>
      <Checkbox
        label="Visible to all players?"
        checked={allPlayers}
        onChange={(_, d) => setAllPlayers(d.checked === true)}
      />
      <CV.SingleInputForm buttonText="Annotate" onSubmit={annotate} />
    </>
  );
}
