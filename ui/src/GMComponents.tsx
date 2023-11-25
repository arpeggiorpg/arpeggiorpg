/// GM-only components
import { Set } from "immutable";
import isEqual from "lodash/isEqual";
import * as React from "react";
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
  const inCombat = M.useState(s => !!s.game.current_combat);
  if (!inCombat) {
    return <StartCombat />;
  }

  return (
    <div>
      <GMCombatHeader />
      <CV.Combat card={GMCombatCreatureCard} initiative={initiative} />
    </div>
  );

  function initiative(creatureId: T.CreatureID, init: number) {
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
            changeInit(creatureId, input);
          }}
        />
      );
    }
  }

  function changeInit(creatureId: T.CreatureID, new_init: string) {
    const initiative = Number(new_init);
    A.sendGMCommand(
      { t: "ChangeCreatureInitiative", creature_id: creatureId, initiative },
    );
  }
}

function StartCombat() {
  const sceneCreatureIDs = M.useState(s => {
    const scene = s.getFocusedScene();
    if (!scene) return;
    return s.getSceneCreatures(scene).map(c => c.id);
  });
  // TODO: clean up this mess. Should this be a useEffect? I hate useEffect!
  const [selected, setSelected] = React.useState<Set<T.CreatureID>>(Set(sceneCreatureIDs));

  if (!sceneCreatureIDs) return <div>No scene</div>;

  // componentWillReceiveProps(nextProps: { scene: T.Scene } & M.ReduxProps) {
  //   const scene_creatures = nextProps.scene.creatures.keySeq().toSet();
  //   // Clear out old creatures that aren't in this scene
  //   this.setState({ selected: this.state.selected.intersect(scene_creatures) });
  // }

  return (
    <div>
      <Button
        onClick={() => {
          const scene = M.getState().getFocusedScene();
          if (!scene) throw new Error("no scene");
          A.sendGMCommand(
            { t: "StartCombat", scene_id: scene.id, combatants: selected.toArray() },
          );
        }}
      >
        Start combat
      </Button>
      <SelectSceneCreatures
        selections={selected}
        add={cid => setSelected(selected.add(cid))}
        remove={cid => setSelected(selected.delete(cid))}
      />
    </div>
  );
}

function GMCombatHeader() {
  const sceneName = M.useState(s => {
    const combatSceneId = s.getCombat()?.scene;
    if (!combatSceneId) return;
    const scene = s.getScene(combatSceneId);
    return scene?.name;
  });

  return (
    <Segment>
      <div>
        <span style={{ fontWeight: "bold" }}>Scene:</span>&nbsp;
        <a
          href="#"
          onClick={() => {
            const combatSceneId = M.getState().getCombat()?.scene;
            if (!combatSceneId) throw new Error("no combat?");
            M.getState().setGridFocus(combatSceneId);
          }}
        >
          {sceneName}
        </a>
        <Button onClick={() => A.sendGMCommand({ t: "StopCombat" })}>
          Stop combat
        </Button>
      </div>
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
    A.sendGMCommand({ t: "EditCreatureDetails", creature: details });
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

  const [class_, setClass] = React.useState<string | undefined>(undefined);
  const [name, setName] = React.useState("");

  const classes = M.useState(s =>
    s.game.classes.valueSeq().toArray().map(class_ => ({
      id: class_.id,
      name: class_.name,
    })), isEqual);
  const classOptions = classes.map(class_ => ({
    key: class_.id,
    value: class_.id,
    text: (
      <>
        <CV.ClassIcon class_id={class_.id} />
        {class_.name}
      </>
    ),
  }));

  const hasClass = !!class_;

  return (
    <Form error={hasClass}>
      <Form.Field>
        <Form.Input label="Name" value={name} onChange={(_, data) => setName(data.value)} />
      </Form.Field>
      <Form.Field style={{ flex: 2 }}>
        <Form.Select
          label="Class"
          value={class_}
          options={classOptions}
          placeholder="Class"
          onChange={(_, data) => setClass(data.value as string)}
        />
      </Form.Field>
      <Form.Group>
        <Form.Button disabled={!hasClass} onClick={save}>
          Save
        </Form.Button>
        <Form.Button onClick={props.onClose}>Cancel</Form.Button>
      </Form.Group>
    </Form>
  );

  function save() {
    if (!class_) return;
    const creature: T.CreatureCreation = {
      name,
      class: class_,
      note: "",
      bio: "",
      portrait_url: "",
      initiative: { Expr: { num: 1, size: 20 } },
      size: { x: 1, y: 1, z: 1 },
      icon_url: "",
    };

    A.sendGMCommand({ t: "CreateCreature", path, creature });
    props.onClose();
  }
}

function GMEditCreature(props: { creature: T.Creature; onClose: () => void }) {
  const { creature, onClose } = props;
  return <EditCreatureForm creature={creature} onSave={save} onClose={onClose} />;

  function save(details: T.CreatureData) {
    A.sendGMCommand({ t: "EditCreatureDetails", creature: details });
    onClose();
  }
}

interface EditCreatureProps {
  creature: T.Creature;
  onClose: () => void;
  onSave: (cdata: T.Creature) => void;
}
function EditCreatureForm(props: EditCreatureProps) {
  const [portraitUrl, setPortraitUrl] = React.useState(props.creature.portrait_url);
  const [name, setName] = React.useState(props.creature.name);
  const [note, setNote] = React.useState(props.creature.note);
  const [bio, setBio] = React.useState(props.creature.bio);
  const [initiativeString, setInitiativeString] = React.useState(
    Dice.format(props.creature.initiative),
  );
  const [maxHealth, setMaxHealth] = React.useState(props.creature.max_health.toString());
  const [curHealth, setCurHealth] = React.useState(props.creature.cur_health.toString());
  const [maxEnergy, setMaxEnergy] = React.useState(props.creature.max_energy.toString());
  const [curEnergy, setCurEnergy] = React.useState(props.creature.cur_energy.toString());
  const [class_, setClass] = React.useState(props.creature.class);
  const [size, setSize] = React.useState<number>(props.creature.size.x);
  const [iconUrl, setIconUrl] = React.useState(props.creature.icon_url);
  const classes = M.useState(s =>
    s.game.classes.valueSeq().toArray().map(class_ => ({
      id: class_.id,
      name: class_.name,
    })), isEqual);

  const { onClose } = props;
  const parsedInitiative = Dice.maybeParse(initiativeString);
  const classOptions = classes.map(class_ => ({
    key: class_.id,
    value: class_.id,
    text: (
      <>
        <CV.ClassIcon class_id={class_.id} />
        {class_.name}
      </>
    ),
  }));
  const formOk = parsedInitiative.status && isValidQuantity(maxHealth)
    && isValidQuantity(curHealth) && isValidQuantity(maxEnergy) && isValidQuantity(curEnergy);
  return (
    <Form error={!parsedInitiative.status}>
      <Form.Group>
        <Form.Field style={{ flex: "3" }}>
          <Form.Input label="Name" value={name} onChange={(_, data) => setName(data.value)} />
        </Form.Field>
        <Form.Field style={{ flex: 2 }}>
          <Form.Select
            label="Class"
            value={class_}
            options={classOptions}
            placeholder="Class"
            onChange={(_, data) => setClass(data.value as string)}
          />
        </Form.Field>
      </Form.Group>
      <Form.Group>
        <Form.Field style={{ flex: 3 }}>
          <Form.Input
            label="Initiative"
            error={!parsedInitiative.status}
            value={initiativeString}
            onChange={(_, data) => setInitiativeString(data.value)}
          />
        </Form.Field>
        {parsedInitiative.status
          ? <Message>Parsed: {Dice.format(parsedInitiative.value)}</Message>
          : (
            <Message error={true}>
              <Message.Header>Couldn't parse dice expression</Message.Header>
              <Message.Content>
                Expected {parsedInitiative.expected} at line {parsedInitiative.index.line}, column
                {" "}
                {parsedInitiative.index.column}
              </Message.Content>
            </Message>
          )}
        <Form.Field style={{ flex: 2 }}>
          <Form.Select
            label="Size"
            value={size}
            onChange={(_, data) => setSize(Number(data.value))}
            options={[
              { key: "medium", text: "Medium", value: 1 },
              { key: "large", text: "Large", value: 2 },
              { key: "huge", text: "Huge", value: 3 },
            ]}
          />
        </Form.Field>
      </Form.Group>
      <Form.Group>
        <Form.Field style={{ flex: 1 }}>
          <Form.Input
            label="Max Energy"
            value={maxEnergy}
            onChange={(_, d) => setMaxEnergy(d.value)}
          />
        </Form.Field>
        <Form.Field style={{ flex: 1 }}>
          <Form.Input
            label="Current Energy"
            value={curEnergy}
            onChange={(_, d) => setCurEnergy(d.value)}
          />
        </Form.Field>
      </Form.Group>
      <Form.Group>
        <Form.Field style={{ flex: 1 }}>
          <Form.Input
            label="Max Health"
            value={maxHealth}
            onChange={(_, d) => setMaxHealth(d.value)}
          />
        </Form.Field>
        <Form.Field style={{ flex: 1 }}>
          <Form.Input
            label="Current Health"
            value={curHealth}
            onChange={(_, d) => setCurHealth(d.value)}
          />
        </Form.Field>
      </Form.Group>
      <Form.Group style={{ width: "100%" }}>
        <Form.Field style={{ flex: "1" }}>
          <label>Icon Image URL:</label>
          <Input fluid={true} value={iconUrl} onChange={(_, data) => setIconUrl(data.value)} />
        </Form.Field>
        {iconUrl
          ? <CV.SquareImageIcon url={iconUrl} />
          : "Enter an URL for preview"}
      </Form.Group>
      <Form.Group style={{ width: "100%" }}>
        <Form.Field style={{ flex: "1" }}>
          <label>Portrait Image URL:</label>
          <Input
            fluid={true}
            value={portraitUrl}
            onChange={(_, data) => setPortraitUrl(data.value)}
          />
        </Form.Field>
        {portraitUrl
          ? <img src={portraitUrl} />
          : "Enter an URL for preview"}
      </Form.Group>
      <Form.Input label="Note" value={note} onChange={(_, data) => setNote(data.value)} />
      <Form.TextArea
        label="Bio"
        value={bio}
        onChange={(_, data) => data.value && setBio(data.value as string)}
      />
      <Form.Group>
        <Form.Button disabled={!formOk} onClick={save}>
          Save
        </Form.Button>
        <Form.Button onClick={onClose}>Cancel</Form.Button>
      </Form.Group>
    </Form>
  );

  function isValidQuantity(s: string) {
    const n = Number(s);
    return n && Number.isInteger(n) && n >= 0;
  }

  function save() {
    const creature = {
      ...props.creature,
      name,
      class: class_,
      portrait_url: portraitUrl,
      note,
      bio,
      initiative: Dice.parse(initiativeString),
      size: { x: size, y: size, z: size },
      icon_url: iconUrl,
      max_health: Number(maxHealth),
      cur_health: Number(curHealth),
      max_energy: Number(maxEnergy),
      cur_energy: Number(curEnergy),
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
