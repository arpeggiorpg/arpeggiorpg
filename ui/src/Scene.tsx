import Input from "@mui/material/Input";
import Slider from "@mui/material/Slider";
import Stack from "@mui/material/Stack";
import Typography from "@mui/material/Typography";
import { Map, Set } from "immutable";
import capitalize from "lodash/capitalize";
import sortBy from "lodash/sortBy";
import * as React from "react";
import { TwitterPicker } from "react-color";
import {
  Button,
  Checkbox,
  Dropdown,
  Form,
  Header,
  Icon,
  Item,
  Label,
  List,
  Loader,
  Menu,
  Popup,
  Segment,
  Tab,
  Table,
} from "semantic-ui-react";
import * as Z from "zod";

import * as A from "./Actions";
import * as Campaign from "./Campaign";
import * as CV from "./CommonView";
import * as M from "./Model";
import * as T from "./PTTypes";
import { EditableNumericLabel, TextInput } from "./TextInput";

export function GMScene({ scene }: { scene: T.Scene }) {
  const scenePlayers = M.useState(s => s.getGame().players.count(p => p.scene === scene.id));
  const totalPlayers = M.useState(s => s.getGame().players.count());
  const player_count = `${scenePlayers}/${totalPlayers}`;
  const linked_scenes_count = scene.related_scenes.count() + scene.scene_hotspots.count();

  const panes = [
    menuItem("Background", () => <EditSceneBackground scene={scene} onDone={() => undefined} />),
    menuItem("Terrain", () => <SceneTerrain scene={scene} />, "Terrain"),
    menuItem("Highlights", () => <SceneHighlights scene={scene} />, "Highlights"),
    menuItem("Volumes", () => <GMSceneVolumes scene={scene} />, "Volumes"),
    menuItem(
      "Creatures",
      () => <GMSceneCreatures scene={scene} />,
      undefined,
      scene.creatures.count().toString(),
    ),
    menuItem("Players", () => <GMScenePlayers scene={scene} />, undefined, player_count),
    menuItem(
      "Items",
      () => <GMSceneInventory scene={scene} />,
      undefined,
      scene.inventory.count().toString(),
    ),
    menuItem(
      "Challenges",
      () => <GMSceneChallenges scene={scene} />,
      undefined,
      scene.attribute_checks.count().toString(),
    ),
    menuItem(
      "Linked Scenes",
      () => <LinkedScenes scene={scene} />,
      "LinkedScenes",
      linked_scenes_count.toString(),
    ),
  ];

  return (
    <Segment>
      <CV.Toggler
        a={open => (
          <Header>
            {scene.name}
            <Icon name="edit" style={{ float: "right", cursor: "pointer" }} onClick={open} />
          </Header>
        )}
        b={close => <EditSceneName scene={scene} onDone={close} />}
      />

      <Tab
        panes={panes}
        defaultActiveIndex={-1}
        onTabChange={(_, data) => {
          const menuItem: { menuItem: string; layer?: M.SceneLayerType } = data
            .panes![data.activeIndex as number] as any;
          // unimplemented!: disable tab-switching when Terrain is unsaved
          M.getState().setGridFocus(scene.id, menuItem.layer);
        }}
        menu={{
          size: "small",
          secondary: true,
          style: { justifyContent: "center", flexWrap: "wrap" },
        }}
      />
    </Segment>
  );

  function menuItem(
    name: string,
    content: () => JSX.Element,
    layer?: M.SceneLayerType,
    detail?: string,
  ) {
    const item = detail
      ? (
        <Menu.Item key={name}>
          {name}
          <Label basic={true} color="grey" circular={true} size="mini">{detail}</Label>
        </Menu.Item>
      )
      : name;
    return { menuItem: item, layer, render: content };
  }
}

export function EditSceneName(props: { scene: T.Scene; onDone: () => void }) {
  return (
    <TextInput
      onSubmit={name => save(name)}
      onCancel={props.onDone}
      defaultValue={props.scene.name}
    />
  );

  function save(name: string) {
    const scene = props.scene;
    A.sendGMCommand({
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

export function EditSceneBackground({ scene, onDone }: { scene: T.Scene; onDone: () => void }) {
  const pendingBackgroundOffset = M.useState(s => s.pendingBackgroundOffset)
    || scene.background_image_offset || [0, 0];
  const pendingBackgroundScale = M.useState(s => s.pendingBackgroundScale?.[0])
    || scene.background_image_scale?.[0] || 0;

  return (
    <div style={{ display: "flex", flexDirection: "column" }}>
      {scene.background_image_url
        ? (
          <>
            <h3>Preview</h3>
            <img
              style={{ width: "90%", marginLeft: "auto", marginRight: "auto" }}
              src={scene.background_image_url}
            />
          </>
        )
        : null}

      <CV.ModalMaker
        button={click => <Button onClick={click}>Upload Image</Button>}
        header={<>Upload Image</>}
        content={closer => <BackgroundImageUpload scene={scene} onClose={closer} />}
      />

      <NumericSlider
        label="X Offset"
        value={pendingBackgroundOffset[0]}
        min={-500}
        max={500}
        step={1}
        onChange={(value) => {
          const currentOffset = M.getState().pendingBackgroundOffset;
          value = value as number; // this is not a ranged slider
          M.getState().setPendingBackgroundOffset([value, currentOffset?.[1] || 0]);
        }}
      />
      <NumericSlider
        label="Y Offset"
        value={pendingBackgroundOffset[1]}
        min={-500}
        max={500}
        step={1}
        onChange={value => {
          const currentOffset = M.getState().pendingBackgroundOffset;
          value = value as number; // this is not a ranged slider
          M.getState().setPendingBackgroundOffset([currentOffset?.[0] || 0, value]);
        }}
      />

      {
        /* Technically, scale separates x/y, but that is... like... really dumb, so I'm not bothering
        * with that. */
      }
      <NumericSlider
        label="Scale"
        min={0}
        max={5}
        step={0.01}
        value={pendingBackgroundScale}
        onChange={(value) => M.getState().setPendingBackgroundScale([value, value])}
      />
      <Button onClick={save}>Save</Button>
    </div>
  );

  function save() {
    const details: T.SceneCreation = {
      ...scene,
      background_image_scale: [pendingBackgroundScale, pendingBackgroundScale],
      background_image_offset: pendingBackgroundOffset,
    };
    A.sendGMCommand({ t: "EditSceneDetails", scene_id: scene.id, details });
    onDone();
  }
}

function NumericSlider(
  { label, value, onChange, min, max, step }: {
    label: string;
    min: number;
    max: number;
    step: number;
    value: number;
    onChange: (value: number) => void;
  },
) {
  const [numberText, setNumberText] = React.useState(value.toString());
  return (
    <>
      <Typography>{label}</Typography>
      <Stack direction="row" spacing={5}>
        <Slider
          value={value}
          min={min}
          max={max}
          step={step}
          valueLabelDisplay="auto"
          onChange={(_event, value) => {
            value = value as number; // this is not a ranged slider.
            onChange(value);
          }}
        />
        <Input
          value={numberText}
          onChange={(event) => {
            setNumberText(event.target.value);
          }}
          onBlur={activate}
          onKeyDown={(event) => {
            console.log("[RADIX] keypress", event.key);
            if (event.key === "Enter") activate();
          }}
        />
      </Stack>
    </>
  );

  function activate() {
    onChange(Number(numberText));
  }
}

function BackgroundImageUpload({ scene, onClose }: { scene: T.Scene; onClose: () => void }) {
  const [type, setType] = React.useState<null | "URL" | "upload">(null);
  const [preparedUpload, setPreparedUpload] = React.useState<
    null | { upload_url: string; final_url: string }
  >(null);

  return (
    <div style={{ display: "flex", flexDirection: "column" }}>
      <div style={{ alignSelf: "center", display: "flex", flexDirection: "row", margin: "5px" }}>
        <Button onClick={() => prepareUpload()}>Upload Image</Button>
        <Button onClick={() => prepareURL()}>Paste URL</Button>
      </div>
      {type === "URL"
        ? <TextInput defaultValue={"Enter URL"} onSubmit={uploadBackgroundFromURL} />
        : type === "upload"
        ? preparedUpload
          ? <input type="file" name="file" accept="image/*" onChange={upload} />
          : <div>Preparing upload...</div>
        : null}
    </div>
  );

  function prepareURL() {
    setType("URL");
    setPreparedUpload(null);
  }

  async function prepareUpload() {
    setType("upload");
    const preparedUpload = await A.sendRequest(
      { t: "RequestUploadImage", purpose: { t: "BackgroundImage" } },
      Z.object({ upload_url: Z.string(), final_url: Z.string() }),
    );
    setPreparedUpload(preparedUpload);
  }

  async function upload(event: React.ChangeEvent<HTMLInputElement>) {
    if (!preparedUpload) {
      console.log("Somehow, preparedUpload is not set");
      return;
    }
    const file = event.currentTarget?.files?.[0];
    if (!file) {
      console.error("No file?");
      return;
    }
    const formData = new FormData();
    formData.append("file", file, file.name);
    const result = await fetch(preparedUpload.upload_url, { method: "POST", body: formData });
    if (!result.ok) {
      onClose();
      M.getState().setError("Error uploading file, sorry!");
    }

    const details: T.SceneCreation = {
      ...scene,
      background_image_scale: [1, 1],
      background_image_url: `${preparedUpload.final_url}/original`,
    };
    A.sendGMCommand({ t: "EditSceneDetails", scene_id: scene.id, details });
    onClose();
  }

  async function uploadBackgroundFromURL(url: string) {
    let result = await A.sendRequest(
      { t: "UploadImageFromURL", url, purpose: { t: "BackgroundImage" } },
      Z.object({ image_url: Z.string() }),
    );
    const details: T.SceneCreation = {
      ...scene,
      background_image_scale: [1, 1],
      background_image_url: `${result.image_url}/original`, // XXX CF Images Dependency
    };

    A.sendGMCommand({ t: "EditSceneDetails", scene_id: scene.id, details });
    onClose();
  }
}

export function SceneTerrain(props: { scene: T.Scene }) {
  const { scene } = props;
  return (
    <div>
      Edit the terrain on the map and then
      <Button onClick={saveTerrain}>Save</Button> or
      <Button onClick={cancelTerrain}>Cancel</Button>
    </div>
  );

  function saveTerrain() {
    const { gridFocus, setGridFocus } = M.getState();
    if (gridFocus?.layer?.t !== "Terrain") return;
    const scene_id = gridFocus.scene_id;
    const terrain = gridFocus.layer.terrain;
    A.sendGMCommand({ t: "EditSceneTerrain", scene_id, terrain });
    // RADIX: why am I setting focus here?
    setGridFocus(scene.id);
  }
  function cancelTerrain() {
    M.getState().setGridFocus(scene.id, "Terrain");
  }
}

export function SceneHighlights(props: { scene: T.Scene }) {
  const { scene } = props;
  const allPlayers = M.useState(s => s.grid.object_visibility === "AllPlayers");
  const highlightColor = M.useState(s => s.grid.highlight_color);

  const vis_checkbox = (
    <Checkbox
      label="Visible to all players?"
      checked={allPlayers}
      onChange={(_, d) => M.getState().setObjectVisibility(d.checked ? "AllPlayers" : "GMOnly")}
    />
  );

  return (
    <div>
      {vis_checkbox}
      <TwitterPicker
        triangle="hide"
        color={highlightColor}
        onChange={color => M.getState().setHighlightColor(color.hex)}
      />
      Edit the highlights on the map and then
      <Button onClick={saveObjects}>Save</Button> or
      <Button onClick={cancelObjects}>Cancel</Button>
    </div>
  );

  function saveObjects() {
    const { gridFocus, setGridFocus } = M.getState();
    if (gridFocus?.layer?.t !== "Highlights") return;
    const scene_id = gridFocus.scene_id;
    const highlights = gridFocus.layer.highlights;
    A.sendGMCommand({ t: "EditSceneHighlights", scene_id, highlights });
    // RADIX: Why am I setting focus here?
    setGridFocus(scene.id);
  }
  function cancelObjects() {
    M.getState().setGridFocus(scene.id, "Highlights");
  }
}

export function GMScenePlayers(props: { scene: T.Scene }) {
  const { scene } = props;
  const players_here = M.useState(
    s => s.getGame().players.valueSeq().toArray().filter(player => player.scene === scene.id),
  );
  return (
    <List relaxed={true}>
      <List.Item>
        <Button onClick={() => moveAll()}>Set as Active Scene and move all players</Button>
      </List.Item>
      {players_here.map(player => <List.Item key={player.player_id}>{player.player_id}</List.Item>)}
    </List>
  );

  function moveAll() {
    const pids = M.getState().game.players.keySeq().toArray();
    const commands: T.GMCommand[] = pids.map(
      player_id => ({ t: "SetPlayerScene", player_id, scene_id: scene.id }),
    );
    commands.push({ t: "SetActiveScene", id: scene.id });
    A.sendGMCommands(commands);
  }
}

export function GMSceneVolumes(_: { scene: T.Scene }) {
  // TODO: add volumes manually
  return <div>Interact with volumes on the grid.</div>;
}

interface LinkedScenesProps {
  scene: T.Scene;
}
export function LinkedScenes(props: LinkedScenesProps) {
  const { scene } = props;
  const relatedScenes = M.useState(s => s.getScenes(scene.related_scenes.toArray()));
  // TODO: this is inefficient without deep-equality on scenes (I don't *think* zustand "shallow" equality will work)
  const scenes = M.useState(s => s.getGame().scenes);
  const hotspotScenes = sortBy(
    M.filterMap(
      scene.scene_hotspots.entrySeq().toArray(),
      ([pos, scene_id]): [T.Scene, T.Point3] | undefined => {
        const hsScene = scenes.get(scene_id);
        if (hsScene) return [hsScene, pos];
      },
    ),
    ([s, _]) => s.name,
  );
  const focusScene = (scene: T.Scene) => () => M.getState().setGridFocus(scene.id);
  return (
    <List>
      <List.Item>
        <List.Header>
          Related Scenes
          <CV.ModalMaker
            button={open => <List.Icon name="edit" style={{ cursor: "pointer" }} onClick={open} />}
            header={<>Add or Remove related scenes</>}
            content={close => (
              <Campaign.MultiSceneSelector
                already_selected={scene.related_scenes}
                on_cancel={close}
                on_selected={related_scenes => {
                  A.sendGMCommand(
                    { t: "EditSceneRelatedScenes", scene_id: scene.id, related_scenes },
                  );
                  close();
                }}
              />
            )}
          />
        </List.Header>
      </List.Item>
      {relatedScenes.map(scene => (
        <List.Item key={`r:${scene.id}`} style={{ cursor: "pointer" }} onClick={focusScene(scene)}>
          {scene.name}
        </List.Item>
      ))}
      <List.Item>
        <List.Header>Hotspot Scenes</List.Header>
      </List.Item>
      {hotspotScenes.map(([scene, point]) => (
        <List.Item key={`h:${scene.id}`} style={{ cursor: "pointer" }} onClick={focusScene(scene)}>
          {scene.name} ({T.encodePoint3(point)})
        </List.Item>
      ))}
    </List>
  );
}

export function GMSceneChallenges({ scene }: { scene: T.Scene }) {
  const challenges = scene.attribute_checks.entrySeq().sortBy(([desc, _]) => desc);
  return (
    <List relaxed={true}>
      <List.Item key="add">
        <CV.ModalMaker
          button={open => <Icon name="add" onClick={open} style={{ cursor: "pointer" }} />}
          header={<span>Add challenge to {scene.name}</span>}
          content={close => <AddChallengeToScene scene={scene} onClose={close} />}
        />
      </List.Item>
      {challenges.map(([description, challenge]) => {
        return (
          <List.Item key={`challenge:${description}`}>
            <CV.ModalMaker
              button={open => (
                <Item.Header style={{ cursor: "pointer" }} onClick={open}>
                  {description}
                  <Dropdown
                    style={{ float: "right" }}
                    icon="caret down"
                    className="right"
                    floating={true}
                    pointing={true}
                  >
                    <Dropdown.Menu>
                      <Dropdown.Header content={description} />
                      <Dropdown.Item
                        content="Delete"
                        onClick={() =>
                          A.sendGMCommand(
                            { t: "RemoveSceneChallenge", scene_id: scene.id, description },
                          )}
                      />
                    </Dropdown.Menu>
                  </Dropdown>
                </Item.Header>
              )}
              header={<span>Challenge</span>}
              content={close => (
                <GMChallenge
                  scene={scene}
                  description={description}
                  challenge={challenge}
                  onClose={close}
                />
              )}
            />
            <Item.Description>{CV.describeChallenge(challenge)}</Item.Description>
          </List.Item>
        );
      })}
    </List>
  );
}

interface GMChallengeProps {
  scene: T.Scene;
  description: string;
  challenge: T.AttributeCheck;
  onClose: () => void;
}
function GMChallenge(props: GMChallengeProps) {
  const [creatureIds, setCreatureIds] = React.useState<Set<T.CreatureID>>(Set());
  const [results, setResults] = React.useState<Map<T.CreatureID, GameChallengeResult> | undefined>(
    undefined,
  );
  const { scene, description, challenge, onClose } = props;
  const creatureResults = M.useState(s =>
    results?.entrySeq().toArray().map(([cid, result]) => ({ creature: s.getCreature(cid), result }))
  );
  return (
    <div>
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
        remove={cid => setCreatureIds(creatureIds.delete(cid))}
      />
      <Button.Group>
        {results === undefined
          ? <Button onClick={() => performChallenge()}>Challenge</Button>
          : null}
        <Button onClick={onClose}>{results === undefined ? "Cancel" : "Close"}</Button>
      </Button.Group>

      {results === undefined
        ? null
        : results.count() !== creatureIds.count()
        ? <Loader />
        : (
          <Table celled={true}>
            <Table.Header>
              <Table.Row>
                <Table.HeaderCell>Creature</Table.HeaderCell>
                <Table.HeaderCell>Creature's Skill</Table.HeaderCell>
                <Table.HeaderCell>Roll</Table.HeaderCell>
                <Table.HeaderCell>Success?</Table.HeaderCell>
              </Table.Row>
            </Table.Header>
            <Table.Body>
              {creatureResults?.map(({ creature, result }) => {
                const creature_name = creature ? creature.name : "Creature disappeared!";
                const creature_skill_level = creature
                  ? creature.attributes.get(challenge.attr)
                  : "Creature does not have attribute";
                return (
                  <Table.Row key={creature?.id}>
                    <Table.Cell>{creature_name}</Table.Cell>
                    <Table.Cell>{creature_skill_level}</Table.Cell>
                    <ChallengeResponse result={result} />
                  </Table.Row>
                );
              })}
            </Table.Body>
          </Table>
        )}
    </div>
  );

  async function performChallenge() {
    const promises = creatureIds.toArray().map(
      async (creatureId): Promise<[string, GameChallengeResult]> => {
        const result = await A.sendGMCommandWithResult({
          t: "AttributeCheck",
          creature_id: creatureId,
          attribute_check: challenge,
        });
        const creatureResponse: GameChallengeResult = (result.t !== "Ok")
          ? { t: "ChallengeResponseError", msg: result.error }
          : M.hasAtLeast(result.result, 1)
          ? { t: "ChallengeResponse", log: result.result[0] }
          : { t: "ChallengeResponseError", msg: `Got unexpected results: ${results}` };
        return [creatureId, creatureResponse];
      },
    );

    setResults(Map());
    const gathered = await Promise.all(promises);
    setResults(Map(gathered));
  }
}

function ChallengeResponse({ result }: { result: GameChallengeResult }) {
  if (result.t === "ChallengeResponseError") {
    return <Table.Cell colSpan={2}>{result.msg}</Table.Cell>;
  }
  const { log } = result;
  if (log.t !== "AttributeCheckResult") {
    return <Table.Cell colSpan={2}>BUG: Got weird GameLog {JSON.stringify(log)}</Table.Cell>;
  }

  return [
    <Table.Cell key="roll">{log.actual}</Table.Cell>,
    <Table.Cell key="success">{log.success ? "😃" : "😡"}</Table.Cell>,
  ];
}

type GameChallengeResult = { t: "ChallengeResponseError"; msg: string } | {
  t: "ChallengeResponse";
  log: T.GameLog;
};

function AddChallengeToScene(props: { scene: T.Scene; onClose: () => void }) {
  const [description, setDescription] = React.useState("");
  const [attr, setAttr] = React.useState<T.AttrID>("strength");
  const [reliable, setReliable] = React.useState(false);
  const [target, setTarget] = React.useState<T.SkillLevel>("Inept");
  const { onClose } = props;
  // TODO: Store attributes on the Game and stop hardcoding them here
  const attr_options = ["strength", "finesse", "magic", "perception"].map(attr => ({
    key: attr,
    text: capitalize(attr),
    value: attr,
  }));
  const skill_level_options = T.SKILL_LEVELS.map(level => ({
    key: level,
    text: level,
    value: level,
  }));
  return (
    <Form>
      <Form.Input label="Description" onChange={(_, d) => setDescription(d.value)} />
      <Form.Group>
        <Form.Select
          label="Attribute"
          options={attr_options}
          onChange={(_, d) => setAttr(d.value as T.AttrID)}
        />
        <Form.Select
          label="Difficulty"
          options={skill_level_options}
          value={target}
          onChange={(_, d) => setTarget(d.value as T.SkillLevel)}
        />
      </Form.Group>
      <Form.Checkbox
        label="Reliable?"
        checked={reliable}
        onChange={(_, d) => setReliable(!!d.checked)}
      />
      <Form.Group>
        <Form.Button onClick={save}>Save</Form.Button>
        <Form.Button onClick={onClose}>Cancel</Form.Button>
      </Form.Group>
    </Form>
  );

  function save() {
    const { scene } = props;
    const challenge = { attr, target, reliable };
    A.sendGMCommand(
      { t: "AddSceneChallenge", scene_id: scene.id, description, challenge },
    );
    onClose();
  }
}

export function GMSceneInventory({ scene }: { scene: T.Scene }) {
  const inventory = M.useState(s => s.getSceneInventory(scene));
  return (
    <div>
      <List relaxed={true}>
        <List.Item key="add">
          <List.Content>
            <CV.ModalMaker
              button={open => <Button onClick={open}>Add</Button>}
              header={<span>Add items to {scene.name}</span>}
              content={close => <AddItemsToScene scene={scene} onClose={close} />}
            />
          </List.Content>
        </List.Item>
        {inventory.map(([item, count]) => (
          <List.Item key={`item:${item.id}`}>
            {item.name}
            <div style={{ float: "right", display: "flex" }}>
              <SceneItemCountEditor scene={scene} item={item} count={count} />
              <Dropdown
                icon="caret down"
                className="right"
                floating={true}
                pointing={true}
              >
                <Dropdown.Menu>
                  <Dropdown.Header content={item.name} />
                  <CV.ModalMaker
                    button={toggler => <Dropdown.Item onClick={toggler} content="Give" />}
                    header={<span>Give {item.name} from {scene.name}</span>}
                    content={close => (
                      <GiveItemFromScene scene={scene} item={item} onClose={close} />
                    )}
                  />
                </Dropdown.Menu>
              </Dropdown>
            </div>
          </List.Item>
        ))}
      </List>
    </div>
  );
}

export function GMSceneCreatures(props: { scene: T.Scene }) {
  const { scene } = props;

  const creatures = M.useState(s =>
    s.getSceneCreatures(scene).map(c => ({ creature: c, inCombat: s.creatureIsInCombat(c.id) }))
  );
  const isCombatScene = M.useState(s => s.getCombat()?.scene === scene.id);

  console.log("[EXPENSIVE:GMSceneCreatures]");
  return (
    <List relaxed={true}>
      <List.Item key="add">
        <List.Content>
          <CV.ModalMaker
            button={toggler => <Button onClick={toggler}>Add or Remove</Button>}
            header={<span>Change creatures in {scene.name}</span>}
            content={toggler => (
              <Campaign.MultiCreatureSelector
                already_selected={scene.creatures.keySeq().toSet()}
                on_cancel={toggler}
                on_selected={cids => {
                  const existing_cids = Set(scene.creatures.keySeq());
                  const new_cids = cids.subtract(existing_cids).toArray();
                  const removed_cids = existing_cids.subtract(cids).toArray();
                  const add_commands = new_cids.map(
                    (creature_id): T.GMCommand => ({
                      t: "AddCreatureToScene",
                      scene_id: scene.id,
                      creature_id,
                      visibility: "AllPlayers",
                    }),
                  );
                  const rem_commands = removed_cids.map(
                    (creature_id): T.GMCommand => ({
                      t: "RemoveCreatureFromScene",
                      scene_id: scene.id,
                      creature_id,
                    }),
                  );
                  A.sendGMCommands(add_commands.concat(rem_commands));
                  toggler();
                }}
              />
            )}
          />
        </List.Content>
      </List.Item>
      {creatures.map(({ creature, inCombat }) => {
        const vis = scene.creatures.get(creature.id)![1]; // !: must exist in map()
        const vis_desc = vis === "GMOnly"
          ? "Only visible to the GM"
          : "Visible to all players";
        return (
          <List.Item key={`cid:${creature.id}`}>
            <List.Content floated="left">
              <CV.ClassIcon class_id={creature.class} />
            </List.Content>
            {creature.name}
            <List.Content floated="right">
              <Popup
                trigger={
                  <Icon
                    name="eye"
                    style={{ cursor: "pointer", color: vis === "GMOnly" ? "grey" : "black" }}
                    onClick={() => {
                      const new_vis: T.Visibility = vis === "GMOnly" ? "AllPlayers" : "GMOnly";
                      console.log("CLICK", new_vis);
                      A.sendGMCommand({
                        t: "SetSceneCreatureVisibility",
                        scene_id: scene.id,
                        creature_id: creature.id,
                        visibility: new_vis,
                      });
                    }}
                  />
                }
                content={vis_desc}
              />
              <Dropdown icon="caret down" className="right" floating={true} pointing={true}>
                <Dropdown.Menu>
                  <Dropdown.Header content={creature.name} />
                  {isCombatScene && inCombat
                    ? (
                      <Dropdown.Item
                        content="Add to Combat"
                        onClick={() => addToCombat(creature)}
                      />
                    )
                    : null}
                </Dropdown.Menu>
              </Dropdown>
            </List.Content>
          </List.Item>
        );
      })}
    </List>
  );

  function addToCombat(creature: T.Creature) {
    A.sendGMCommand({ t: "AddCreatureToCombat", creature_id: creature.id });
  }
}

interface SelectSceneCreaturesProps {
  scene: T.Scene;
  add: (cid: T.CreatureID) => void;
  remove: (cid: T.CreatureID) => void;
  selections: Set<T.CreatureID>;
}
export function SelectSceneCreatures(props: SelectSceneCreaturesProps) {
  const { scene, add, remove, selections } = props;
  const creatures = M.useState(s => s.getSceneCreatures(scene));
  return (
    <List relaxed={true}>
      {creatures.map(creature => (
        <List.Item key={creature.id} style={{ display: "flex", flexDirection: "row" }}>
          <input
            type="checkbox"
            checked={selections.includes(creature.id)}
            onChange={nv =>
              nv.currentTarget.checked ? add(creature.id) : remove(creature.id)}
          />
          <CV.ClassIcon class_id={creature.class} />
          {creature.name}
        </List.Item>
      ))}
    </List>
  );
}

function GiveItemFromScene(props: { scene: T.Scene; item: T.Item; onClose: () => void }) {
  const { scene, item, onClose } = props;
  const available_count = scene.inventory.get(item.id);
  if (!available_count) return <div>Lost item {item.name}!</div>;
  const availableRecipients = M.useState(s => s.getSceneCreatures(scene));
  return (
    <CV.TransferItemsToRecipientForm
      available_count={available_count}
      available_recipients={availableRecipients}
      onGive={give}
      onClose={onClose}
    />
  );
  function give(recip: T.Creature, count: number) {
    A.sendGMCommand({
      t: "TransferItem",
      from: { Scene: scene.id },
      to: { Creature: recip.id },
      item_id: item.id,
      count: BigInt(count),
    });
    onClose();
  }
}

function AddItemsToScene(props: { scene: T.Scene; onClose: () => void }) {
  const { scene, onClose } = props;
  return (
    <Campaign.MultiItemSelector
      require_selected={scene.inventory.keySeq().toSet()}
      on_selected={item_ids => {
        const new_items = item_ids.subtract(scene.inventory.keySeq().toSet());
        for (const item_id of new_items.toArray()) {
          A.sendGMCommand(
            { t: "SetItemCount", owner: { Scene: scene.id }, item_id, count: 1n },
          );
        }
        onClose();
      }}
      on_cancel={onClose}
    />
  );
}

function SceneItemCountEditor(props: { scene: T.Scene; item: T.Item; count: number }) {
  const { scene, item, count } = props;
  return <EditableNumericLabel value={count} save={save} />;

  function save(num: number) {
    A.sendGMCommand(
      { t: "SetItemCount", owner: { Scene: scene.id }, item_id: item.id, count: BigInt(num) },
    );
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
