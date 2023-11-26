import { Map, Set } from "immutable";
import capitalize from "lodash/capitalize";
import sortBy from "lodash/sortBy";
import * as React from "react";
import * as Panels from "react-resizable-panels";

import {
  Accordion,
  Button,
  Dimmer,
  Dropdown,
  Form,
  Header,
  Icon,
  Input,
  Label,
  List,
  Menu,
  Message,
  Modal,
  Segment,
} from "semantic-ui-react";

import * as A from "./Actions";
import { useWindowSize } from "./lib/hooks";
import * as M from "./Model";
import * as T from "./PTTypes";
import * as TextInput from "./TextInput";

/**
 * The threshold at which we switch from narrow to wide view.
 * I chose 500 because it's between portait and landscape mode on pretty much all phones, so
 * any phone user that switches to landscape mode should switch to wide view.
 */
const NARROW_THRESHOLD = 500;

export function CreatureCard(props: {
  creature: T.Creature;
  children?: JSX.Element | Array<JSX.Element>;
  menu?: JSX.Element;
}) {
  const creature = props.creature;
  return (
    <Segment style={{ width: "100%" }} raised={true}>
      <div style={{ display: "flex", justifyContent: "space-between" }}>
        <div style={{ display: "flex" }}>
          <CreatureIcon creature={creature} size={80} />
          <div>
            <div style={{ display: "flex" }}>
              <Header>{creature.name}</Header>
              <ClassIcon class_id={creature.class} />
            </div>
            <div>
              {dynamicConditions(creature)
                .valueSeq()
                .map((ac) => conditionIcon(ac.condition))}
            </div>
            {props.children}
          </div>
        </div>
        {props.menu}
      </div>
    </Segment>
  );
}

function dynamicConditions(
  creature: T.Creature,
): Map<T.ConditionID, T.AppliedCondition> {
  return creature.own_conditions.merge(creature.volume_conditions);
}

interface ClassIconProps {
  class_id: T.ClassID;
}
export function ClassIcon(props: ClassIconProps) {
  // XXX TODO: this should not hard-code class names! Instead the game data should have a
  // "game_emoji" property on class objects.
  const class_ = M.useState((s) => s.getClass(props.class_id));
  if (class_ === undefined) {
    return null;
  }
  // We *should* be able to use <>💉</> et al, but there's a bug requiring us to use <span>
  // https://github.com/Microsoft/TypeScript/issues/22012
  switch (class_.name) {
    case "Healer":
      return <span>💉</span>;
    case "Rogue":
      return <span>🗡️</span>;
    case "Archer":
      return <span>🏹</span>;
    case "Mage":
      return <span>🔮</span>;
    case "Knight":
      return <span>🛡️</span>;
    case "Creature":
      return <span>🙂</span>;
    case "Baddie":
      return <span>👹</span>;
    default:
      return null;
  }
}

export function square_style(size: number, color?: string) {
  return {
    width: `${size}px`,
    height: `${size}px`,
    borderRadius: "10px",
    border: "solid 1px black",
    backgroundColor: color,
  };
}

export function CreatureIcon({
  size = 50,
  creature,
}: {
  size?: number;
  creature: T.Creature;
}) {
  const classColor = M.useState((s) => s.getClass(creature.class)?.color);
  if (creature.icon_url !== "") {
    return <SquareImageIcon size={size} url={creature.icon_url} />;
  } else {
    const color = classColor ?? "red";
    return <div style={{ ...square_style(size, color) }}>{creature.name}</div>;
  }
}

export function SquareImageIcon({
  url,
  size = 50,
}: {
  url: string;
  size?: number;
}): JSX.Element {
  return <img src={url} style={square_style(size)} />;
}

export function CollapsibleInventory({
  creature,
}: {
  creature: T.Creature;
}): JSX.Element {
  return (
    <Accordion
      panels={[
        {
          key: "Inventory",
          title: "Inventory",
          content: <CreatureInventory creature={creature} />,
        },
      ]}
    />
  );
}

interface CreatureInventoryProps {
  creature: T.Creature;
}
export function CreatureInventory({ creature }: CreatureInventoryProps) {
  const inv = creature.inventory;
  const items = M.useState((s) => s.getItems(inv.keySeq().toArray()));

  return (
    <List relaxed={true}>
      {items.map((item) => {
        const count = inv.get(item.id);
        if (!count) {
          return;
        }
        return (
          <List.Item
            key={item.id}
            style={{ display: "flex", justifyContent: "space-between" }}
          >
            <div style={{ flex: "1" }}>{item.name}</div>
            <Label circular={true}>
              <Dropdown
                text={count.toString()}
                icon="caret down"
                className="right"
                pointing={true}
                floating={true}
              >
                <Dropdown.Menu>
                  <Dropdown.Header content={item.name} />
                  <ModalMaker
                    button={(open) => <Dropdown.Item onClick={open} content="Give" />}
                    header={<span>Give {item.name}</span>}
                    content={(close) => <GiveItem giver={creature} item={item} onClose={close} />}
                  />
                  <ModalMaker
                    button={(open) => <Dropdown.Item onClick={open} content="Remove" />}
                    header={<span>Remove {item.name}</span>}
                    content={(close) => (
                      <RemoveItem
                        creature={creature}
                        item={item}
                        onClose={close}
                      />
                    )}
                  />
                </Dropdown.Menu>
              </Dropdown>
            </Label>
          </List.Item>
        );
      })}
    </List>
  );
}

interface RemoveItemProps {
  creature: T.Creature;
  item: T.Item;
  onClose: () => void;
}
export class RemoveItem extends React.Component<
  RemoveItemProps,
  { count: number | undefined }
> {
  constructor(props: RemoveItemProps) {
    super(props);
    this.state = { count: 1 };
  }
  render(): JSX.Element {
    const { creature, item, onClose } = this.props;
    const max_count = creature.inventory.get(item.id);
    if (!max_count) {
      return <div>No more!</div>;
    }
    return (
      <Form>
        <Message>
          You have {creature.inventory.get(item.id)}{" "}
          of this item. How many would you like to remove?
        </Message>
        <PositiveIntegerInput
          label="count"
          max={max_count}
          value={this.state.count}
          onChange={(num) => this.setState({ count: num })}
        />
        <Form.Group>
          <Form.Button
            disabled={!this.state.count}
            onClick={() => this.remove()}
          >
            Remove
          </Form.Button>
          <Form.Button onClick={() => onClose()}>Cancel</Form.Button>
        </Form.Group>
      </Form>
    );
  }

  remove() {
    const { creature, item, onClose } = this.props;
    if (!this.state.count) {
      return;
    }
    A.sendGMCommand({
      t: "RemoveItem",
      owner: { Creature: creature.id },
      item_id: item.id,
      // RADIX FIXME this count should be bigint from the beginning
      count: BigInt(this.state.count),
    });
    onClose();
  }
}

interface TransferItemsToRecipientFormProps {
  available_count: number;
  available_recipients: Array<T.Creature>;
  onGive: (recipient: T.Creature, count: number) => void;
  onClose: () => void;
}
export class TransferItemsToRecipientForm extends React.Component<
  TransferItemsToRecipientFormProps,
  { receiver: T.CreatureID | undefined; count: number | undefined }
> {
  constructor(props: TransferItemsToRecipientFormProps) {
    super(props);
    this.state = { receiver: undefined, count: 1 };
  }
  render(): JSX.Element {
    const { available_count, available_recipients } = this.props;
    if (!available_recipients) {
      return <div>There is nobody to give items to.</div>;
    }
    const creature_options = available_recipients.map((creature) => ({
      key: creature.id,
      text: creature.name,
      value: creature.id,
    }));
    return (
      <Form>
        <Message>
          There {available_count === 1 ? "is" : "are"} {available_count}{" "}
          of this item. How many would you like to give?
        </Message>
        <Form.Group>
          <PositiveIntegerInput
            max={available_count}
            label="Count"
            value={this.state.count}
            onChange={(num) => this.setState({ count: num })}
          />
          <Form.Select
            label="Recipient"
            options={creature_options}
            placeholder="Select a Recipient"
            onChange={(_, ev) => this.setState({ receiver: ev.value as T.CreatureID })}
          />
        </Form.Group>
        <Form.Group>
          <Form.Button
            disabled={!(this.state.receiver && this.state.count)}
            onClick={() => this.give()}
          >
            Give
          </Form.Button>
          <Form.Button onClick={() => this.props.onClose()}>Cancel</Form.Button>
        </Form.Group>
      </Form>
    );
  }

  give() {
    const count = this.state.count as number; // Protected by button `disabled`
    const receiver_id = this.state.receiver as T.CreatureID; // Protected by button `disabled`
    const receiver = M.getState().getCreature(receiver_id);
    if (!receiver) {
      console.log(
        "[TransferItemsToRecipientForm.give] Receiver has disappeared",
        receiver_id,
      );
      this.props.onClose();
      return;
    }
    this.props.onGive(receiver, count);
  }
}

interface GiveItemProps {
  item: T.Item;
  giver: T.Creature;
  onClose: () => void;
}
export function GiveItem(props: GiveItemProps) {
  // WARNING: I am playing with fire by putting hook usage after conditional
  // returns. I *think* it is fine because the conditionals hopefully don't
  // change for any given mounting of this component.
  const { item, giver, onClose } = props;
  const scene = M.useState((s) => s.getFocusedScene());
  if (!scene) {
    return <div>You can only transfer items in a scene.</div>;
  }
  const other_cids_in_scene = Set(scene.creatures.keySeq().toArray())
    .delete(giver.id)
    .toArray();
  const other_creatures = M.useState((s) => s.getCreatures(other_cids_in_scene));
  if (!other_creatures) {
    return <div>There is nobody in this scene to give items to.</div>;
  }
  const giver_count = giver.inventory.get(item.id);
  if (!giver_count) {
    return (
      <div>
        {giver.name} does not have any {item.name} to give.
      </div>
    );
  }

  // If this is the Player UI, we don't want to show invisible creatures:
  const available_recipients = M.useState((s) => s.playerId)
    ? other_creatures.filter((c) => {
      const entry = scene.creatures.get(c.id);
      return entry && entry[1] === "AllPlayers";
    })
    : other_creatures;

  return (
    <TransferItemsToRecipientForm
      available_count={giver_count}
      available_recipients={available_recipients}
      onGive={(recip, count) => give(recip, count)}
      onClose={onClose}
    />
  );

  function give(recip: T.Creature, count: number) {
    A.sendGMCommand({
      t: "TransferItem",
      from: { Creature: giver.id },
      to: { Creature: recip.id },
      item_id: item.id,
      count: BigInt(count),
    });
    onClose();
  }
}

interface PositiveIntegerInputProps {
  max?: number;
  value: number | undefined;
  label?: string;
  onChange: (num: number | undefined) => void;
}
export class PositiveIntegerInput extends React.Component<PositiveIntegerInputProps> {
  render(): JSX.Element {
    return (
      <Form.Input
        {...this.props}
        label={this.props.label}
        value={this.props.value === undefined ? "" : this.props.value}
        onChange={(event) => {
          let num = Number(event.currentTarget.value);
          if (event.currentTarget.value === "") {
            this.props.onChange(undefined);
          } else if (num) {
            if (this.props.max !== undefined && num > this.props.max) {
              num = this.props.max;
            }
            this.props.onChange(num);
          }
        }}
      />
    );
  }
}

export function conditionIcon(cond: T.Condition): string {
  if (cond === "Dead") return "💀";
  if (cond === "Incapacitated") return "😞";
  if (cond === "DoubleMaxMovement") return "🏃";
  if ("RecurringEffect" in cond) return "🔁";
  if ("AddDamageBuff" in cond) return "😈";
  if ("ActivateAbility" in cond) {
    return "Ability Activated: " + cond.ActivateAbility;
  }
  M.assertNever(cond);
}

type MenuSize = React.ComponentProps<typeof Menu>["size"];

interface TabbedViewProps {
  // selected_tab allows forcing a specific tab to be focused.
  selected_tab?: string;
  menu_size: MenuSize;
  tabs: React.ReactElement<TabProps>[];
}
export class TabbedView extends React.Component<
  TabbedViewProps,
  { selected: number }
> {
  constructor(props: TabbedViewProps) {
    super(props);
    this.state = { selected: 0 };
  }

  render() {
    let selected = this.state.selected;
    const { tabs } = this.props;
    tabs.forEach((item, index) => {
      if (item.props.name === this.props.selected_tab) {
        selected = index;
      }
    });
    if (!tabs[selected]) {
      return <div>woops</div>;
    }
    return (
      <div
        style={{
          height: "100%",
          width: "100%",
          display: "flex",
          flexDirection: "column",
        }}
      >
        <Menu
          pointing={true}
          compact={true}
          size={this.props.menu_size}
          secondary={true}
        >
          {tabs.map((child, index) => (
            <a
              className={`item ${selected === index ? "active" : ""}`}
              key={child.props.name}
              onClick={() => this.setState({ selected: index })}
            >
              {child.props.name}
            </a>
          ))}
        </Menu>
        <div style={{ position: "relative", height: "100%" }}>
          {tabs.map((child, index) => {
            if (index === selected || child.props.always_render) {
              let style = {};
              if (index !== selected && child.props.always_render) {
                style = { zIndex: -100, visibility: "hidden" };
              }
              return (
                <div
                  key={child.props.name}
                  style={{
                    position: "absolute",
                    height: "100%",
                    width: "100%",
                    ...style,
                  }}
                >
                  {child}
                </div>
              );
            }
          })}
        </div>
      </div>
    );
  }
}

interface TabProps extends React.PropsWithChildren {
  name: string;
  // always_render indicates that the tab contents must be rendered to the DOM *in a realized box*
  // at all times. Meaning that even when the tab is not focused, the contents will be rendered with
  // `visibility: hidden` (*not* `display: none`).
  //  This basically needs to be here for svgPanZoom, which freaks out if the SVG
  // that it's controlling has no physical size in the DOM.
  always_render?: boolean;
}
export class Tab extends React.Component<TabProps> {
  render() {
    return React.Children.only(this.props.children);
  }
}

interface CombatProps {
  card?: React.ComponentType<{ creature: T.Creature }>; // TODO: This should just take a CreatureID
  initiative?: (creatureId: T.CreatureID, init: number) => JSX.Element;
}
export function Combat({ card, initiative }: CombatProps): JSX.Element {
  const creaturesWithInit = M.useState(s => {
    const combat = s.getCombat();
    if (!combat) return;
    return M.filterMap(combat.creatures.data, ([cid, init]) => {
      const creature = s.getCreature(cid);
      if (creature) {
        return [creature, init];
      }
    }) as Array<[T.Creature, number]>;
  });
  const currentPosition = M.useState(s => s.getCombat()?.creatures.cursor);
  if (!creaturesWithInit) return <div>No combat</div>;

  const Card = card ? card : CreatureCard;
  return (
    <Segment.Group>
      {creaturesWithInit.map(([creature, init], index) => {
        const show_init = initiative ? initiative(creature.id, init) : null;
        return (
          <Segment.Group key={creature.id} horizontal={true}>
            <Segment
              compact={true}
              style={{
                width: "25px",
                paddingLeft: 0,
                paddingRight: 0,
                display: "flex",
                flexDirection: "column",
                alignItems: "center",
              }}
            >
              <div style={{ height: "25px" }}>
                {index === currentPosition ? "▶️" : ""}
              </div>
              <div>{show_init}</div>
            </Segment>
            <Card creature={creature} />
          </Segment.Group>
        );
      })}
    </Segment.Group>
  );
}

export function ActionBar(props: {
  creatureId: T.CreatureID;
  combat?: T.Combat;
}) {
  const creature = M.useState((s) => s.getCreature(props.creatureId));
  if (!creature) return <div>Can't find creature {props.creatureId}.</div>;
  let abilities = M.useState((s) =>
    sortBy(
      M.filterMap(Object.values(creature.abilities), (abstatus) => {
        const ability = s.getAbility(abstatus.ability_id);
        if (ability) {
          return { ability_id: abstatus.ability_id, ability };
        }
      }),
      (abo) => abo.ability.name,
    )
  );

  let abilityButtons;
  if (props.combat) {
    const combat = props.combat;
    abilityButtons = abilities.map((abinfo) => (
      <AbilityButton
        key={abinfo.ability_id}
        creature={creature}
        abinfo={abinfo}
        scene_id={combat.scene}
      />
    ));
  } else {
    abilityButtons = undefined;
  }
  return (
    <div style={{ display: "flex" }}>
      <CreatureIcon creature={creature} />
      {props.combat ? <DoneButton /> : <noscript />}
      <MoveButton creature={creature} combat={props.combat} />
      {abilityButtons}
    </div>
  );
}

function DoneButton(): JSX.Element {
  return (
    <Button
      style={{ height: "50px", flex: "1" }}
      onClick={() => A.sendGMCommand({ t: "EndTurn" })}
    >
      Done
    </Button>
  );
}

interface AbilityButtonProps {
  creature: T.Creature;
  abinfo: { ability_id: T.AbilityID; ability: T.Ability };
  scene_id: T.SceneID;
}

function AbilityButton(props: AbilityButtonProps): JSX.Element {
  const { abinfo, creature, scene_id } = props;
  const onClick = () =>
    A.requestCombatAbility(
      creature.id,
      abinfo.ability_id,
      abinfo.ability,
      scene_id,
    );
  const disabled = creature.cur_energy < abinfo.ability.cost;
  return (
    <Button
      style={{ height: "50px", flex: "1" }}
      disabled={disabled}
      onClick={onClick}
    >
      {props.abinfo.ability.name} ({props.abinfo.ability.cost})
    </Button>
  );
}

function MoveButton(props: { creature: T.Creature; combat?: T.Combat }) {
  const movement_left = props.combat
    ? props.creature.speed - props.combat.movement_used
    : 0;
  const suffix = props.combat
    ? " (" + Number(movement_left / 100).toFixed(0) + ")"
    : "";
  return (
    <Button
      style={{ height: "50px", flex: "1" }}
      onClick={() => A.requestCombatMovement()}
    >
      Move {suffix}
    </Button>
  );
}

/**
 * A component which renders a very light grey translucent block over the entire screen,
 * and then renders child elements inside of it.
 *
 * Caveat: child elements should be position: fixed.
 */
export function ClickAway({
  onClick,
  children,
}: {
  onClick: () => void;
  children: React.ReactNode;
}): JSX.Element {
  return (
    <div>
      <Dimmer
        page={true}
        inverted={true}
        active={true}
        onClick={() => onClick()}
      />
      {/* Dimmer uses a z-index of 1000, so we use a 1001 for the content.*/}
      <div style={{ position: "fixed", zIndex: 1001 }}>{children}</div>
    </div>
  );
}

export function ErrorModal() {
  const error = M.useState((s) => s.error);
  const clearError = () => M.getState().clearError();
  if (error) {
    return (
      <Modal dimmer="inverted" open={true} onClose={clearError}>
        <Modal.Header>Error</Modal.Header>
        <Modal.Content>
          <div>{error}</div>
          <Button onClick={clearError}>Ok</Button>
        </Modal.Content>
      </Modal>
    );
  } else {
    return null;
  }
}

interface TheLayoutProps {
  tabs: Array<JSX.Element>;
  bottom_left?: JSX.Element;
  top_left?: JSX.Element;
  bottom_right?: JSX.Element;
  bar_width: number;
  menu_size: MenuSize;
  bottom_bar?: JSX.Element;
  map: React.ReactElement;
}
export function TheLayout(props: TheLayoutProps) {
  const {
    tabs,
    bottom_left,
    top_left,
    bottom_right,
    bar_width,
    menu_size,
    bottom_bar,
    map,
  } = props;
  const window_size = useWindowSize();

  // if we're doing certain grid-oriented things like moving or using abilities, we want to disable
  // all other UI interactions until they're done because otherwise we get into weird inconsistent
  // states. For example: user clicks to move, never chooses destination, and then ends their turn.
  // The movement options would remain on screen and clicking them might do something wrong
  // or just return an error. So we work around this by just disabling all sidebar actions.
  // There are still some other places this needs to be worked around, i.e. when generating actions
  // for the grid creature popup menu.
  const movementOptions = M.useState((s) => s.grid.movement_options);
  const targetOptions = M.useState((s) => s.grid.target_options);
  const disable_bars = !!(movementOptions || targetOptions);

  const disable_div = disable_bars ? <Dimmer active={true} inverted={true} /> : null;

  const middle = (
    // all this relative/absolute crap is because chrome has a stupid flexbox model
    <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <div style={{ flex: 1, position: "relative" }}>
        <div style={{ position: "absolute", width: "100%", height: "100%" }}>
          {map}
        </div>
      </div>
      <div
        style={{
          position: "relative",
          height: "50px",
          width: "100%",
          overflowX: "auto",
        }}
      >
        <div style={{ position: "absolute", width: "100%", height: "50px" }}>
          {bottom_bar}
          {disable_div}
        </div>
      </div>
    </div>
  );

  return (
    <div style={{ height: "100%", width: "100%" }}>
      {window_size.width >= NARROW_THRESHOLD
        ? wideView()
        : narrowView(window_size.width)}
      <ErrorModal />
    </div>
  );

  function right_bar(
    tabs: Array<JSX.Element>,
    extra?: JSX.Element,
    force_map: boolean = false,
  ) {
    const selected_tab = force_map ? "Map" : undefined;
    const tabbed_view = (
      <div style={{ overflowY: "scroll" }}>
        <TabbedView
          menu_size={menu_size}
          selected_tab={selected_tab}
          tabs={tabs}
        />
      </div>
    );
    return extra !== undefined
      ? (
        <Panels.PanelGroup direction="vertical">
          <Panels.Panel style={{ overflowY: "auto" }}>{tabbed_view}</Panels.Panel>
          <Panels.PanelResizeHandle>
            <hr />
          </Panels.PanelResizeHandle>
          <Panels.Panel style={{ overflowY: "auto" }}>{extra}</Panels.Panel>
        </Panels.PanelGroup>
      )
      : tabbed_view;
  }

  function left_bar() {
    return (
      <div
        style={{
          position: "relative",
          height: "100%",
          width: "20%",
          minWidth: "20em",
        }}
      >
        <Panels.PanelGroup direction="vertical">
          <Panels.Panel style={{ overflowY: "auto" }}>{top_left}</Panels.Panel>
          <Panels.PanelResizeHandle>
            <hr />
          </Panels.PanelResizeHandle>
          <Panels.Panel style={{ overflowY: "auto" }}>{bottom_left}</Panels.Panel>
        </Panels.PanelGroup>
        {disable_div}
      </div>
    );
  }

  function wideView() {
    return (
      <div style={{ width: "100%", height: "100%", display: "flex" }}>
        {bottom_left || top_left ? left_bar() : null}
        <div style={{ flex: "1" }}>{middle}</div>
        <div style={{ position: "relative", width: bar_width, height: "100%" }}>
          {right_bar(tabs, bottom_right)}
          {disable_div}
        </div>
      </div>
    );
  }

  function narrowView(width: number) {
    const amended_tabs = tabs.concat(
      <Tab key="Map" name="Map" always_render={true}>
        {middle}
      </Tab>,
    );
    const scale = width / bar_width;
    return (
      <div
        style={{
          height: "100%",
          width: bar_width,
          zoom: `${scale * 100}%`,
        }}
      >
        <div style={{ width: bar_width }}>
          {right_bar(amended_tabs, bottom_right, disable_bars)}
        </div>
      </div>
    );
  }
}

export function MaterialIcon(props: React.PropsWithChildren) {
  return (
    <i
      className="material-icons"
      style={{
        MozUserSelect: "none",
        WebkitUserSelect: "none",
        msUserSelect: "none",
      }}
    >
      {props.children}
    </i>
  );
}

/**
 * The Note Editor
 * Complexities:
 * - The `name` prop may be undefined if we're creating a new note.
 * - Player notes can't be renamed, hence disallow_rename.
 */

export type CreateOrEdit = { t: "CreateNote" } | { t: "EditNote"; original_name: string };
interface NoteEditorProps {
  path: T.FolderPath;
  name: string | undefined;
  disallow_rename?: boolean;
  saveNote: (
    thingy: CreateOrEdit,
    path: T.FolderPath,
    note: T.Note,
  ) => void;
}

export function NoteEditor({
  path,
  disallow_rename,
  ...props
}: NoteEditorProps) {
  const [draftName, setDraftName] = React.useState(props.name);
  const [draftContent, setDraftContent] = React.useState<string | undefined>(
    undefined,
  );

  const originalNote = M.useState((s) => s.getNote(path, props.name));

  React.useEffect(() => {
    setDraftName(props.name);
    setDraftContent(undefined);
  }, [path, props.name]);
  const originalContent = originalNote?.content;
  const renderedContent = draftContent ?? originalContent ?? "";

  return (
    <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <div style={{ display: "flex", justifyContent: "space-between" }}>
        <div>
          <span style={{ fontSize: "xx-small" }}>
            {T.folderPathToString(path)}
          </span>
          <br />
          <Toggler
            a={(edit) => (
              <div>
                <strong>{draftName ?? "Enter a name"}</strong>
                {disallow_rename ? null : (
                  <Icon
                    onClick={edit}
                    name="edit"
                    style={{ cursor: "pointer" }}
                  />
                )}
              </div>
            )}
            b={(view) => (
              <TextInput.TextInput
                defaultValue={draftName || ""}
                onSubmit={(input) => {
                  setDraftName(input);
                  view();
                }}
                onCancel={view}
              />
            )}
          />
        </div>
        <Button
          disabled={draftName === undefined
            || (renderedContent === originalContent && draftName === props.name)}
          onClick={submit}
        >
          Save
        </Button>
      </div>
      <textarea
        style={{ flex: "1", resize: "none", width: "100%", height: "100%" }}
        value={renderedContent}
        onChange={(e) => setDraftContent(e.currentTarget.value)}
      />
    </div>
  );

  function submit() {
    const { saveNote } = props;
    if (!draftName) {
      console.log("[NoteEditorComp.submit] I have no name");
      return;
    }
    const content = draftContent ?? originalNote?.content;
    if (!content) {
      console.log("[NoteEditorComp.submit] No content to save");
      return;
    }
    const newNote: T.Note = { name: draftName, content };
    const thingy: CreateOrEdit = originalNote
      ? { t: "EditNote", original_name: originalNote.name }
      : { t: "CreateNote" };
    saveNote(thingy, path, newNote);
  }
}

export type ToggleFunc = () => void;
interface TogglerProps {
  a: (t: ToggleFunc) => JSX.Element | Array<JSX.Element>;
  b: (t: ToggleFunc) => JSX.Element | Array<JSX.Element>;
}
export class Toggler extends React.Component<
  TogglerProps,
  { toggled: boolean }
> {
  constructor(props: TogglerProps) {
    super(props);
    this.state = { toggled: false };
  }

  render(): JSX.Element | Array<JSX.Element> {
    const toggle = () => this.setState({ toggled: !this.state.toggled });
    if (this.state.toggled) {
      return this.props.b(toggle);
    } else {
      return this.props.a(toggle);
    }
  }
}

interface ModalMakerProps {
  button: (clicker: () => void) => JSX.Element;
  header: JSX.Element;
  content: (closer: () => void) => JSX.Element;
  onClose?: () => void;
}

export function ModalMaker({
  button,
  header,
  content,
  onClose,
}: ModalMakerProps) {
  return (
    <Toggler
      a={button}
      b={(tf) => {
        const close = () => {
          if (onClose) {
            onClose();
          }
          tf();
        };
        return (
          <>
            {button(tf)}
            <Modal
              dimmer="inverted"
              open={true}
              onClose={close}
              style={{ zIndex: 1002 }}
              closeIcon="close"
              closeOnDimmerClick={false}
            >
              <Modal.Header>{header}</Modal.Header>
              <Modal.Content>{content(close)}</Modal.Content>
            </Modal>
          </>
        );
      }}
    />
  );
}

export function describeChallenge(challenge: T.AttributeCheck) {
  return `${challenge.target} ${capitalize(challenge.attr)}
   ${challenge.reliable ? "(reliable)" : ""}`;
}

interface SingleInputFormProps {
  onSubmit: (input: string) => void;
  onCancel?: () => void;
  buttonText: string;
  default?: string;
}
export class SingleInputForm extends React.Component<
  SingleInputFormProps,
  { text: string }
> {
  constructor(props: SingleInputFormProps) {
    super(props);
    this.state = { text: props.default ? props.default : "" };
  }
  render() {
    return (
      <Input
        type="text"
        value={this.state.text}
        onKeyDown={(e: KeyboardEvent) => {
          if (e.key === "Enter") {
            this.props.onSubmit(this.state.text);
            this.setState({ text: "" });
          } else if (e.key === "Escape") {
            if (this.props.onCancel) {
              this.props.onCancel();
            }
          }
        }}
        action={
          <Button
            type="submit"
            onClick={() => this.props.onSubmit(this.state.text)}
          >
            {this.props.buttonText}
          </Button>
        }
        onChange={(e) => this.setState({ text: e.currentTarget.value })}
      />
    );
  }
}

interface GenericChatProps {
  renderLog: (input: T.GameLog) => JSX.Element | undefined;
  sendChat: (input: string) => void;
}
export function GenericChat(props: GenericChatProps): JSX.Element {
  const { renderLog, sendChat } = props;
  const logs = M.useState(s => s.recentLogs);
  return (
    <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <div
        ref={(el) => {
          if (el) {
            el.scrollTop = el.scrollHeight;
          }
        }}
        style={{ flex: "1", overflowY: "auto" }}
      >
        {logs.map(([log_index, log]) => {
          const chat_line = renderLog(log);
          return (
            <div
              style={{
                display: "flex",
                flexDirection: "row",
                justifyContent: "space-between",
              }}
              key={`${log_index.game_idx}-${log_index.log_idx}`}
            >
              {chat_line}
            </div>
          );
        })}
      </div>
      <SingleInputForm onSubmit={sendChat} buttonText="Send" />
    </div>
  );
}

export function ChatLog({ log }: { log: T.GameLog }) {
  if (log.t === "ChatFromPlayer" || log.t === "ChatFromGM") {
    const sender = log.t === "ChatFromPlayer" ? log.player_id : "GM";
    const message = log.message;
    return (
      <span>
        &lt;
        <strong>{sender}</strong>
        &gt; {message}
      </span>
    );
  }
}
