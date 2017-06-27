import * as I from 'immutable';
import * as LD from "lodash";
import * as React from "react";
import PanelGroup from 'react-panelgroup';
import { Provider } from 'react-redux';
import * as WindowSizeListener from 'react-window-size-listener';
import * as Redux from 'redux';

// import 'semantic-ui-css/semantic.min.css';
import { Accordion, Button, Form, Input, Menu, Modal, Segment } from 'semantic-ui-react';
import * as SUI from 'semantic-ui-react';


import { PTUI } from './Model';
import * as M from './Model';
import * as T from './PTTypes';

/** The threshold at which we switch from narrow to wide view.
 * I chose 500 because it's between portait and landscape mode on pretty much all phones, so
 * any phone user that switches to landscape mode should switch to wide view.
 */
const NARROW_THRESHOLD = 500;


interface MainProps {
  app?: object;
  rpi_url: string;
}
export class Main extends React.Component<MainProps, { store: Redux.Store<M.PTUI> | undefined; }> {
  app?: object;
  rpi_url: string;

  constructor(props: MainProps) {
    super(props);
    const ptui = props.app
      ? new M.PTUI(props.rpi_url, T.decodeApp.decodeAny(props.app))
      : undefined;
    const store = ptui ? Redux.createStore(M.update, ptui) : undefined;
    this.state = { store };
  }

  componentWillReceiveProps(nextProps: MainProps) {
    if (!M.isEqual(this.props, nextProps)) {
      if (this.state.store) {
        if (nextProps.app) {
          this.state.store.dispatch(
            { type: "RefreshApp", app: T.decodeApp.decodeAny(nextProps.app) });
        }
      } else {
        if (nextProps.app) {
          const ptui = new M.PTUI(
            nextProps.rpi_url, T.decodeApp.decodeAny(nextProps.app));
          const store = Redux.createStore(M.update, ptui);
          this.setState({ store });
        }
      }
    }
  }

  render(): JSX.Element {
    if (!this.state.store) {
      return <div>Waiting for initial data from server.</div>;
    }
    const ptui = this.state.store.getState();
    return <Provider store={this.state.store}>{this.props.children}</Provider>;
  }
}

export const CreatureCard = M.connectRedux(
  function CreatureCard(
    props: { creature: T.Creature; children?: JSX.Element | Array<JSX.Element>; menu?: JSX.Element }
      & M.ReduxProps): JSX.Element {
    const creature = props.creature;
    return <Segment style={{ width: "100%" }} raised={true}>
      <div style={{ display: "flex", justifyContent: "space-between" }}>
        <div>{classIcon(creature)} <strong>{creature.name}</strong>
          {LD.values(creature.conditions).map(ac => conditionIcon(ac.condition))}</div>
        {props.menu}
      </div>
      <div style={{ display: "flex" }}>
        <CreatureIcon app={props.ptui.app} creature={creature} />
        <div style={{ display: "flex", flexDirection: "column" }}>
          {props.children}
        </div>
      </div>
    </Segment>;

  });

export function classIcon(creature: T.Creature): string {
  switch (creature.class_) {
    case "cleric": return "💉";
    case "rogue": return "🗡️";
    case "ranger": return "🏹";
    case "creature": return "🙂";
    case "baddie": return "👹";
    default: return "";
  }
}

const SQUARE_STYLE = {
  width: "50px", height: "50px",
  borderRadius: "10px", border: "solid 1px black",
};

export function CreatureIcon(props: { app: T.App, creature: T.Creature }): JSX.Element | null {
  if (props.creature.portrait_url !== "") {
    return <SquareImageIcon url={props.creature.portrait_url} />;
  } else {
    const class_ = M.get(props.app.current_game.classes, props.creature.class_);
    const color = class_ ? class_.color : "red";
    return <div style={{ backgroundColor: color, ...SQUARE_STYLE }}>{props.creature.name}</div>;
  }
}

export function SquareImageIcon(props: { url: string }): JSX.Element {
  return <img src={props.url} style={SQUARE_STYLE} />;
}

export const CollapsibleInventory = M.connectRedux(
  function CollapsibleInventory({ creature }: { creature: T.Creature }): JSX.Element {
    return <Accordion panels={[{
      title: "Inventory",
      content: <CreatureInventory creature={creature} />,
    }]} />;
  });

interface CreatureInventoryProps {
  creature: T.Creature;
}
class CreatureInventoryComp extends React.Component<CreatureInventoryProps & M.ReduxProps,
  { giving: T.CreatureID | undefined }> {
  constructor(props: CreatureInventoryProps & M.ReduxProps) {
    super(props);
    this.state = { giving: undefined };
  }
  render(): JSX.Element | null {
    const inv = this.props.creature.inventory;
    const items = this.props.ptui.getItems(inv.keySeq().toArray());

    return <div>
      {items.map(item =>
        <div key={item.id} style={{ display: "flex", justifyContent: "space-between" }}>
          {item.name} ({inv.get(item.id)})
          <Button onClick={() => this.setState({ giving: item.id })}>Give</Button>
          <SUI.Modal dimmer="inverted"
            open={this.state.giving === item.id}
            onClose={() => this.setState({ giving: undefined })}>
            <SUI.Modal.Header>Give {item.name}</SUI.Modal.Header>
            <SUI.Modal.Content>
              <GiveItem giver={this.props.creature.id} item_id={item.id}
                onClose={() => this.setState({ giving: undefined })} />
            </SUI.Modal.Content>
          </SUI.Modal>
        </div>
      )}
    </div>;
  }
}

export const CreatureInventory = M.connectRedux(CreatureInventoryComp);

interface GiveItemProps {
  item_id: T.ItemID;
  giver: T.CreatureID;
  onClose: () => void;
}
export class GiveItemComp extends React.Component<
  GiveItemProps & M.ReduxProps,
  { receiver: T.CreatureID | undefined; count: number | undefined }> {
  constructor(props: GiveItemProps & M.ReduxProps) {
    super(props);
    this.state = { receiver: undefined, count: 1 };
  }
  render(): JSX.Element {
    const ptui = this.props.ptui;
    const scene = this.props.ptui.focused_scene();
    if (!scene) { return <div>You can only transfer items in a scene.</div>; }
    const other_cids_in_scene = I.Set(scene.creatures.keySeq().toArray())
      .delete(this.props.giver).toArray();
    const other_creatures = ptui.getCreatures(other_cids_in_scene);
    if (!other_creatures) { return <div>There is nobody in this scene to give items to.</div>; }
    const item = ptui.getItem(this.props.item_id);
    if (!item) { return <div>The Item definition cannot be found.</div>; }
    const giver_ = ptui.getCreature(this.props.giver);
    if (!giver_) { return <div>Giver not found!</div>; }
    const giver = giver_;
    const giver_count = giver.inventory.get(this.props.item_id);
    if (!giver_count) { return <div>{giver.name} does not have any {item.name} to give.</div>; }
    const creature_options = other_creatures.map(
      creature => ({ key: creature.id, text: creature.name, value: creature.id }));
    return <Form>
      <Form.Group>
        <PositiveIntegerInput max={giver_count} label="Count" value={this.state.count}
          onChange={num => this.setState({ count: num })} />
        <Form.Select label="Creature" options={creature_options}
          placeholder="Select a Creature"
          onChange={(_, ev) => this.setState({ receiver: ev.value as T.CreatureID })} />
      </Form.Group>
      <Form.Group>
        <Form.Button
          disabled={!(this.state.receiver && this.state.count)}
          onClick={ev => this.give(giver)}>
          Give
            </Form.Button>
        <Form.Button onClick={ev => this.props.onClose()}>Cancel</Form.Button>
      </Form.Group>
    </Form>;
  }

  give(giver: T.Creature) {
    const count = this.state.count as number; // Protected by button `disabled`
    const receiver_id = this.state.receiver as T.CreatureID; // Protected by button `disabled`
    const receiver = this.props.ptui.app.current_game.creatures.get(receiver_id);
    if (!receiver) {
      console.log("[give] Receiver has disappeared", receiver_id);
      this.props.onClose();
      return;
    }

    const newGiver = LD.assign({}, giver,
      { inventory: M.removeFromInventory(giver.inventory, this.props.item_id, count) });
    const newReceiver = LD.assign({}, receiver,
      { inventory: M.addToInventory(receiver.inventory, this.props.item_id, count) });

    this.props.ptui.sendCommand(this.props.dispatch, { t: "EditCreature", creature: newGiver });
    this.props.ptui.sendCommand(this.props.dispatch, { t: "EditCreature", creature: newReceiver });
    this.props.onClose();
  }
}
export const GiveItem = M.connectRedux(GiveItemComp);


interface PositiveIntegerInputProps {
  max?: number; value: number | undefined;
  onChange: (num: number | undefined) => void;
}
export class PositiveIntegerInput
  extends React.Component<PositiveIntegerInputProps & { [index: string]: any }, undefined> {
  render(): JSX.Element {
    return <Form.Input
      {...this.props}
      value={this.props.value === undefined ? "" : this.props.value}
      onChange={event => {
        let num = Number(event.currentTarget.value);
        if (event.currentTarget.value === "") {
          this.props.onChange(undefined);
        } else if (num) {
          if (this.props.max !== undefined && num > this.props.max) { num = this.props.max; }
          this.props.onChange(num);
        }
      }} />;
  }
}

export function conditionIcon(cond: T.Condition): string {
  switch (cond.t) {
    case "RecurringEffect": return cond.effect.toString();
    case "Dead": return "💀";
    case "Incapacitated": return "😞";
    case "AddDamageBuff": return "😈";
    case "DoubleMaxMovement": return "🏃";
    case "ActivateAbility": return "Ability Activated: " + cond.ability_id;
  }
}

type MenuSize = 'mini' | 'tiny' | 'small' | 'large' | 'huge' | 'massive';

interface TabbedViewProps {
  children: Array<JSX.Element | null>;
  menu_size: MenuSize;
}
export class TabbedView extends React.Component<TabbedViewProps, { selected: number }> {

  constructor(props: TabbedViewProps) {
    super(props);
    this.state = { selected: 0 };
  }

  render(): JSX.Element {
    const children_ = React.Children.map(
      this.props.children,
      c => c);
    const children: Array<Tab> = M.filterMap(
      children_, (c: any) => { if (c && c.type === Tab) { return c; } });
    if (!M.idx<JSX.Element | null>(this.props.children, this.state.selected)) {
      return <div>woops</div>;
    }
    return <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <Menu pointing={true} compact={true} size={this.props.menu_size} secondary={true}>
        {children.map((child, index) =>
          <Menu.Item key={child.props.name} name={child.props.name}
            active={this.state.selected === index}
            onClick={() => this.setState({ selected: index })} />)
        }
      </Menu>
      <div style={{ overflowY: "auto", position: "relative", height: "100%" }}>
        {children.map((child, index) => {
          const style = index === this.state.selected
            ? {}
            : (child.props.always_render ?
              { zIndex: -100, visibility: "hidden" } : { display: "none" });
          return <div key={child.props.name}
            style={{ position: "absolute", height: "100%", width: "100%", ...style }}>
            {child}
          </div>;
        })}
      </div>
    </div>;
  }
}

interface TabProps { name: string; always_render?: boolean; }
export class Tab extends React.Component<TabProps, undefined> {
  render(): JSX.Element {
    return React.Children.only(this.props.children);
  }
}

interface CombatProps {
  combat: T.Combat;
  card?: React.ComponentType<{ creature: T.Creature }>;
  initiative?: (creature: T.Creature, init: number) => JSX.Element;
}
export const Combat = M.connectRedux(
  function Combat({ combat, card, ptui, initiative }: CombatProps & M.ReduxProps): JSX.Element {
    const creatures_with_init = M.filterMap(combat.creatures.data,
      ([cid, init]) => {
        const creature = ptui.getCreature(cid);
        if (creature) { return [creature, init]; }
      }) as Array<[T.Creature, number]>;

    const Card = card ? card : CreatureCard;
    return <Segment.Group>
      {creatures_with_init.map(([creature, init], index) => {
        const show_init = initiative ? initiative(creature, init) : null;
        return <Segment.Group key={creature.id} horizontal={true}>
          <Segment compact={true}
            style={{
              width: "25px", paddingLeft: 0, paddingRight: 0,
              display: "flex", flexDirection: "column", alignItems: "center",
            }}>
            <div style={{ height: "25px" }}>{index === combat.creatures.cursor ? "▶️" : ""}</div>
            <div>{show_init}</div>
          </Segment>
          <Card creature={creature} />
        </Segment.Group>;
      })
      }
    </Segment.Group>;
  });

export const ActionBar = M.connectRedux((
  props: { creature: T.Creature; combat?: T.Combat } & M.ReduxProps): JSX.Element => {
  const abilities = M.filterMap(LD.values(props.creature.abilities),
    abstatus => {
      const ability = M.get(props.ptui.app.current_game.abilities, abstatus.ability_id);
      if (ability) {
        return { ability_id: abstatus.ability_id, ability };
      }
    });

  let abilityButtons;
  if (props.combat) {
    const combat = props.combat;
    abilityButtons = abilities.map(abinfo =>
      <AbilityButton key={abinfo.ability_id}
        creature={props.creature} abinfo={abinfo}
        scene_id={combat.scene} />);
  } else {
    abilityButtons = <noscript />;
  }
  return <div style={{ display: "flex" }}>
    <CreatureIcon app={props.ptui.app} creature={props.creature} />
    {props.combat ? <DoneButton /> : <noscript />}
    <MoveButton creature={props.creature} combat={props.combat} />
    {abilityButtons}
  </div>;
});

export const DoneButton = M.connectRedux(({ ptui, dispatch }: M.ReduxProps): JSX.Element => {
  const command: T.GameCommand = { t: "Done" };
  return <Button
    style={{ height: "50px", flex: "1" }}
    onClick={() => ptui.sendCommand(dispatch, command)}>
    Done
  </Button>;
});

interface AbilityButtonProps {
  creature: T.Creature;
  abinfo: { ability_id: T.AbilityID; ability: T.Ability };
  scene_id: T.SceneID;
}
const AbilityButton = M.connectRedux((props: AbilityButtonProps & M.ReduxProps): JSX.Element => {
  const onClick = () =>
    props.ptui.requestCombatAbility(props.dispatch,
      props.creature.id, props.abinfo.ability_id, props.abinfo.ability, props.scene_id);
  return <Button style={{ height: "50px", flex: "1" }}
    onClick={onClick}>
    {props.abinfo.ability.name}
  </Button>;
});

const MoveButton = M.connectRedux((props: { creature: T.Creature; combat?: T.Combat } & M.ReduxProps)
  : JSX.Element => {
  const movement_left = props.combat ? props.creature.speed - props.combat.movement_used : 0;
  const suffix = props.combat ? " (" + Number(movement_left / 100).toFixed(0) + ")" : "";
  return <Button style={{ height: "50px", flex: "1" }}
    onClick={() => props.ptui.requestCombatMovement(props.dispatch)}>
    Move {suffix}
  </Button>;
});


/** A component which renders a very light grey translucent block over the entire screen,
 * and then renders child elements inside of it.
 *
 * Caveat: child elements should be position: fixed.
 */
export function ClickAway({ onClick, children }: { onClick: () => void, children: React.ReactNode })
  : JSX.Element {
  return <div><div style={{
    position: "fixed", top: 0, left: 0, width: "100%", height: "100%",
    backgroundColor: "rgba(0,0,0, 0.1)",
    zIndex: 1,
  }}
    onClick={() => onClick()} />
    <div style={{ position: "fixed", zIndex: 2 }}>{children}</div>
  </div>;
}

function errorModal({ ptui, dispatch }: M.ReduxProps): JSX.Element {
  if (ptui.state.error) {
    return <SUI.Modal dimmer="inverted"
      open={true}
      onClose={() => dispatch({ type: "ClearError" })}>
      <SUI.Modal.Header>Error</SUI.Modal.Header>
      <SUI.Modal.Content>
        <div>{ptui.state.error}</div>
        <Button onClick={() => dispatch({ type: "ClearError" })}>Ok</Button>
      </SUI.Modal.Content>
    </SUI.Modal>;
  } else {
    return <noscript />;
  }
}
export const ErrorModal = M.connectRedux(errorModal);

interface TheLayoutProps {
  map: JSX.Element;
  tabs: Array<JSX.Element>;
  secondary?: JSX.Element;
  tertiary?: JSX.Element;
  bar_width: number;
  menu_size: MenuSize;
}
class TheLayoutComp extends React.Component<TheLayoutProps & M.ReduxProps,
  { width: number; height: number }> {

  constructor(props: TheLayoutProps & M.ReduxProps) {
    super(props);
    this.state = { width: window.innerWidth, height: window.innerHeight };
  }

  render(): JSX.Element {
    const { map, tabs, secondary, tertiary, ptui, dispatch, bar_width, menu_size } = this.props;

    const contents = this.state.width >= NARROW_THRESHOLD
      ? wideView()
      : narrowView(this.state.width);


    return <div style={{ height: "100%", width: "100%" }} >
      <WindowSizeListener
        onResize={({ windowWidth, windowHeight }) =>
          this.setState({ width: windowWidth, height: windowHeight })} />
      {contents}
      <ErrorModal />
    </div>;

    function bar(tabs_: Array<JSX.Element>, extra?: JSX.Element) {
      const tabbed_view = <TabbedView menu_size={menu_size}>{tabs_}</TabbedView>;
      return extra !== undefined
        ? <PanelGroup direction="column" borderColor="grey" spacing="8px">
          <div style={{ width: "100%" }}>{tabbed_view}</div>
          <div style={{ width: "100%" }}>{extra}</div>
        </PanelGroup>
        : tabbed_view;
    }

    function wideView() {
      return <div style={{ width: "100%", height: "100%", display: "flex" }}>
        {(secondary || tertiary)
          ? <div
            style={{
              height: "100%", width: "20%", minWidth: "20em",
            }}>
            <PanelGroup direction="column" borderColor="grey" spacing="8px" minHeight="10%">
              <div style={{ width: "100%", backgroundColor: "white", overflowY: "auto" }}>
                {tertiary}
              </div>
              <div style={{ width: "100%", backgroundColor: "white" }}>{secondary}</div>
            </PanelGroup>
          </div>
          : null}
        <div style={{ flex: "1" }}>{map}</div>
        <div style={{ width: bar_width, height: "100%" }}>
          {bar(tabs)}
        </div>
      </div>;
    }

    function narrowView(width: number) {
      const amended_tabs = LD.concat(tabs,
        <Tab key="Map" name="Map" always_render={true}>{map}</Tab>);
      const scale = width / bar_width;
      return <div style={{
        height: "100%",
        width: bar_width,
        zoom: `${scale * 100}%`,
      }}>
        <div style={{ width: bar_width }}>
          {bar(amended_tabs, secondary)}
        </div>
      </div>;
    }
  }
}

export const TheLayout = M.connectRedux(TheLayoutComp);

export function Icon(props: { children: Array<any> | any }): JSX.Element {
  return <i
    className="material-icons"
    style={{ MozUserSelect: "none", WebKitUserSelect: "none", msUserSelect: "none" }}
  >{props.children}</i>;
}

interface NoteEditorProps {
  path: T.FolderPath;
  name: string;
}
class NoteEditorComp
  extends React.Component<NoteEditorProps & M.ReduxProps, { content: string | undefined }> {
  constructor(props: NoteEditorProps & M.ReduxProps) {
    super(props);
    this.state = { content: undefined };
  }

  componentWillReceiveProps(nextProps: NoteEditorProps & M.ReduxProps) {
    if (!M.isEqual([this.props.path, this.props.name], [nextProps.path, nextProps.name])) {
      this.setState({ content: undefined });
    }
  }

  render(): JSX.Element {
    const self = this;

    const { path, name, ptui, dispatch } = this.props;

    const player_folder = ptui.getFolderNode(path);
    if (!player_folder) {
      return <div>Please ask your GM to create the folder "{M.folderPathToString(path)}"</div>;
    }
    const note = ptui.getNote(path, name);
    const origContent = note ? note.content : "Enter notes here!";
    const content = this.state.content !== undefined ? this.state.content : origContent;
    return <div style={{ height: "100%", display: "flex", flexDirection: "column" }}>
      <div style={{ display: "flex", justifyContent: "space-between" }}>
        <div>{M.folderPathToString(LD.concat(path, name))}</div>
        <Button
          disabled={this.state.content === undefined || this.state.content === origContent}
          onClick={() => submit(note)}>Save</Button>
      </div>
      <textarea style={{ flex: "1", resize: "none", width: "100%", height: "100%" }}
        value={content}
        onChange={e => this.setState({ content: e.currentTarget.value })} />
    </div>;

    function submit(origNote: T.Note | undefined) {
      if (!self.state.content) { return; }
      const newNote = { name, content: self.state.content };
      const cmd: T.GameCommand = origNote
        ? { t: "EditNote", path, name, note: newNote }
        : { t: "CreateNote", path, note: newNote };
      ptui.sendCommand(dispatch, cmd);
    }
  }
}
export const NoteEditor = M.connectRedux(NoteEditorComp);


export type ToggleFunc = () => void;
interface TogglerProps { a: (t: ToggleFunc) => JSX.Element; b: (t: ToggleFunc) => JSX.Element; }
export class Toggler extends React.Component<TogglerProps, { toggled: boolean }> {
  constructor(props: TogglerProps) {
    super(props);
    this.state = { toggled: false };
  }

  render(): JSX.Element {
    const self = this;
    function toggle() {
      self.setState({ toggled: !self.state.toggled });
    }
    if (this.state.toggled) {
      return this.props.b(toggle);
    } else {
      return this.props.a(toggle);
    }
  }
}


export function ModalMaker({ button, modal }: {
  button: (clicker: () => void) => JSX.Element,
  modal: (closer: () => void) => JSX.Element,
}) {
  return <Toggler
    a={button}
    b={tf =>
      <div>{button}<Modal dimmer='inverted' open={true} onClose={tf}>{modal(tf)}</Modal></div>}
  />;
}
