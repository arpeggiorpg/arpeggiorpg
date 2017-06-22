import * as LD from "lodash";
import * as React from "react";
import PanelGroup from 'react-panelgroup';
import { Provider } from 'react-redux';
import * as WindowSizeListener from 'react-window-size-listener';
import * as Redux from 'redux';

// import 'semantic-ui-css/semantic.min.css';
import { Accordion, Button, Form, Input, Menu, Segment } from 'semantic-ui-react';
import * as SUI from 'semantic-ui-react';


import { PTUI } from './Model';
import * as M from './Model';
import * as T from './PTTypes';

const SIDE_BAR_WIDTH = 450;

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
    return <div
      style={{
        width: "300px",
        borderRadius: "10px", border: "1px solid black",
        padding: "3px",
      }}>
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
    </div>;

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

export function CreatureIcon(props: { app: T.App, creature: T.Creature }): JSX.Element | null {
  const squareStyle = {
    width: "50px", height: "50px",
    borderRadius: "10px", border: "solid 1px black",
  };
  if (props.creature.portrait_url !== "") {
    return <img src={props.creature.portrait_url}
      style={squareStyle} />;
  } else {
    const class_ = M.get(props.app.current_game.classes, props.creature.class_);
    const color = class_ ? class_.color : "red";
    return <div style={{ backgroundColor: color, ...squareStyle }}>{props.creature.name}</div>;
  }
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
    const other_cids_in_scene = LD.keys(scene.creatures);
    LD.pull(other_cids_in_scene, this.props.giver);
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
    const receiver = M.get(this.props.ptui.app.current_game.creatures, receiver_id);
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

interface TabbedViewProps { children: Array<JSX.Element | null>; }
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
    return <div>
      <Menu pointing={true}>
        {children.map((child, index) =>
          <Menu.Item key={child.props.name} name={child.props.name}
            active={this.state.selected === index}
            onClick={() => this.setState({ selected: index })} />)
        }
      </Menu>
      {children.map((child, index) => {
        const display = index === this.state.selected ? "block" : "none";
        return <div key={child.props.name} style={{ display }}>{child}</div>;
      })}
    </div>;
  }
}

interface TabProps { name: string; }
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
    return <div>
      {creatures_with_init.map(([creature, init], index) => {
        const show_init = initiative ? initiative(creature, init) : null;
        return <div key={creature.id} style={{ display: "flex" }}>
          <div style={{ width: "25px", display: "flex", flexDirection: "column" }}>
            <div style={{ height: "25px" }}>{index === combat.creatures.cursor ? "▶️" : ""}</div>
            <div>{show_init}</div>
          </div>
          <Card creature={creature} />
        </div>;
      })
      }
    </div>;
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

function modal({ ptui, dispatch }: M.ReduxProps): JSX.Element {
  if (ptui.state.error) {
    return <div style={{
      position: "fixed", top: "50%", left: "50%", transform: "translate(-50%, -50%)",
      backgroundColor: "white",
      border: "1px solid black",
      minHeight: "30%",
      minWidth: "30%",
      borderRadius: "5px",
      display: "flex",
      flexDirection: "column",
    }}>
      <h1>Error</h1>
      <div style={{ flex: "1 0 auto" }}>{ptui.state.error}</div>
      <div style={{ display: "flex", justifyContent: "space-around" }}>
        <div>
          <button style={{ minHeight: "40px", minWidth: "80px" }}
            onClick={() => dispatch({ type: "ClearError" })}>Ok</button>
        </div>
      </div>
    </div>;
  } else {
    return <noscript />;
  }
}
export const Modal = M.connectRedux(modal);

interface TheLayoutProps {
  map: JSX.Element;
  tabs: Array<JSX.Element>;
  secondary?: JSX.Element;
}
class TheLayoutComp extends React.Component<TheLayoutProps & M.ReduxProps,
  { width: number; height: number }> {

  constructor(props: TheLayoutProps & M.ReduxProps) {
    super(props);
    this.state = { width: window.innerWidth, height: window.innerHeight };
  }

  render(): JSX.Element {
    const { map, tabs, secondary, ptui, dispatch } = this.props;

    const contents = this.state.width >= (2 * SIDE_BAR_WIDTH)
      ? wideView()
      : narrowView(this.state.width);


    return <div style={{ height: "100%", width: "100%" }} >
      <WindowSizeListener
        onResize={({ windowWidth, windowHeight }) =>
          this.setState({ width: windowWidth, height: windowHeight })} />
      {contents}
      <Modal />
    </div>;

    function bar(tabs_: Array<JSX.Element>, extra?: JSX.Element) {
      const top = <div style={{ flex: "1", border: "1px solid black" }}>
        <TabbedView>
          {tabs_}
        </TabbedView>
      </div>;
      return extra !== undefined
        ? <PanelGroup direction="column" borderColor="grey" spacing="8px">
          {top}
          <div style={{ width: "100%" }}>{extra}</div>
        </PanelGroup>
        : <div style={{
          display: "flex", flexDirection: "column", height: "100%",
          overflowY: "auto", overflowX: "hidden", // :(
        }}>{top}</div>;
    }

    function wideView() {
      return <div style={{ width: "100%", height: "100%", display: "flex" }}>
        {secondary
          ? <div
            style={{
              position: "fixed", top: 0, left: 0, height: "100%",
              width: "20%", minWidth: "20em",
            }}>
            <PanelGroup direction="column" borderColor="grey" spacing="8px">
              <div />
              <div style={{ width: "100%", backgroundColor: "white" }}>{secondary}</div>
            </PanelGroup>
          </div>
          : null}
        <div style={{ flex: "1" }}>{map}</div>
        <div style={{ width: SIDE_BAR_WIDTH, height: "100%" }}>
          {bar(tabs)}
        </div>
      </div>;
    }

    function narrowView(width: number) {
      const amended_tabs = LD.concat(tabs,
        <Tab key="Map" name="Map">{map}</Tab>);
      const scale = width / SIDE_BAR_WIDTH;
      return <div style={{
        height: "100%",
        width: SIDE_BAR_WIDTH,
        zoom: `${scale * 100}%`,
      }}>
        <div style={{ width: SIDE_BAR_WIDTH }}>
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
    console.log("[NoteEditor] componentWillReceiveProps");
    if (!M.isEqual([this.props.path, this.props.name], [nextProps.path, nextProps.name])) {
      console.log("[NoteEditor] resetting state!");
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
      <div style={{ flex: "1" }}>
        <textarea style={{ resize: "none", width: "100%", height: "100%" }}
          value={content}
          onChange={e => this.setState({ content: e.currentTarget.value })} /></div>
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


export class IconMenu
  extends React.Component<{ children?: any }, { expanded: boolean }> {

  constructor(props: { children?: any }) {
    super(props);
    this.state = { expanded: false };
  }

  render(): JSX.Element {
    const self = this;
    const open = this.state.expanded
      ? <ClickAway onClick={() => this.setState({ expanded: false })}>
        <div style={{
          position: "absolute", right: "-1.8em", border: "1px solid black",
          backgroundColor: "white",
          boxShadow: "5px 5px 2px -2px rgb(128,128,128)",
          whiteSpace: "nowrap",
        }}>
          {React.Children.map(this.props.children, child => {
            if (typeof child !== "string" && typeof child !== "number" && child.props) {
              if (child.props.children) {
                const onClick = child.props.onClick
                  ? () => { self.setState({ expanded: false }); child.props.onClick(); }
                  : undefined;
                const cursor = child.props.onClick ? "pointer" : "auto";
                return <div onClick={onClick} style={{ cursor }}>{child}</div>;
              }
            }
          })}
        </div>
      </ClickAway>
      : null;


    return <div style={{ position: "relative" }}>
      <div style={{ cursor: "pointer" }}
        onClick={() => this.setState({ expanded: !this.state.expanded })}>
        <Icon>menu</Icon></div>
      {open}
    </div>;
  }
}


export class MenuItem extends React.Component<{ onClick?: () => void }, undefined> {
  render(): JSX.Element {
    return <div style={{
      marginLeft: "1em", marginRight: "1em",
      marginTop: "0.25em", marginBottom: "0.25em",
    }}>{this.props.children}</div>;
  }
}

export class MenuHeader extends React.Component<{}, undefined> {
  render(): JSX.Element {
    return <div style={{
      marginLeft: "1em", marginRight: "1em", marginTop: "0.25em",
      fontWeight: "bold", borderBottom: "1px solid gray",
    }}>
      {this.props.children}
    </div>;
  }
}

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
