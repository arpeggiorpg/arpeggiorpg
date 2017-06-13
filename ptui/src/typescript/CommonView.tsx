import * as React from "react";
import * as LD from "lodash";
import * as T from './PTTypes';
import { PTUI } from './Model';
import * as M from './Model';

export class Collapsible extends React.Component<{ name: string }, { collapsed: boolean }> {
  constructor(props: { name: string }) {
    super(props);
    this.state = { collapsed: false };
  }
  toggle() {
    this.setState({ collapsed: !this.state.collapsed });
  }
  render(): JSX.Element {
    let buttonText, noneOrBlock;
    if (this.state.collapsed) {
      buttonText = "▶"; noneOrBlock = "none";
    }
    else {
      buttonText = "▼"; noneOrBlock = "block";
    };
    return <div>
      <div style={{ display: "flex" }}>
        <strong>{this.props.name}</strong>
        <button onClick={this.toggle.bind(this)}>{buttonText}</button>
      </div>
      <div style={{ display: noneOrBlock }}>{this.props.children}</div>
    </div>
  }
}

export class CreatureCard extends React.Component<{ creature: T.Creature; app: T.App }, undefined> {
  render(): JSX.Element {
    let creature = this.props.creature;
    return <div
      style={{
        width: "300px",
        borderRadius: "10px", border: "1px solid black",
        padding: "3px"
      }}>
      <div>{classIcon(creature)} <strong>{creature.name}</strong>
        {LD.values(creature.conditions).map((ac) => conditionIcon(ac.condition))}
      </div>
      <CreatureIcon app={this.props.app} creature={creature} />
    </div>;
  }
}

export function classIcon(creature: T.Creature): string {
  switch (creature.class_) {
    case "cleric": return "💉";
    case "rogue": return "🗡️";
    case "ranger": return "🏹";
    case "creature": return "🏃";
    case "baddie": return "👹";
    default: return ""
  }
}

export function CreatureIcon(props: { app: T.App, creature: T.Creature }): JSX.Element | null {
  let squareStyle = { width: "50px", height: "50px", borderRadius: "10px", border: "solid 1px black" };
  if (props.creature.portrait_url !== "") {
    return <img src={props.creature.portrait_url}
      style={squareStyle} />
  } else {
    let class_ = M.get(props.app.current_game.classes, props.creature.class_);
    let color = class_ ? class_.color : "red";
    return <div style={{ backgroundColor: color, ...squareStyle }}>{props.creature.name}</div>
  }
}

interface CreatureInventoryProps { ptui: PTUI; current_scene: T.SceneID | undefined; creature: T.Creature }
export class CreatureInventory extends React.Component<CreatureInventoryProps, { giving: T.ItemID | undefined }> {
  constructor(props: CreatureInventoryProps) {
    super(props);
    this.state = { giving: undefined };
  }
  render(): JSX.Element | null {
    let inv = this.props.creature.inventory;
    let items = this.props.ptui.getItems(LD.keys(inv));

    let give = this.state.giving
      ? <GiveItem ptui={this.props.ptui} current_scene={this.props.current_scene}
        giver={this.props.creature.id} item_id={this.state.giving} onClose={() => { this.setState({ giving: undefined }) }} />
      : <noscript />;

    return <div>
      {items.map((item) =>
        <div key={item.id} style={{ display: "flex", justifyContent: "space-between" }}>
          {item.name} ({M.get(inv, item.id)})
        <button onClick={(e) => this.setState({ giving: item.id })}>Give</button>
        </div>
      )}
      {give}
    </div>;
  }
}

interface GiveItemProps { ptui: PTUI; current_scene: T.SceneID | undefined; item_id: T.ItemID; giver: T.CreatureID; onClose: () => void }
export class GiveItem extends React.Component<GiveItemProps, { receiver: T.CreatureID | undefined; count: number | undefined }> {
  constructor(props: GiveItemProps) {
    super(props);
    this.state = { receiver: undefined, count: 1 };
  }
  render(): JSX.Element {
    let ptui = this.props.ptui;
    if (!this.props.current_scene) { return <div>You can only transfer items in a scene.</div> }
    let scene = M.get(ptui.app.current_game.scenes, this.props.current_scene);
    if (!scene) { return <div>Couldn't find your scene</div> }
    let other_cids_in_scene = LD.keys(scene.creatures);
    LD.pull(other_cids_in_scene, this.props.giver);
    let other_creatures = ptui.getCreatures(other_cids_in_scene);
    if (!other_creatures) { return <div>There is nobody in this scene to give items to.</div> }
    let item = ptui.getItem(this.props.item_id);
    if (!item) { return <div>The Item definition cannot be found.</div> }
    let _giver = ptui.getCreature(this.props.giver);
    if (!_giver) { return <div>Giver not found!</div> }
    let giver = _giver;
    let giver_count = M.get(giver.inventory, this.props.item_id);
    if (!giver_count) { return <div>{giver.name} does not have any {item.name} to give.</div> }
    return <div>
      Giving
      <PositiveIntegerInput
        max={giver_count}
        onChange={(num) => this.setState({ count: num })}
        value={this.state.count} />
      {item.name}
      from {giver.name} to
      <select value={this.state.receiver} onChange={(ev) => this.onSelectCreature(ev)}>
        <option key="undefined" value="">Choose a creature</option>
        {other_creatures.map(
          (creature) => <option key={creature.id} value={creature.id}>{creature.name}</option>
        )}
      </select>
      <button disabled={!(this.state.receiver && this.state.count)} onClick={ev => this.give(giver)}>Give</button>
      <button onClick={ev => this.props.onClose()}>Cancel</button>
    </div>;
  }

  onSelectCreature(event: React.SyntheticEvent<HTMLSelectElement>) {
    this.setState({ receiver: event.currentTarget.value });
  }

  give(giver: T.Creature) {
    let count = this.state.count as number; // Protected by button `disabled`
    let receiver_id = this.state.receiver as T.CreatureID; // Protected by button `disabled`
    let receiver = M.get(this.props.ptui.app.current_game.creatures, receiver_id);
    if (!receiver) {
      console.log("[give] Receiver has disappeared", receiver_id);
      this.props.onClose();
      return;
    }

    let newGiver = LD.assign({}, giver, { inventory: M.removeFromInventory(giver.inventory, this.props.item_id, count) });
    let newReceiver = LD.assign({}, receiver, { inventory: M.addToInventory(receiver.inventory, this.props.item_id, count) });

    this.props.ptui.sendCommand({ t: "EditCreature", creature: newGiver });
    this.props.ptui.sendCommand({ t: "EditCreature", creature: newReceiver });
    this.props.onClose();
  }
}

interface PositiveIntegerInputProps { max?: number; value: number | undefined; onChange: (num: number | undefined) => void }
export class PositiveIntegerInput extends React.Component<PositiveIntegerInputProps, undefined> {
  render(): JSX.Element {
    return <input type="text" value={this.props.value === undefined ? "" : this.props.value} onChange={(event) => {
      let num = Number(event.currentTarget.value);
      if (event.currentTarget.value === "") {
        this.props.onChange(undefined);
      } else if (num) {
        if (this.props.max !== undefined && num > this.props.max) { num = this.props.max; }
        this.props.onChange(num);
      }
    }} />
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

interface TabbedViewProps { children: Array<JSX.Element> }
export class TabbedView extends React.Component<TabbedViewProps, { selected: number }> {

  constructor(props: TabbedViewProps) {
    super(props);
    this.state = { selected: 0 };
  }

  render(): JSX.Element {
    let children_names = React.Children.map(
      this.props.children,
      (c: any): string | undefined => { if (c.type === Tab) { return c.props.name; } });
    let selectedView = M.idx(this.props.children, this.state.selected);
    return <div>
      <div style={{ display: "flex" }}>
        {children_names.map((name, index) =>
          <button key={name} style={{ display: "block", width: "auto", height: "auto" }}
            onClick={() => this.setState({ selected: index })}>
            {name}</button>)
        }
      </div>
      {selectedView}
    </div>;
  }
}

interface TabProps { name: string }
export class Tab extends React.Component<TabProps, undefined> {
  render(): JSX.Element {
    return React.Children.only(this.props.children);
  }
}

export function Combat(props: { ptui: PTUI }): JSX.Element {
  if (!props.ptui.app.current_game.current_combat) {
    return <div>There is no combat!</div>
  }
  let combat = props.ptui.app.current_game.current_combat;
  let creatures_with_init = M.filterMap(combat.creatures.data,
    ([cid, init]) => {
      let creature = props.ptui.getCreature(cid);
      if (creature) { return [creature, init]; }
    }) as Array<[T.Creature, number]>;

  return <div>
    {creatures_with_init.map(([creature, init], index) => {
      return <div key={creature.id} style={{ display: "flex" }}>
        <div style={{ width: "25px" }}>{index === combat.creatures.cursor ? "▶️" : ""}</div>
        <CreatureCard app={props.ptui.app} creature={creature} />
      </div>;
    })
    }
  </div>;
}

export function ActionBar(props: { creature: T.Creature; ptui: PTUI; combat?: T.Combat }): JSX.Element {
  let abilities = M.filterMap(LD.values(props.creature.abilities),
    (abstatus) => {
      let ability = M.get(props.ptui.app.current_game.abilities, abstatus.ability_id);
      if (ability) {
        return { ability_id: abstatus.ability_id, ability: ability };
      }
    });

  let abilityButtons;
  if (props.combat) {
    let combat = props.combat;
    abilityButtons = abilities.map((abinfo) =>
      <AbilityButton key={abinfo.ability_id}
        ptui={props.ptui} creature={props.creature} abinfo={abinfo}
        scene_id={combat.scene} />);
  }
  else {
    abilityButtons = <noscript />;
  }
  return <div style={{ display: "flex" }}>
    <CreatureIcon app={props.ptui.app} creature={props.creature} />
    {props.combat ? <DoneButton ptui={props.ptui} /> : <noscript />}
    <MoveButton ptui={props.ptui} creature={props.creature} combat={props.combat} />
    {abilityButtons}
  </div>;
}

function DoneButton(props: { ptui: PTUI }): JSX.Element {
  let command: T.GameCommand = { t: "Done" };
  return <button style={{ width: "50px", height: "50px" }}
    onClick={() => props.ptui.sendCommand(command)}>
    Done
    </button>
}

interface AbilityButtonProps {
  ptui: PTUI;
  creature: T.Creature;
  abinfo: { ability_id: T.AbilityID; ability: T.Ability };
  scene_id: T.SceneID;
}
function AbilityButton(props: AbilityButtonProps): JSX.Element {
  let onClick = () =>
    props.ptui.requestCombatAbility(
      props.creature.id, props.abinfo.ability_id, props.abinfo.ability, props.scene_id);
  return <button style={{ width: "50px", height: "50px" }}
    onClick={onClick}>
    {props.abinfo.ability.name}
  </button>;
}

function MoveButton(props: { ptui: PTUI, creature: T.Creature; combat?: T.Combat }): JSX.Element {
  let movement_left = props.combat ? props.creature.speed - props.combat.movement_used : 0;
  let suffix = props.combat ? " (" + movement_left / 100 + ")" : "";
  return <button style={{ width: "50px", height: "50px" }}
    onClick={() => props.ptui.requestCombatMovement()}>
    Move {suffix}
  </button>
}
