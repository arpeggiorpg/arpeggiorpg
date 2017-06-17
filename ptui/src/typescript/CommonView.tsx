import * as LD from "lodash";
import * as React from "react";

import { PTUI } from './Model';
import * as M from './Model';
import * as T from './PTTypes';

export class Collapsible extends React.Component<{ name: string }, { collapsed: boolean }> {
  constructor(props: { name: string }) {
    super(props);
    this.state = { collapsed: false };
  }
  toggle() {
    this.setState({ collapsed: !this.state.collapsed });
  }
  render(): JSX.Element {
    let buttonText;
    let noneOrBlock;
    if (this.state.collapsed) {
      buttonText = "▶"; noneOrBlock = "none";
    } else {
      buttonText = "▼"; noneOrBlock = "block";
    }
    return <div>
      <div style={{ display: "flex" }}>
        <strong>{this.props.name}</strong>
        <button onClick={() => this.toggle()}>{buttonText}</button>
      </div>
      <div style={{ display: noneOrBlock }}>{this.props.children}</div>
    </div>;
  }
}

export class CreatureCard extends React.Component<{ creature: T.Creature; app: T.App }, undefined> {
  render(): JSX.Element {
    const creature = this.props.creature;
    return <div
      style={{
        width: "300px",
        borderRadius: "10px", border: "1px solid black",
        padding: "3px",
      }}>
      <div>{classIcon(creature)} <strong>{creature.name}</strong>
        {LD.values(creature.conditions).map(ac => conditionIcon(ac.condition))}
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

interface CreatureInventoryProps {
  ptui: PTUI;
  current_scene: T.SceneID | undefined;
  creature: T.Creature;
}
export class CreatureInventory extends React.Component<CreatureInventoryProps,
  { giving: T.ItemID | undefined }> {
  constructor(props: CreatureInventoryProps) {
    super(props);
    this.state = { giving: undefined };
  }
  render(): JSX.Element | null {
    const inv = this.props.creature.inventory;
    const items = this.props.ptui.getItems(inv.keySeq().toArray());

    const give = this.state.giving
      ? <GiveItem giver={this.props.creature.id} item_id={this.state.giving}
        onClose={() => this.setState({ giving: undefined })} />
      : <noscript />;

    return <div>
      {items.map(item =>
        <div key={item.id} style={{ display: "flex", justifyContent: "space-between" }}>
          {item.name} ({inv.get(item.id)})
        <button onClick={e => this.setState({ giving: item.id })}>Give</button>
        </div>
      )}
      {give}
    </div>;
  }
}

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
    return <div>
      Giving
      <PositiveIntegerInput
        max={giver_count}
        onChange={num => this.setState({ count: num })}
        value={this.state.count} />
      {item.name}
      from {giver.name} to
      <select value={this.state.receiver} onChange={ev => this.onSelectCreature(ev)}>
        <option key="undefined" value="">Choose a creature</option>
        {other_creatures.map(
          creature => <option key={creature.id} value={creature.id}>{creature.name}</option>
        )}
      </select>
      <button disabled={!(this.state.receiver && this.state.count)} onClick={ev => this.give(giver)}>
        Give
      </button>
      <button onClick={ev => this.props.onClose()}>Cancel</button>
    </div>;
  }

  onSelectCreature(event: React.SyntheticEvent<HTMLSelectElement>) {
    this.setState({ receiver: event.currentTarget.value });
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
export class PositiveIntegerInput extends React.Component<PositiveIntegerInputProps, undefined> {
  render(): JSX.Element {
    return <input type="text" value={this.props.value === undefined ? "" : this.props.value}
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
    const children_names = React.Children.map(
      this.props.children,
      (c: any): string | undefined => { if (c && c.type === Tab) { return c.props.name; } });
    const selectedView = M.idx<JSX.Element | null>(this.props.children, this.state.selected);
    if (!selectedView) { return <div>woops</div>; }
    return <div>
      <div style={{ display: "flex" }}>
        {children_names.map((name, index) =>
          <button key={name} style={{ display: "block", height: "40px", flex: "1" }}
            onClick={() => this.setState({ selected: index })}>
            {name}</button>)
        }
      </div>
      {selectedView}
    </div>;
  }
}

interface TabProps { name: string; }
export class Tab extends React.Component<TabProps, undefined> {
  render(): JSX.Element {
    return React.Children.only(this.props.children);
  }
}

export function Combat(props: { ptui: PTUI }): JSX.Element {
  if (!props.ptui.app.current_game.current_combat) {
    return <div>There is no combat!</div>;
  }
  const combat = props.ptui.app.current_game.current_combat;
  const creatures_with_init = M.filterMap(combat.creatures.data,
    ([cid, init]) => {
      const creature = props.ptui.getCreature(cid);
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

export function ActionBar(props: { creature: T.Creature; ptui: PTUI; combat?: T.Combat })
  : JSX.Element {
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
}

export const DoneButton = M.connectRedux(({ ptui, dispatch }: M.ReduxProps): JSX.Element => {
  const command: T.GameCommand = { t: "Done" };
  return <button style={{ height: "50px", flex: "1" }}
    onClick={() => ptui.sendCommand(dispatch, command)}>
    Done
  </button >;
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
  return <button style={{ height: "50px", flex: "1" }}
    onClick={onClick}>
    {props.abinfo.ability.name}
  </button>;
});

const MoveButton = M.connectRedux((props: { creature: T.Creature; combat?: T.Combat } & M.ReduxProps)
  : JSX.Element => {
  const movement_left = props.combat ? props.creature.speed - props.combat.movement_used : 0;
  const suffix = props.combat ? " (" + Number(movement_left / 100).toFixed(0) + ")" : "";
  return <button style={{ height: "50px", flex: "1" }}
    onClick={() => props.ptui.requestCombatMovement(props.dispatch)}>
    Move {suffix}
  </button>;
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
