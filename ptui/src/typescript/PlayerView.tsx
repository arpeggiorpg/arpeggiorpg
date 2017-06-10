import * as React from "react";
import * as ReactDOM from "react-dom";
import * as T from './PTTypes';
import * as CommonView from './CommonView';
import { PTUI } from './Model';

export function renderPlayerUI(
  elmApp: any,
  [id, player_id, current_scene, data]: [string, T.PlayerID, T.SceneID | undefined, any]) {
  let element = document.getElementById(id);
  console.log("[renderPlayerUI] Rendering Player component from Elm", id, element, player_id, current_scene);
  let app = T.decodeApp.decodeAny(data);
  let ptui = new PTUI(elmApp, app);
  ReactDOM.render(
    <PlayerUI ptui={ptui} player_id={player_id} current_scene={current_scene} />,
    element
  );
}

class PlayerUI extends React.Component<
  { player_id: T.PlayerID; current_scene: string | undefined; ptui: PTUI; },
  undefined> {

  render(): JSX.Element {
    console.log("[PlayerUI:render]");

    return <CommonView.TabbedView>
      <CommonView.Tab name="Creatures">
        <PlayerCreatures player_id={this.props.player_id} current_scene={this.props.current_scene} ptui={this.props.ptui} />
      </CommonView.Tab>
      <CommonView.Tab name="Combat">
        <div>Combat!</div>
      </CommonView.Tab>
    </CommonView.TabbedView>;
  }
}

class PlayerCreatures extends React.Component<{ current_scene: T.SceneID | undefined; player_id: T.PlayerID; ptui: PTUI; }, undefined> {

  creatureSection(creature: T.Creature): JSX.Element {
    return <div key={creature.id}>
      <CommonView.CreatureCard app={this.props.ptui.app} creature={creature} />
      <div style={{ marginLeft: "1em" }}>
        <CommonView.Collapsible name="Inventory">
          <CommonView.CreatureInventory ptui={this.props.ptui} current_scene={this.props.current_scene} creature={creature} />
        </CommonView.Collapsible>
      </div>
    </div>;
  }

  render(): JSX.Element {
    let cids = this.props.ptui.app.players[this.props.player_id].creatures;
    let creatures = T.getCreatures(this.props.ptui.app, cids);
    return <div>
      {creatures.map(this.creatureSection.bind(this))}
    </div>
  }
}
