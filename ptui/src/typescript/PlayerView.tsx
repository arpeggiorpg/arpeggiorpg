import * as React from "react";
import * as ReactDOM from "react-dom";
import * as LD from "lodash";
import * as T from './PTTypes';
import * as CommonView from './CommonView';

export function renderPlayerUI(
  elmApp: any,
  [id, player_id, current_scene, data]: [string, T.PlayerID, T.SceneID | undefined, any]) {
  let element = document.getElementById(id);
  console.log("[renderPlayerUI] Rendering Player component from Elm", id, element, player_id, current_scene);
  let app = T.decodeApp.decodeAny(data);
  ReactDOM.render(
    <PlayerUI app={app} player_id={player_id} current_scene={current_scene} />,
    element
  );
}

class PlayerUI extends React.Component<
  { player_id: T.PlayerID; current_scene: string | undefined; app: T.App; },
  undefined> {

  render(): JSX.Element {
    console.log("[PlayerUI:render]");
    return <div>
      <div>Player: {this.props.player_id}</div>
      <PlayerCreatures player_id={this.props.player_id} current_scene={this.props.current_scene} app={this.props.app} />
    </div>;
  }
}

class PlayerCreatures extends React.Component<{ current_scene: T.SceneID | undefined; player_id: T.PlayerID; app: T.App; }, undefined> {

  creatureSection(creature: T.Creature): JSX.Element {
    return <div key={creature.id}>
      <CommonView.CreatureCard app={this.props.app} creature={creature} />
      <div style={{ marginLeft: "1em" }}>
        <CommonView.Collapsible name="Inventory">
          <CommonView.CreatureInventory app={this.props.app} current_scene={this.props.current_scene} creature={creature} />
        </CommonView.Collapsible>
      </div>
    </div>;
  }

  render(): JSX.Element {
    let cids = this.props.app.players[this.props.player_id].creatures;
    let creatures = T.getCreatures(this.props.app, cids);
    return <div>
      {creatures.map(this.creatureSection.bind(this))}
    </div>
  }
}
