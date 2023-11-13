import isEqual from "lodash/isEqual";
import * as React from "react";
import useSWR from "swr";

import * as A from "./Actions";
import * as CV from "./CommonView";
import Connector from "./Connector";
import * as Grid from "./Grid";
import * as History from "./History";
import * as M from "./Model";
import * as T from "./PTTypes";

function PlayerGameView({ playerId }: { playerId: T.PlayerID }) {
  const { player, sceneId, visibleCreatures } = M.useState(s => {
    const player = s.game.players.get(playerId);
    const scene = player?.scene ? s.getScene(player.scene) : undefined;
    if (!scene) return { player, sceneId: undefined, visibleCreatures: undefined };
    const visibleCreatures = M.filterMap(
      scene?.creatures.entrySeq().toArray(),
      ([creatureId, [_, vis]]) => {
        if (vis === "AllPlayers") return creatureId;
      },
    );
    return { player, sceneId: scene?.id, visibleCreatures };
  }, isEqual);
  const combat = M.useState(s => s.getCombat());
  React.useEffect(() => {
    M.getState().setGridFocus(sceneId);
  }, [sceneId]);
  if (!player) {
    return <div>Player {playerId} not found</div>;
  }

  const tabs = [
    <CV.Tab key="Creatures" name="Creatures">
      <PlayerCreatures player={player} />
    </CV.Tab>,
    <CV.Tab key="Notes" name="Notes">
      <PlayerNote player_id={player.player_id} />
    </CV.Tab>,
  ];
  return (
    <CV.TheLayout
      map={visibleCreatures
        ? (
          <Grid.SceneGrid
            creatureIds={visibleCreatures}
            actionProducer={creatureMenuActions(player.creatures)}
          />
        )
        : <div>Ask your GM to put you in a scene</div>}
      bottom_right={<PlayerChat player_id={player.player_id} />}
      tabs={tabs}
      bar_width={325}
      menu_size="large"
      bottom_bar={<PlayerActionBar player={player} combat={combat} />}
    />
  );
}

function creatureMenuActions(playerCreatures: T.CreatureID[]) {
  function creatureMenuActions(
    state: M.AllStates,
    creatureId: T.CreatureID,
  ): Grid.Action[] {
    const actions = [];
    const combat = state.getCombat();
    if (combat) {
      if (state.getCurrentCombatCreatureID() === creatureId) {
        actions.push({ actionName: "Combat Move", action: A.requestCombatMovement });
      }
    } else if (playerCreatures.includes(creatureId)) {
      actions.push({ actionName: "Walk", action: A.requestMove });
    }
    return actions;
  }
  return creatureMenuActions;
}

function PlayerCreatures(props: { player: T.Player }) {
  const cids = props.player.creatures;
  const creatures = M.useState(s => s.getCreatures(cids));
  if (creatures.length === 0) {
    return <div>You have no creatures in your control yet.</div>;
  }
  return (
    <div>
      {creatures.map(creature => (
        <div key={creature.id}>
          <CV.CreatureCard creature={creature} />
          <div style={{ marginLeft: "1em" }}>
            <CV.CollapsibleInventory creature={creature} />
          </div>
        </div>
      ))}
    </div>
  );
}

function PlayerNote({ player_id }: { player_id: T.PlayerID }): JSX.Element {
  const path = ["Notes"];
  return (
    <CV.NoteEditor
      path={["Players", player_id].concat(path)}
      name="Scratch"
      disallow_rename={true}
      saveNote={saveNote}
    />
  );

  function saveNote(thingy: CV.CreateOrEdit, path: T.FolderPath, note: T.Note) {
    // we are ignoring the path because the backend just wants the part that's
    // relative to the player's folder.
    A.sendPlayerCommand({ ...thingy, path: ["Notes"], note });
  }
}

function PlayerActionBar(props: { player: T.Player; combat: T.Combat | null }) {
  const creature = M.useState(s => {
    const cid = s.getCurrentCombatCreatureID();
    if (cid) return s.getCreature(cid);
  });
  if (props.combat) {
    if (creature) {
      if (props.player.creatures.includes(creature.id)) {
        return <CV.ActionBar combat={props.combat} creatureId={creature.id} />;
      } else {
        return <div>{creature.name} is acting</div>;
      }
    } else {
      return <div>Creature disappeared!</div>;
    }
  } else {
    return <div>TODO: out-of-combat actions</div>;
  }
}

interface PlayerChatProps {
  player_id: T.PlayerID;
}
function PlayerChat(props: PlayerChatProps): JSX.Element {
  const { player_id } = props;
  return <CV.GenericChat renderLog={get_chat_line} sendChat={sendChat} />;

  function get_chat_line(log: T.GameLog) {
    if (log.t === "ChatFromPlayer" || log.t === "ChatFromGM") {
      return <CV.ChatLog log={log} />;
    }
    if (log.t === "CreatureLog") {
      return <History.CreatureLog creatureId={log.creature_id} log={log.log} />;
    }
  }

  function sendChat(line: string) {
    A.sendPlayerCommand({ t: "ChatFromPlayer", message: line });
  }
}

export default function PlayerView() {
  let {
    data: games,
    error,
    isLoading,
  } = useSWR("/g/list", (k) => A.ptfetch(k, {}, T.decodeGameList), {
    revalidateIfStale: false,
    revalidateOnFocus: false,
    revalidateOnReconnect: false,
  });
  let gameId = M.useState(s => s.gameId);
  console.log("[PlayerGame]", { isLoading, games, gameId });

  const playerId = games?.games.find(
    ([profile, _meta]) => profile.game_id === gameId && profile.role === "Player",
  )?.[0].profile_name;
  React.useEffect(() => {
    M.getState().setPlayerId(playerId);
  }, [playerId]);

  if (isLoading || !games) {
    return <div>Loading...</div>;
  }

  return (
    <Connector role="Player">
      {playerId
        ? <PlayerGameView playerId={playerId} />
        : <div>Loading. or maybe we can't find your player. Who knows?</div>}
    </Connector>
  );
}
