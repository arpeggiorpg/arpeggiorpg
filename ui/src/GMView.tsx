import { Map } from "immutable";
import isEqual from "lodash/isEqual";
import mapValues from "lodash/mapValues";
import * as React from "react";

import { Outlet, useParams } from "react-router-dom";
import * as A from "./Actions";
import * as Campaign from "./Campaign";
import * as CV from "./CommonView";
import Connector from "./Connector";
import * as GM from "./GMComponents";
import * as Grid from "./Grid";
import * as History from "./History";
import * as M from "./Model";
import * as Players from "./Players";
import * as T from "./PTTypes";
import * as Scene from "./Scene";

export default function GMView() {
  return (
    <Connector role="GM">
      <React.Suspense fallback={<div>Loading...</div>}>
        <GMMain />
      </React.Suspense>
    </Connector>
  );
}

function GMMain() {
  const numPlayers = M.useState(s => s.game.players.count());
  const combatActive = M.useState(s => !!s.game.current_combat);

  const tabs = [
    <CV.Tab key="Campaign" name="Campaign">
      <Campaign.Campaign />
    </CV.Tab>,
    <CV.Tab key="Combat" name={`Combat ${combatActive ? "(!)" : ""}`}>
      <GM.GMCombat />
    </CV.Tab>,
    <CV.Tab key="Players" name={`Players (${numPlayers})`}>
      <div style={{ display: "flex", flexDirection: "column" }}>
        <Players.Players />
        <Players.Invitations />
      </div>
    </CV.Tab>,
    <CV.Tab key="History" name="History">
      <History.History />
    </CV.Tab>,
    <CV.Tab key="Extras" name="Extras">
      <GMExtras />
    </CV.Tab>,
  ];

  const combat = M.useState((s) => s.getCombat());
  const currentCreatureInCombat = M.useState((s) => s.getCurrentCombatCreatureID());

  const bottom_bar = combat && currentCreatureInCombat
    ? <CV.ActionBar creatureId={currentCreatureInCombat} combat={combat} />
    : undefined;

  return (
    <CV.TheLayout
      map={<Outlet />}
      tabs={tabs}
      bottom_left={<Secondary />}
      top_left={<Scene.GMScene />}
      bottom_right={<GMChat />}
      bar_width={450}
      menu_size="tiny"
      bottom_bar={bottom_bar}
    />
  );
}

function GMExtras() {
  return (
    <>
      <h2>Download game</h2>
      <GM.ExportGame />
    </>
  );
}

function Secondary() {
  const focus2 = M.useState((s) => s.secondaryFocus);
  if (!focus2) {
    return undefined;
  }
  switch (focus2.t) {
    case "Note":
      return (
        <CV.NoteEditor
          path={focus2.path}
          name={focus2.name}
          // We need to refocus with the new name after a note gets renamed:
          saveNote={saveNote}
        />
      );
    case "Creature":
      return <GM.CreatureFocus creatureId={focus2.creature_id} />;
    case "Item":
      return <GM.GMViewItem itemId={focus2.item_id} />;
    case "Class":
      return <GM.ClassEditor classId={focus2.class_id} />;
    case "Ability":
      return <GM.AbilityEditor abilityId={focus2.ability_id} />;
  }
  M.assertNever(focus2);

  function saveNote(thingy: CV.CreateOrEdit, path: T.FolderPath, note: T.Note) {
    A.sendGMCommand({ ...thingy, path, note });
    M.getState().setSecondaryFocus({ t: "Note", path, name: note.name });
  }
}

export function GMMap() {
  const { "*": path } = useParams();
  const { sceneId, creaturesInMap } = M.useState((s) => {
    if (!path) return { scene: undefined, creaturesInMap: undefined };
    const scene = getSceneFromPath(s, path);
    if (!scene) return { scene: undefined, creaturesInMap: undefined };
    const creaturesInMap = mapCreatures(scene, s);
    return { sceneId: scene.id, creaturesInMap };
  }, isEqual);
  React.useEffect(() => {
    // We need to synchronize the scene from the path to the zustand store
    console.log("setting grid focus to", sceneId);
    M.getState().setGridFocus(sceneId);
  }, [path, sceneId]);
  if (!sceneId) {
    return <div>Couldn't find scene {path}</div>;
  }
  return <Grid.SceneGrid creatures={creaturesInMap} />;
}

function getSceneFromPath(
  state: M.AllStates,
  pathstr: string,
): T.Scene | undefined {
  let path: T.FolderPath;
  try {
    path = T.decodeFolderPath.parse(`/${pathstr}`);
  } catch (e) {
    console.error("Couldn't parse path", pathstr);
    return undefined;
  }
  let sceneName = path.at(-1);
  let folder = M.getFolder(state.game.campaign, path.slice(0, -1));
  if (!folder) {
    console.error("Couldn't find folder", path.slice(0, -1));
    return;
  }
  for (const sceneId of folder.data.scenes) {
    let scene = state.game.scenes.get(sceneId);
    if (!scene) {
      console.info("skipping unknown scene", sceneId);
      continue;
    }
    // WARNING! WARNING!
    // It's possible for there to be multiple scenes with the same name in the same folder!!!!!
    // We need to make that impossible in the backend!
    if (scene.name === sceneName) {
      return scene;
    }
  }
  console.error("Couldn't find scene with name", sceneName);
}

/** Create `MapCreature`s for all creatures in a scene, and annotate them with GM-specific actions.
 */
function mapCreatures(
  scene: T.Scene,
  state: M.AllStates,
): { [index: string]: Grid.MapCreature } {
  return mapValues(Grid.mapCreatures(state, scene), (mapc) => ({
    ...mapc,
    actions: mapc.actions.concat(
      creatureMenuActions(
        state,
        scene,
        state.getGame().current_combat,
        mapc.creature,
      ),
    ),
  }));
}

function creatureMenuActions(
  state: M.AllStates,
  scene: T.Scene,
  combat: T.Combat | null,
  creature: T.Creature,
): Grid.MapCreature["actions"] {
  const actions = [
    {
      actionName: "View Creature",
      action: (cid: T.CreatureID) =>
        M.getState().setSecondaryFocus({ t: "Creature", creature_id: cid }),
    },
    { actionName: "Walk", action: (cid: T.CreatureID) => A.requestMove(cid) },
    { actionName: "Teleport", action: (cid: T.CreatureID) => Grid.requestTeleport(scene, cid) },
  ];
  if (combat && state.getCurrentCombatCreatureID() === creature.id) {
    actions.push({
      actionName: "Combat-move",
      action: (_: T.CreatureID) => A.requestCombatMovement(),
    });
  }
  return actions;
}

function GMChat(): JSX.Element {
  return <CV.GenericChat renderLog={get_chat_line} sendChat={sendChat} />;

  function get_chat_line(log: T.GameLog) {
    if (log.t === "ChatFromPlayer" || log.t === "ChatFromGM") {
      return <CV.ChatLog log={log} />;
    }
    if (log.t === "CreatureLog") {
      return <History.CreatureLog creatureId={log.creature_id} log={log.log} />;
    }
  }

  function sendChat(message: string) {
    A.sendGMCommand({ t: "ChatFromGM", message });
  }
}
