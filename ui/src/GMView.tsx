import { Map } from "immutable";
import mapValues from "lodash/mapValues";
import * as React from "react";

import { Outlet, useParams } from "react-router-dom";
import * as A from "./Actions";
import * as Campaign from "./Campaign";
import * as CV from "./CommonView";
import * as GM from "./GMComponents";
import * as Grid from "./Grid";
import * as History from "./History";
import * as M from "./Model";
import * as Players from "./Players";
import * as T from "./PTTypes";

export function GMMain() {
  const scene = M.useState((s) => s.getFocusedScene());

  const tabs = [
    <CV.Tab key="Campaign" name="Campaign">
      <Campaign.Campaign />
    </CV.Tab>,
    <CV.Tab key="Combat" name="Combat">
      <GM.GMCombat />
    </CV.Tab>,
    <CV.Tab key="Players" name="Players">
      <div style={{ display: "flex", flexDirection: "column" }}>
        <Players.Players />
        <Players.Invitations />
      </div>
    </CV.Tab>,
    <CV.Tab key="History" name="History"><History.History /></CV.Tab>,
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
      top_left={scene ? <GM.GMScene scene={scene} /> : <div>Select a scene</div>}
      bottom_right={<GMChat />}
      bar_width={450}
      menu_size="tiny"
      bottom_bar={bottom_bar}
    />
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
          afterSave={(path, note) =>
            M.getState().setSecondaryFocus({ t: "Note", path, name: note.name })}
        />
      );
    case "Creature":
      return <GM.CreatureFocus creatureId={focus2.creature_id} />;
    case "Item":
      return <GM.GMViewItem itemId={focus2.item_id} />;
  }
}

export function GMMap() {
  const { "*": path } = useParams();
  const { scene, creaturesInMap } = M.useState((s) => {
    if (!path) return { scene: undefined, creaturesInMap: undefined };
    const scene = getSceneFromPath(s, path);
    if (!scene) return { scene: undefined, creaturesInMap: undefined };
    const creaturesInMap = mapCreatures(scene, s);
    return { scene, creaturesInMap };
  });
  React.useEffect(() => {
    // We need to synchronize the scene from the path to the zustand store
    console.log("setting grid focus to", scene?.id);
    M.getState().setGridFocus(scene?.id);
  }, [path, scene]);
  if (!scene) {
    return <div>Couldn't find scene {path}</div>;
  }
  return <Grid.SceneGrid scene={scene} creatures={creaturesInMap} />;
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
  console.log("[getSceneFromPath]", path);
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



export function GMChat(): JSX.Element {
  return <CV.GenericChat renderLog={get_chat_line} sendChat={sendChat} />;

  function get_chat_line(log: T.GameLog) {
    if (typeof log !== "string") {
      if ("ChatFromPlayer" in log || "ChatFromGM" in log) {
        return <CV.ChatLog log={log} />
      }
      if ("CreatureLog" in log) {
        return <History.CreatureLog creatureId={log.CreatureLog[0]} log={log.CreatureLog[1]} />;
      }
    }
  }

  function sendChat(line: string) {
    A.sendGMCommand({ChatFromGM: line});
  }
}
