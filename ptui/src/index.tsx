import * as React from "react";
import {createRoot} from "react-dom/client";

import { Main } from "./Main";

async function getInnerComponent(component_name: string): Promise<JSX.Element> {
  switch (component_name) {
    case "gm": {
      const GMView = await import("./GMView")
      return <GMView.GMMain />;
    }
    case "player": {
      const PlayerView = await import("./PlayerView");
      return <PlayerView.PlayerMain />;
    }
    default:
      throw new Error(`Unknown component ${component_name}`);
  }
}

let root;

async function PT_renderMain(component_name: string, id: string) {
  const el = document.getElementById(id);
  const component = await getInnerComponent(component_name);
  const comp = (
    <Main>
      {component}
    </Main>
  );
  if (!el) {
    console.error("where's the root!");
    return;
  }
  root = createRoot(el);
  root.render(comp);
}

const mode = new URLSearchParams(window.location.search).get("mode") || "player";
PT_renderMain(mode.toLowerCase(), "react-main");
