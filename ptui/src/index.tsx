import * as React from "react";
import {createRoot} from "react-dom/client";

import { Main } from "./Main";
import * as GMView from "./GMView";
import * as PlayerView from "./PlayerView";

function getInnerComponent(component_name: string): JSX.Element {
  switch (component_name) {
    case "gm":
      return <GMView.GMMain />;
    case "player":
      return <PlayerView.PlayerMain />;
    default:
      throw new Error(`Unknown component ${component_name}`);
  }
}

let root;

function PT_renderMain(component_name: string, id: string) {
  const el = document.getElementById(id);
  const comp = (
    <Main>
      {getInnerComponent(component_name)}
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
