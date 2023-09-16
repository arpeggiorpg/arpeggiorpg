import * as React from "react";
import {createRoot} from "react-dom/client";

import { Main } from "./Main";
// import * as CV from "./CommonView";
import * as GMView from "./GMView";
// import * as PlayerView from "./PlayerView";

function getInnerComponent(component_name: string): JSX.Element {
  switch (component_name) {
    case "GM":
      return <GMView.GMMain />;
    case "Player":
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

PT_renderMain("GM", "react-main");
