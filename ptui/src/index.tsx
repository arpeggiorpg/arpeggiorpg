import * as React from "react";
import {createRoot} from "react-dom/client";

import { Main } from "./Main";
import { SignIn } from "./SignIn";

async function getInnerComponent(component_name: string): Promise<JSX.Element> {
  switch (component_name) {
    case "gm": {
      const GMView = await import("./GMView")
      return <Main><GMView.GMMain /></Main>;
    }
    case "player": {
      const PlayerView = await import("./PlayerView");
      return <Main><PlayerView.PlayerMain /></Main>;
    }
    default:
      return <SignIn />;
  }
}

let root;

async function PT_renderMain(component_name: string, id: string) {
  const el = document.getElementById(id);
  const component = await getInnerComponent(component_name);
  if (!el) {
    console.error("where's the root!");
    return;
  }
  root = createRoot(el);
  root.render(component);
}

const mode = new URLSearchParams(window.location.search).get("mode") || "unset";
PT_renderMain(mode.toLowerCase(), "react-main");
