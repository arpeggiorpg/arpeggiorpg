import * as React from "react";
import * as ReactDOM from "react-dom";

import * as CV from './CommonView';
import * as GMView from './GMView';
import * as PlayerView from "./PlayerView";

function getInnerComponent(component_name: string): JSX.Element {
  switch (component_name) {
    case "GM": return <GMView.GMMain />;
    case "Player": return <PlayerView.PlayerMain />;
    default: throw new Error(`Unknown component ${component_name}`);
  }
}

function PT_renderMain(rpi_url: string, component_name: string, id: string) {
  const el = document.getElementById(id);
  ReactDOM.render(
    <CV.Main rpi_url={rpi_url}>
      {getInnerComponent(component_name)}
    </CV.Main>,
    el);
}

// I can't figure out any other way to export these functions such that they can be called from plain
// old javascript callers

(window as any).PT_renderMain = PT_renderMain;
