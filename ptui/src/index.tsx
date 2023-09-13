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

const RPI_URL = import.meta.env.VITE_RPI_URL;

PT_renderMain(RPI_URL, 'GM', 'react-main');
