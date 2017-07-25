import * as React from "react";
import * as ReactDOM from "react-dom";

// import * as Perf from 'react-addons-perf';

// (window as any).Perf = Perf;

import * as CommonView from './CommonView';
import * as PTDice from "./Dice";
import * as GMView from './GMView';
import * as PlayerView from "./PlayerView";
import * as PTTypes from "./PTTypes";

function PT_initializeComponents(app: any) {
  app.ports.renderReactMain.subscribe(([elemID, rpi_url, componentName, pt_app]:
    [string, string, string, any]) =>
    PT_renderMain(rpi_url, componentName, elemID, pt_app));
}

function getInnerComponent(component_name: string): JSX.Element {
  switch (component_name) {
    case "GM": return <GMView.GMMain />;
    case "Player": return <PlayerView.PlayerMain />;
    default: throw new Error(`Unknown component ${component_name}`);

  }
}

function PT_renderMain(rpi_url: string, component_name: string, id: string, pt_app: any) {
  const el = document.getElementById(id);
  ReactDOM.render(
    <CommonView.Main rpi_url={rpi_url} app={pt_app}>
      {getInnerComponent(component_name)}
    </CommonView.Main>,
    el);
}

// I can't figure out any other way to export these functions such that they can be called from plain
// old javascript callers

(window as any).PT_initializeComponents = PT_initializeComponents;

(window as any).PTT = PTTypes;
(window as any).PTDice = PTDice;
(window as any).PT_renderMain = PT_renderMain;
