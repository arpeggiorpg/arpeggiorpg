import * as React from "react";
import * as ReactDOM from "react-dom";

import * as PTDice from "./Dice";
import * as Hello from "./Hello";
import * as History from "./History";
import * as Players from "./Players";
import * as PlayerView from "./PlayerView";
import * as PTTypes from "./PTTypes";
import * as TextInput from "./TextInput";

function unloadComponent(id: string) {
  console.log("[unloadComponent]", id);
  const el = document.getElementById(id);
  if (el !== null) {
    ReactDOM.unmountComponentAtNode(el);
  }
}

function PT_initializeComponents(app: any) {
  app.ports.renderHello.subscribe(afterView(Hello.renderHello));
  app.ports.renderTextInput.subscribe(afterView(
    (x: [string, string, object, boolean]) => TextInput.renderTextInput(app, x)
  ));

  app.ports.renderHistory.subscribe(afterView((x: [string, any]) => History.renderHistory(app, x)));

  app.ports.renderPlayers.subscribe(afterView((x: any) => Players.renderPlayers(app, x)));

  app.ports.renderPlayerUI.subscribe(afterView((x: any) => PlayerView.renderPlayerUI(app, x)))

  app.ports.unloadComponent.subscribe(unloadComponent);
  app.ports.renderReactMain.subscribe(([elemID, componentName, pt_app]: [string, string, any]) =>
    PT_renderMain(app, componentName, elemID, pt_app));
}

function afterView(f: any) {
  /// A small wrapper that delays a function call until the next animation frame.
  /// This is critical for Elm, since it emits messages to the native javascript code before
  /// rendering is finished, but it guarantees (I think) that views are completely calculated
  /// and updated before the next browser animation frame.
  return function (this: any) {
    const args = arguments;
    const self = this;
    window.requestAnimationFrame(_ => f.apply(self, args));
  }
}


function PT_renderMain(elm_app: any, component_name: string, id: string, pt_app: any) {
  const el = document.getElementById(id);
  let component;
  switch (component_name) {
    // case "GM": component = <GMMain />;
    case "Player": component = <PlayerView.PlayerMain elm_app={elm_app} app={pt_app} />; break;
    default: throw new Error(`Unknown component ${component}`);
  };
  ReactDOM.render(component, el);
}

// I can't figure out any other way to export these functions such that they can be called from plain
// old javascript callers

(window as any).PT_initializeComponents = PT_initializeComponents;

(window as any).PTT = PTTypes;
(window as any).PTDice = PTDice;
(window as any).PT_renderMain = PT_renderMain;
