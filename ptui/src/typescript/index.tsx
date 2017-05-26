import * as React from "react";
import * as ReactDOM from "react-dom";

import { Hello } from "./Hello";

function renderHello(id: string) {
  console.log("[renderHello]", id);
  ReactDOM.render(
    <Hello compiler="TypeScript" framework="React" />,
    document.getElementById(id)
  );
}

function unloadHello(id: string) {
  let el =  document.getElementById(id);
  if (el !== null) {
    ReactDOM.unmountComponentAtNode(el);
  }
}

function afterView(f: any) {
  return function(this: any) {
    let args = arguments;
    let self = this;
    window.requestAnimationFrame(function(_) {
        f.apply(self, args);
    });
  }
}

function PT_initializeComponents(app: any) {
  app.ports.renderHello.subscribe(afterView(renderHello));
  app.ports.unloadHello.subscribe(afterView(unloadHello));
}

// I can't figure out any other way to export these functions such that they can be called from plain
// old javascript callers

(window as any).PT_initializeComponents = PT_initializeComponents;
