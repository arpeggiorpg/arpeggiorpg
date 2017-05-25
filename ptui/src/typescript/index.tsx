import * as React from "react";
import * as ReactDOM from "react-dom";

import { Hello } from "./Hello";

export function renderHello(id: string) {
  console.log("[renderHello]", id);
  ReactDOM.render(
    <Hello compiler="TypeScript" framework="React" />,
    document.getElementById(id)
  );
}

export function unloadHello(id: string) {
  let el =  document.getElementById(id);
  if (el !== null) {
    ReactDOM.unmountComponentAtNode(el);
  }
}

// I can't figure out any other way to export these functions such that they can be called from plain
// old javascript callers
(window as any).components =
  { renderHello: renderHello, unloadHello: unloadHello};
