import * as React from "react";
import * as ReactDOM from "react-dom";

import * as Hello from "./Hello";

function unloadComponent(id: string) {
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
  app.ports.renderHello.subscribe(afterView(Hello.renderHello));
  app.ports.unloadComponent.subscribe(afterView(unloadComponent));
}

// I can't figure out any other way to export these functions such that they can be called from plain
// old javascript callers

(window as any).PT_initializeComponents = PT_initializeComponents;
