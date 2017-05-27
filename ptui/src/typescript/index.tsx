import * as React from "react";
import * as ReactDOM from "react-dom";

import * as Hello from "./Hello";
import * as TextInput from "./TextInput";

function unloadComponent(id: string) {
  let el = document.getElementById(id);
  if (el != null) {
    ReactDOM.unmountComponentAtNode(el);
  }
}

function PT_initializeComponents(app: any) {
  app.ports.renderHello.subscribe(afterView(Hello.renderHello));
  app.ports.renderTextInput.subscribe(afterView(
    function (x: [string, string, object, boolean]) { TextInput.renderTextInput(app, x) }
  ));
  app.ports.unloadComponent.subscribe(unloadComponent);
}

function afterView(f: any) {
  /// A small wrapper that delays a function call until the next animation frame.
  /// This is critical for Elm, since it emits messages to the native javascript code before 
  /// rendering is finished, but it guarantees (I think) that views are completely calculated
  /// and updated before the next browser animation frame.
  return function (this: any) {
    let args = arguments;
    let self = this;
    window.requestAnimationFrame(function (_) {
      f.apply(self, args);
    });
  }
}

// I can't figure out any other way to export these functions such that they can be called from plain
// old javascript callers

(window as any).PT_initializeComponents = PT_initializeComponents;
