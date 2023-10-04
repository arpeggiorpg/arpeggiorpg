import * as React from "react";
import { createRoot } from "react-dom/client";

import { Main } from "./Main";

import { RPI_URL } from "./Actions";
let root;

async function PT_renderMain(id: string) {
  const el = document.getElementById(id);
  if (!el) {
    console.error("where's the root!");
    return;
  }
  root = createRoot(el);
  if (!RPI_URL) {
    return <h1 style={{color: "red"}}>DEPLOYMENT FAILED: VITE_RPI_URL is not set</h1>
  }
  root.render(<Main />);
}

PT_renderMain("react-main");
