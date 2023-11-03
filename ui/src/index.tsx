import * as React from "react";
import { createRoot, Root } from "react-dom/client";

import { router } from "./Main";

import { RouterProvider } from "react-router-dom";
import { RPI_URL } from "./Actions";

let root: Root | undefined;

async function renderMain(id: string) {
  const el = document.getElementById(id);
  if (!el) {
    console.error("where's the root!");
    return;
  }
  if (!root) {
    root = createRoot(el);
  }
  if (!RPI_URL) {
    return <h1 style={{ color: "red" }}>DEPLOYMENT FAILED: VITE_RPI_URL is not set</h1>;
  }
  root.render(<RouterProvider router={router} />);
}

renderMain("react-main");
