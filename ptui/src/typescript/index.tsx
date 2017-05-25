import * as React from "react";
import * as ReactDOM from "react-dom";

import { Hello } from "./Hello";


function kickit() {
    ReactDOM.render(
        <Hello compiler="TypeScript" framework="React" />,
        document.getElementById("example")
    );
}
