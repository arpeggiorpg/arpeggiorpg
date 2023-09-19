import * as React from "react";

import * as M from "./Model";
import {startPoll, RPI_URL, sayHello} from "./Actions";

export function Main(props: React.PropsWithChildren) {
  const fetchStatus = M.useState(s => s.fetchStatus);
  // kick off a fetch of the app
  React.useEffect(() => { startPoll(); }, []);
  React.useEffect(() => { sayHello(); }, []);

  if (fetchStatus === "Unfetched") {
    return <div>Waiting for initial data from server.</div>;
  } else if (fetchStatus === "Error") {
    return (
      <div>
        There was an error fetching the application state from {RPI_URL}.
        Trying again...
      </div>
    );
  } else {
    return props.children;
  }
}
