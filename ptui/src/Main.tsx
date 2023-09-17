import * as React from "react";

import * as M from "./Model";

export function Main(props: React.PropsWithChildren) {
  const fetchStatus = M.useState(s => s.fetchStatus);
  // kick off a fetch of the app
  React.useEffect(() => {
    M.startPoll();
  });

  if (fetchStatus === "Unfetched") {
    return <div>Waiting for initial data from server.</div>;
  } else if (fetchStatus === "Error") {
    return (
      <div>
        There was an error fetching the application state from {M.RPI_URL}.
        Trying again...
      </div>
    );
  } else {
    return props.children;
  }
}
