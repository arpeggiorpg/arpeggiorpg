import * as React from "react";

// import * as M from "./Model";
// import {startPoll, RPI_URL} from "./Actions";

import useCookie from 'react-use-cookie';

import { SignIn } from "./SignIn";


export function Main() {
  // const fetchStatus = M.useState(s => s.fetchStatus);
  // kick off a fetch of the app

  const [token, setToken] = useCookie("pt-id-token");

  if (token) {
    return <div>
      <p>You're logged in! good job!</p>

      <button onClick={() => setToken('')}>Log Off</button>
    </div>;
  } else {
    return <SignIn signedIn={setToken} />;
  }
}


  // React.useEffect(() => { startPoll(); }, []);

  // if (fetchStatus === "Unfetched") {
  //   return <div>Waiting for initial data from server.</div>;
  // } else if (fetchStatus === "Error") {
  //   return (
  //     <div>
  //       There was an error fetching the application state from {RPI_URL}.
  //       Trying again...
  //     </div>
  //   );
  // } else {
  //   return props.children;
  // }



// async function getInnerComponent(component_name: string): Promise<JSX.Element> {
//   switch (component_name) {
//     case "gm": {
//       const GMView = await import("./GMView")
//       return <GMView.GMMain />;
//     }
//     case "player": {
//       const PlayerView = await import("./PlayerView");
//       return <Main><PlayerView.PlayerMain /></Main>;
//     }
//     default:
//       return <SignIn />;
//   }
// }
