import * as React from "react";

import { createHashRouter, RouterProvider, Link, useLoaderData, useParams } from "react-router-dom";

// import * as M from "./Model";
import * as A from "./Actions";
// import {startPoll, RPI_URL} from "./Actions";

import useCookie from "react-use-cookie";

import { SignIn } from "./SignIn";
import { ptfetch } from "./Actions";
import * as T from "./PTTypes";
import { Campaign } from "./Campaign";
import { useState } from "./Model";

export const router = createHashRouter([
  {
    path: "/",
    element: <Main />,
  },
  {
    path: "/gm/:gameId",
    element: <GMGame />
  }
]);

export function Main() {
  // const fetchStatus = M.useState(s => s.fetchStatus);
  // kick off a fetch of the app

  const [token, setToken] = useCookie("pt-id-token");

  if (token) {
    return (
      <div>
        <p>You're logged in! good job!</p>

        <ErrorBoundary fallback={<div>ERROR OCCURRED</div>}>
          <GameList />
        </ErrorBoundary>

        <button onClick={() => setToken("")}>Log Off</button>
      </div>
    );
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

function GameList() {
  let [games, setGames] = React.useState<T.UserGames | null>(null);
  React.useEffect(() => {
    async function getGames() {
      setGames(await ptfetch("/g/list", {}, T.decodeUserGames));
    }
    getGames();
  }, []);
  return (
    <>
      <h1>You are GM of these games</h1>
      <ul>
        {games?.gm_games.map((game) => (
          <li>
            <Link to={`gm/${game}`}>{game}</Link>
          </li>
        ))}
      </ul>
      <h1>You are a player in these games</h1>
      <ul>
        {games?.player_games.map((game) => (
          <li>
            <Link to={`gm/${game}`}>{game}</Link>
          </li>
        ))}
      </ul>
    </>
  );
}

type ErrorBoundaryProps = {
  fallback: React.ReactElement;
} & React.PropsWithChildren;

class ErrorBoundary extends React.Component<
  ErrorBoundaryProps,
  { hasError: boolean }
> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError() {
    // Update state so the next render will show the fallback UI.
    return { hasError: true };
  }

  componentDidCatch(error: any) {
    console.error(error);
  }

  render() {
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return this.props.fallback;
    }

    return this.props.children;
  }
}


function GMGame() {
  let { gameId } = useParams() as {gameId: string};

  React.useEffect(() => {
    A.startPoll("gm", gameId);
  }, []);

  let game = useState(s => s.game);

  console.log("game", game);
  return <Campaign />;
}
