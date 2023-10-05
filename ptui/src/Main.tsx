import * as React from "react";

import { createHashRouter, RouterProvider, Link, useLoaderData, useParams, redirect, useNavigate } from "react-router-dom";

// import * as M from "./Model";
import * as A from "./Actions";
// import {startPoll, RPI_URL} from "./Actions";

import useCookie from "react-use-cookie";

import { SignIn } from "./SignIn";
import { ptfetch } from "./Actions";
import * as T from "./PTTypes";
import { useState } from "./Model";
import { GMMain } from "./GMView";

export const router = createHashRouter([
  {
    path: "/",
    loader: async (): Promise<T.GameList> => {
      return await ptfetch("/g/list", {}, T.decodeGameList);
    },
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

function GameList() {
  let games = useLoaderData() as T.GameList;

  let navigate = useNavigate();
  return (
    <>
      <h1>You are GM of these games</h1>
      <ul>
        {games?.gm_games.map(([gameId, name]) => (
          <li key={gameId}>
            <Link to={`gm/${gameId}`}>{name}</Link>
          </li>
        ))}
        <li><button onClick={createGame}>Create New</button></li>
      </ul>
      <h1>You are a player in these games</h1>
      <ul>
        {games?.player_games.map(([gameId, name]) => (
          <li key={gameId}>
            <Link to={`gm/${gameId}`}>{name}</Link>
          </li>
        ))}
      </ul>
    </>
  );

  async function createGame() {
    let game_id = await A.createGame();
    navigate(`/gm/${game_id}`);
  }
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
  return <GMMain />;
}
