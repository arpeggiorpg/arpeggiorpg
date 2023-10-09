import * as React from "react";

import {
  createHashRouter,
  Link,
  useParams,
  useNavigate,
  Outlet,
} from "react-router-dom";

import * as M from "./Model";
import * as A from "./Actions";

import { SignIn } from "./SignIn";
import { ptfetch } from "./Actions";
import * as T from "./PTTypes";
import { GMMain, GMMap } from "./GMView";
import useSWR from "swr";
import { ModalMaker } from "./CommonView";
import { TextInput } from "./TextInput";

// I am using a hash router until I spend the time to figure out routing for the web server; from
// what I've read it probably needs to be implemented with CloudFlare route rules or something.
export const router = createHashRouter([
  {
    path: "/",
    element: <Main />,
    children: [
      { index: true, element: <GameList /> },
      {
        path: "gm/:gameId",
        element: <GMGame />,
        children: [
          { index: true, element: <div>Pick a scene!</div> },
          { path: "campaign/*", element: <GMMap />}
        ],
      },
    ],
  },
]);

export function Main() {
  const token = M.useState((s) => s.userToken);
  const gameName = M.useState((s) => s.gameName);

  if (token && token.length > 1) {
    return (
      <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
        <div style={{ display: "flex", justifyContent: "space-between" }}>
          <h1><Link to="/">ArpeggioRPG</Link> {gameName ? <span>— {gameName}</span> : null}</h1>
          <div className="rightNavThing">
            <Link to="/">Game List</Link>
            <button onClick={() => M.getState().setUserToken(undefined)}>
              Log Off
            </button>
          </div>
        </div>
        <ErrorBoundary fallback={<div>ERROR OCCURRED</div>}>
          <React.Suspense>
            <Outlet />
          </React.Suspense>
        </ErrorBoundary>
      </div>
    );
  } else {
    return <SignIn />;
  }
}

function GameList() {
  React.useEffect(() => {
    console.log("GameList MOUNT");
    return () => console.log("GameList UNMOUNT");
  }, []);

  let {
    data: games,
    error,
    isLoading,
  } = useSWR("g/list", () => ptfetch("/g/list", {}, T.decodeGameList), {
    suspense: true,
  });

  let gm_games = games.games.filter(g => g[0].role === "GM");
  let player_games = games.games.filter(g => g[0].role === "Player");


  return (
    <>
      <h1>You are GM of these games</h1>
      <ul>
        {gm_games.map(([{game_id, profile_name}, meta]) => (
          <li key={game_id}>
            <Link to={`gm/${game_id}`}>{meta.name} (as {profile_name})</Link>
          </li>
        ))}
        <li>
          <ModalMaker
            button={(clicker) => <button onClick={clicker}>Create New</button>}
            header={<>Create Game</>}
            content={(closer) => <CreateGame closer={closer} />}
          />
        </li>
      </ul>
      <h1>You are a player in these games</h1>
      <ul>
        {player_games.map(([{game_id, profile_name}, meta]) => (
          <li key={game_id}>
            <Link to={`player/${game_id}`}>{meta.name} (as {profile_name})</Link>
          </li>
        ))}
      </ul>
    </>
  );
}

function CreateGame({ closer }: { closer: () => void }) {
  const navigate = useNavigate();

  async function createGame(name: string) {
    let game_id = await A.createGame(name);
    navigate(`/gm/${game_id}`);
  }

  return (
    <div>
      <TextInput
        defaultValue="Name of your Game"
        onSubmit={createGame}
        onCancel={closer}
      />
    </div>
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

  componentDidCatch(error: Error, info: React.ErrorInfo) {
    console.error("[ErrorBoundary error]", error);
    console.log("Additional Stack:", info.componentStack);
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
  const { gameId } = useParams() as { gameId: string };

  React.useEffect(() => {
    // startPoll returns a cancellation function, which we return here from the effect so react will
    // call it when this component gets unmounted.
    return A.startPoll("gm", gameId);
  }, []);

  let game = M.useState((s) => s.game);
  let status = M.useState(s => s.fetchStatus);

  console.log("game!!!!", game);
  if (status === "Ready") {
    return <GMMain />;
  } else  if (status === "Unfetched") {
    return <div>Loading game...</div>;
  } else if (status === "Error") {
    return <div>Error loading game!</div>;
  }
}
