import * as React from "react";

import {
  createBrowserRouter,
  Link,
  useParams,
  useNavigate,
  Outlet,
} from "react-router-dom";
import * as Z from "zod";

import * as M from "./Model";
import * as A from "./Actions";
import { SignIn } from "./SignIn";
import { ptfetch } from "./Actions";
import * as T from "./PTTypes";
import { GMMain, GMMap } from "./GMView";
import useSWR from "swr";
import { ModalMaker } from "./CommonView";
import { TextInput } from "./TextInput";
import { PlayerGameView } from "./PlayerView";

export const router = createBrowserRouter([
  {
    path: "/",
    element: <Main />,
    children: [
      { index: true, element: <GameList /> },
      {
        path: "/invitations/:gameId/:invitationId",
        element: <AcceptInvitation />,
      },
      { path: "player/:gameId", element: <PlayerGame /> },
      {
        path: "gm/:gameId",
        element: <GMGame />,
        children: [
          { index: true, element: <div>Pick a scene!</div> },
          { path: "campaign/*", element: <GMMap /> },
        ],
      },
    ],
  },
]);


const ws = new WebSocket(import.meta.env.VITE_WEBSOCKET_URL);

ws.addEventListener("open", (event) => {
  ws.send("Hello Server!");
});

// Listen for messages
ws.addEventListener("message", (event) => {
  console.log("Message from server ", event.data);
});


if (window) (window as any).ARPEGGIO_WS = ws;

export function Main() {
  const token = M.useState((s) => s.userToken);
  const gameName = M.useState((s) => s.gameName);

  if (token && token.length > 1) {
    return (
      <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
        <div style={{ display: "flex", justifyContent: "space-between" }}>
          <h1>
            <Link to="/">ArpeggioRPG</Link>{" "}
            {gameName ? <span>— {gameName}</span> : null}
          </h1>
          <div className="rightNavThing">
            <Link to="/">Game List</Link>
            <button onClick={() => M.getState().setUserToken(undefined)}>
              Log Off
            </button>
          </div>
        </div>
        <ErrorBoundary fallback={<div>ERROR OCCURRED</div>}>
          <Outlet />
        </ErrorBoundary>
      </div>
    );
  } else {
    return <SignIn />;
  }
}

function GameList() {
  let {
    data: games,
    error,
    isLoading,
  } = useSWR("/g/list", (k) => ptfetch(k, {}, T.decodeGameList));

  if (isLoading || !games) {
    return <div>Loading games...</div>;
  }

  let gm_games = games.games.filter((g) => g[0].role === "GM");
  let player_games = games.games.filter((g) => g[0].role === "Player");

  return (
    <>
      <h1>You are GM of these games</h1>
      <ul>
        {gm_games.map(([{ game_id, profile_name }, meta]) => (
          <li key={game_id}>
            <Link to={`gm/${game_id}`}>
              {meta.name} (as {profile_name})
            </Link>
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
        {player_games.map(([{ game_id, profile_name }, meta]) => (
          <li key={game_id}>
            <Link to={`player/${game_id}`}>
              {meta.name} (as {profile_name})
            </Link>
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


function usePoll(mode: "gm" | "player") {
  const { gameId } = useParams() as { gameId: string };

  React.useEffect(() => {
    // startPoll returns a cancellation function, which we return here from the effect so react will
    // call it when this component gets unmounted.
    return A.startPoll(mode, gameId);
  }, []);

  const status = M.useState((s) => s.fetchStatus);
  return {gameId, status};

}

function GMGame() {
  const {status} = usePoll("gm");

  if (status === "Ready") {
    return <GMMain />;
  } else if (status === "Unfetched") {
    return <div>Loading game...</div>;
  } else if (status === "Error") {
    return <div>Error loading game!</div>;
  }
}

function AcceptInvitation() {
  const { gameId, invitationId } = useParams() as {
    gameId: string;
    invitationId: string;
  };
  const navigate = useNavigate();

  const [profileName, setProfileName] = React.useState<string>("");

  const { data: invitationExists, isLoading } = useSWR(
    `/g/invitations/${gameId}/${invitationId}`,
    (k) => ptfetch(k, {}, Z.boolean())
  );
  if (isLoading) {
    return <div>Checking invitation...</div>;
  }

  if (invitationExists) {
    return (
      <div>
        <p>Invitation checks out!</p>
        <p>Wanna join? Enter a name!</p>
        <input
          type="text"
          value={profileName}
          onChange={(e) => setProfileName(e.target.value)}
        />
        <button onClick={accept}>Join as a player!</button>
      </div>
    );
  } else {
    return <div>Sorry, that invitation doesn't seem to exist.</div>;
  }

  async function accept() {
    await ptfetch(
      `/g/invitations/${gameId}/${invitationId}/accept`,
      {
        method: "POST",
        body: JSON.stringify(profileName),
        headers: { "content-type": "application/json" },
      },
      Z.any()
    );
    navigate(`/`);
  }
}


function PlayerGame() {
  const {gameId, status} = usePoll("player");

  let {
    data: games,
    error,
    isLoading,
  } = useSWR("/g/list", (k) => ptfetch(k, {}, T.decodeGameList));
  console.log(isLoading, games, status);

  const playerId = games?.games.find(([profile, _meta]) => profile.game_id === gameId && profile.role === "Player")?.[0].profile_name;
  React.useEffect(() => {
    M.getState().setPlayerId(playerId);
  }, [playerId])
  if (isLoading || !games || status !== "Ready") {
    return <div>Loading...</div>;
  }

  if (!playerId) {
    return <div>Sorry, couldn't find a player for you</div>;
  }
  return <PlayerGameView playerId={playerId} />;
}
