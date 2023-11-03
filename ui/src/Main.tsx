import * as React from "react";

import { createBrowserRouter, Link, Outlet, useNavigate, useParams } from "react-router-dom";
import * as Z from "zod";

import { Modal } from "semantic-ui-react";
import useSWR from "swr";
import * as A from "./Actions";
import { ptfetch } from "./Actions";
import { ModalMaker } from "./CommonView";
import * as M from "./Model";
import * as T from "./PTTypes";
import { SignIn } from "./SignIn";
import { TextInput } from "./TextInput";
import * as WS from "./wsrpi";

const GMMain = React.lazy(() => import("./GMView").then(GMV => ({ default: GMV.GMMain })));
const GMMap = React.lazy(() => import("./GMView").then(GMV => ({ default: GMV.GMMap })));

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
          {
            path: "campaign/*",
            element: (
              <React.Suspense fallback={<div>Loading...</div>}>
                <GMMap />
              </React.Suspense>
            ),
          },
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
          <h1>
            <Link to="/">ArpeggioRPG</Link> {gameName ? <span>— {gameName}</span> : null}
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

  return (
    <div>
      <TextInput
        defaultValue="Name of your Game"
        onSubmit={createGame}
        onCancel={closer}
      />
    </div>
  );

  async function createGame(name: string) {
    let game_id = await A.createGame(name);
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

function Connector(props: { role: T.Role } & React.PropsWithChildren) {
  const { gameId } = useParams() as { gameId: string };
  const [connectionCount, setConnectionCount] = React.useState(0);

  React.useEffect(() => {
    // connect returns a cancellation function, which we return here from the effect so react will
    // call it when this component gets unmounted.
    return WS.connect(gameId, props.role);
  }, [gameId, connectionCount]);

  const status = M.useState((s) => s.socketStatus);
  const reconnect = () => setConnectionCount(connectionCount + 1);

  return (
    <>
      {status === "closed"
        ? (
          <Modal open={true}>
            <Modal.Header>Connection is inactive</Modal.Header>
            <Modal.Content>
              Connection is inactive (most likely due to idle timeout; I'm just trying to avoid
              running up a big server bill!)
            </Modal.Content>
            <Modal.Actions>
              <button onClick={reconnect}>Reconnect</button>
            </Modal.Actions>
          </Modal>
        )
        : null}
      {props.children}
    </>
  );
}

function GMGame() {
  return (
    <Connector role="GM">
      <React.Suspense fallback={<div>Loading...</div>}>
        <GMMain />
      </React.Suspense>
    </Connector>
  );
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
    (k) => ptfetch(k, {}, Z.boolean()),
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
      Z.any(),
    );
    navigate(`/`);
  }
}

const PlayerGameView = React.lazy(() =>
  import("./PlayerView").then(pv => {
    console.log("what have I got here?? pv", pv);
    console.log("and PlayerGameView?", pv.PlayerGameView);
    return { default: pv.PlayerGameView };
  })
);

function PlayerGame() {
  let {
    data: games,
    error,
    isLoading,
  } = useSWR("/g/list", (k) => ptfetch(k, {}, T.decodeGameList));
  let gameId = M.useState(s => s.gameId);
  console.log("[PlayerGame]", { isLoading, games, gameId });

  const playerId = games?.games.find(
    ([profile, _meta]) => profile.game_id === gameId && profile.role === "Player",
  )?.[0].profile_name;
  React.useEffect(() => {
    M.getState().setPlayerId(playerId);
  }, [playerId]);

  if (isLoading || !games) {
    return <div>Loading...</div>;
  }

  return (
    <Connector role="Player">
      <React.Suspense fallback={<div>Loading...</div>}>
        {playerId
          ? <PlayerGameView playerId={playerId} />
          : <div>Loading. or maybe we can't find your player. Who knows?</div>}
      </React.Suspense>
    </Connector>
  );
}
