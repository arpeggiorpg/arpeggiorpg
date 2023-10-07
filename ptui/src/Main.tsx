import * as React from "react";

import {
  createHashRouter,
  RouterProvider,
  Link,
  useLoaderData,
  useParams,
  redirect,
  useNavigate,
  Outlet,
} from "react-router-dom";

import * as M from "./Model";
import * as A from "./Actions";
// import {startPoll, RPI_URL} from "./Actions";

import { SignIn } from "./SignIn";
import { ptfetch } from "./Actions";
import * as T from "./PTTypes";
import { GMMain } from "./GMView";
import useSWR from "swr";
import { ModalMaker } from "./CommonView";
import { TextInput } from "./TextInput";

import jwt_decode, { JwtPayload } from "jwt-decode";

export const router = createHashRouter([
  {
    path: "/",
    element: <Main />,
    children: [
      { index: true, element: <GameList />},
      {
        path: "/gm/:gameId",
        element: <GMGame />,
      },
    ],
  },
]);

export function Main() {
  const token = M.useState(s => s.userToken);

  if (token && token.length > 1) {
    return (
      <div>
        <p>You're logged in! good job!</p>

        <ErrorBoundary fallback={<div>ERROR OCCURRED</div>}>
          <Outlet />
        </ErrorBoundary>

        <button onClick={() => M.getState().setUserToken(undefined)}>Log Off</button>
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
  } = useSWR("g/list", () => ptfetch("/g/list", {}, T.decodeGameList), {
    suspense: true,
  });

  return (
    <>
      <h1>You are GM of these games</h1>
      <ul>
        {games?.gm_games.map(([gameId, meta]) => (
          <li key={gameId}>
            <Link to={`gm/${gameId}`}>{meta.name}</Link>
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
        {games?.player_games.map(([gameId, meta]) => (
          <li key={gameId}>
            <Link to={`gm/${gameId}`}>{meta.name}</Link>
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
  let { gameId } = useParams() as { gameId: string };

  React.useEffect(() => {
    A.startPoll("gm", gameId);
  }, []);

  let game = M.useState(s => s.game);

  console.log("game", game);
  return <GMMain />;
}
