import { Link, useNavigate } from "react-router-dom";
import useSWR from "swr";

import * as A from "./Actions";
import { ModalMaker } from "./CommonView";
import * as T from "./PTTypes";
import { TextInput } from "./TextInput";

export default function GameList() {
  let {
    data: games,
    error,
    isLoading,
  } = useSWR("/g/list", (k) => A.ptfetch(k, {}, T.decodeGameList));

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
