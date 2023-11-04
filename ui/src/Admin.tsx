import useSWR from "swr";
import * as Z from "zod";

import React from "react";
import * as A from "./Actions";
import * as T from "./PTTypes";
import { decodeGameMetadata } from "./PTTypes";

export default function Admin() {
  let { data } = useSWR(
    "/superuser/games",
    (k) =>
      A.ptfetch(k, {}, Z.object({ "games": Z.array(Z.tuple([Z.string(), decodeGameMetadata])) })),
  );
  const [gameDumpId, setGameDumpId] = React.useState<T.GameID | null>(null);

  if (!data) {
    return <div>Loading... or maybe there was an error</div>;
  }
  return (
    <div>
      <h1>Welcome, admin!</h1>
      <h2>All games in game_metadata</h2>
      <ul>
        {data.games.map(([id, meta]) => (
          <li key={id}>
            {id} - {meta.name}
            (<button onClick={() => setGameDumpId(id)}>dump</button>)
          </li>
        ))}
      </ul>

      {gameDumpId ? <Dump id={gameDumpId} /> : null}
    </div>
  );
}

function Dump({ id }: { id: T.GameID }) {
  let { data } = useSWR(
    `/superuser/dump/${id}`,
    key => A.ptfetch(key, {}, Z.record(Z.string(), Z.any())),
    { revalidateOnFocus: false, revalidateIfStale: false, revalidateOnReconnect: false },
  );
  if (!data) return <div>No data...</div>;
  return (
    <div>
      <h2>Game {id}</h2>
      <div>
        <pre style={{whiteSpace: "pre-wrap"}}>
          {JSON.stringify(data, Object.keys(data).sort(), 2)}
        </pre>
      </div>
    </div>
  );
}
