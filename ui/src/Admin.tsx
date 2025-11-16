import React from "react";
import useSWR from "swr";
import * as Z from "zod";
import * as A from "./Actions";
import type * as T from "./PTTypes";
import { decodeGameMetadata } from "./PTTypes";

export default function Admin() {
  const { data } = useSWR("/superuser/games", (k) =>
    A.ptfetch(k, {}, Z.object({ games: Z.array(Z.tuple([Z.string(), decodeGameMetadata])) })),
  );

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
            {id} - {meta.name}(
            <button type="button" onClick={() => logGame(meta.name, id)}>
              dump
            </button>
            <button type="button" onClick={() => destroyGame(id)}>DESTROY</button>
            )
          </li>
        ))}
      </ul>

      <span>Just look at your console for the dump.</span>
    </div>
  );
}

async function logGame(name: string, id: T.GameID) {
  const data = await A.ptfetch(`/superuser/dump/${id}`, {}, Z.record(Z.string(), Z.any()));
  console.log("Dump", name, { data });
}
async function destroyGame(id: T.GameID) {
  const result = await A.ptfetch(`/superuser/destroy/${id}`, {}, Z.record(Z.string(), Z.any()));
  console.log("DESTROYED GAME:", result);
}
