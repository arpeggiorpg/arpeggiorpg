import useSWR from "swr";
import * as Z from "zod";

import * as A from "./Actions";
import { decodeGameMetadata } from "./PTTypes";

export default function Admin() {
  let { data } = useSWR(
    "/superuser/games",
    (k) =>
      A.ptfetch(k, {}, Z.object({ "games": Z.array(Z.tuple([Z.string(), decodeGameMetadata])) })),
  );
  if (!data) {
    return <div>Loading... or maybe there was an error</div>;
  }
  return (
    <div>
      Welcome, admin!
      <ul>
        {data.games.map(([id, meta]) => <li>{id} - {meta.name}</li>)}
      </ul>
    </div>
  );
}
