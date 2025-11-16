import useSWR from "swr";
import * as Z from "zod";
import * as A from "./Actions";
import type * as T from "./PTTypes";
import { decodeGameMetadata } from "./PTTypes";

const durableObjectSchema = Z.object({
  id: Z.string(),
  hasStoredData: Z.boolean().optional(),
});

const cloudflareApiResponseSchema = Z.object({
  result: Z.array(durableObjectSchema).optional(),
});

const namespaceSchema = Z.object({
  id: Z.string(),
  name: Z.string().optional(),
  class: Z.string().optional(),
});

const namespacesResponseSchema = Z.object({
  result: Z.array(namespaceSchema).optional(),
});

export default function Admin() {
  const { data } = useSWR(
    "/superuser/games",
    (k) =>
      A.ptfetch(
        k,
        {},
        Z.object({
          games: Z.array(Z.tuple([Z.string(), decodeGameMetadata])),
          do_namespaces: namespacesResponseSchema,
          do_objects: Z.record(Z.string(), cloudflareApiResponseSchema),
          arpeggiogame_ids: Z.record(Z.string(), Z.string()),
          arpeggiogame_legacy_ids: Z.record(Z.string(), Z.string()),
        }),
      ),
    {
      // this endpoint has gotten kinda slow so don't refetch it automatically
      revalidateIfStale: false,
      revalidateOnFocus: false,
      revalidateOnReconnect: false,
      refreshInterval: 0,
    },
  );

  if (!data) {
    return <div>Loading... or maybe there was an error</div>;
  }

  // Helper function to get DO status indicators
  const getDOStatus = (gameId: string) => {
    const hasLegacy = data.arpeggiogame_legacy_ids && gameId in data.arpeggiogame_legacy_ids;
    const hasSQL = data.arpeggiogame_ids && gameId in data.arpeggiogame_ids;

    // Find storage data for this game's DOs
    const legacyDoId = data.arpeggiogame_legacy_ids?.[gameId];
    const sqlDoId = data.arpeggiogame_ids?.[gameId];

    let legacyHasData = false;
    let sqlHasData = false;

    // Check each namespace's objects for storage data
    Object.values(data.do_objects).forEach((objectsResponse) => {
      if (objectsResponse.result) {
        objectsResponse.result.forEach((obj) => {
          if (obj.id === legacyDoId) {
            legacyHasData = obj.hasStoredData === true;
          }
          if (obj.id === sqlDoId) {
            sqlHasData = obj.hasStoredData === true;
          }
        });
      }
    });

    return { hasLegacy, hasSQL, legacyHasData, sqlHasData };
  };

  // Find orphan DOs - DOs that exist in Cloudflare but aren't mapped to any known game
  const knownLegacyDOIds = new Set(Object.values(data.arpeggiogame_legacy_ids));
  const knownSQLDOIds = new Set(Object.values(data.arpeggiogame_ids));

  type OrphanDO = {
    id: string;
    namespace: string;
    hasStoredData: boolean | undefined;
  };

  const orphanDOs: OrphanDO[] = [];

  // Create a mapping of namespace IDs to namespace names
  const namespaceIdToName = new Map<string, string>();
  if (data.do_namespaces.result) {
    data.do_namespaces.result.forEach((ns) => {
      if (ns.id && ns.name) {
        namespaceIdToName.set(ns.id, ns.name);
      }
    });
  }

  // Check each namespace's objects for orphans
  Object.entries(data.do_objects).forEach(([namespaceId, objectsResponse]) => {
    if (objectsResponse.result) {
      objectsResponse.result.forEach((obj) => {
        if (obj.id) {
          // If this DO exists but isn't mapped to any known game, it's an orphan
          if (!knownLegacyDOIds.has(obj.id) && !knownSQLDOIds.has(obj.id)) {
            // Get the actual namespace name, fall back to ID if not found
            const namespaceName = namespaceIdToName.get(namespaceId) || namespaceId;

            orphanDOs.push({
              id: obj.id,
              namespace: namespaceName,
              hasStoredData: obj.hasStoredData,
            });
          }
        }
      });
    }
  });

  return (
    <div>
      <h1>Welcome, admin!</h1>
      <h2>All games in game_metadata</h2>
      <table style={{ borderCollapse: "collapse", width: "100%" }}>
        <thead>
          <tr style={{ backgroundColor: "#f5f5f5" }}>
            <th style={{ border: "1px solid #ddd", padding: "8px", textAlign: "left" }}>Game ID</th>
            <th style={{ border: "1px solid #ddd", padding: "8px", textAlign: "left" }}>Name</th>
            <th style={{ border: "1px solid #ddd", padding: "8px", textAlign: "center" }}>
              Legacy DO
            </th>
            <th style={{ border: "1px solid #ddd", padding: "8px", textAlign: "center" }}>
              SQL DO
            </th>
            <th style={{ border: "1px solid #ddd", padding: "8px", textAlign: "center" }}>
              Actions
            </th>
          </tr>
        </thead>
        <tbody>
          {data.games.map(([id, meta]) => {
            const { hasLegacy, hasSQL, legacyHasData, sqlHasData } = getDOStatus(id);
            return (
              <tr key={id}>
                <td
                  style={{
                    border: "1px solid #ddd",
                    padding: "8px",
                    fontFamily: "monospace",
                    fontSize: "12px",
                  }}
                >
                  {id}
                </td>
                <td style={{ border: "1px solid #ddd", padding: "8px" }}>{meta.name}</td>
                <td
                  style={{
                    border: "1px solid #ddd",
                    padding: "8px",
                    textAlign: "center",
                    color: hasLegacy ? "green" : "red",
                  }}
                >
                  {hasLegacy ? "✓" : "✗"} {hasLegacy && (legacyHasData ? "💾" : "☐")}
                </td>
                <td
                  style={{
                    border: "1px solid #ddd",
                    padding: "8px",
                    textAlign: "center",
                    color: hasSQL ? "green" : "red",
                  }}
                >
                  {hasSQL ? "✓" : "✗"} {hasSQL && (sqlHasData ? "💾" : "☐")}
                </td>
                <td style={{ border: "1px solid #ddd", padding: "8px", textAlign: "center" }}>
                  <button
                    type="button"
                    onClick={() => logGame(meta.name, id)}
                    style={{ marginRight: "4px" }}
                  >
                    dump
                  </button>
                  <button type="button" onClick={() => destroyGame(id)} style={{ color: "red" }}>
                    DESTROY
                  </button>
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>

      {orphanDOs.length > 0 && (
        <div>
          <h2>Orphan Durable Objects</h2>
          <p style={{ fontSize: "14px", color: "#666", marginBottom: "16px" }}>
            These DOs exist in Cloudflare but are not mapped to any game in our database:
          </p>
          <table style={{ borderCollapse: "collapse", width: "100%" }}>
            <thead>
              <tr style={{ backgroundColor: "#f5f5f5" }}>
                <th style={{ border: "1px solid #ddd", padding: "8px", textAlign: "left" }}>
                  DO ID
                </th>
                <th style={{ border: "1px solid #ddd", padding: "8px", textAlign: "left" }}>
                  Namespace
                </th>
                <th style={{ border: "1px solid #ddd", padding: "8px", textAlign: "center" }}>
                  Has Data
                </th>
              </tr>
            </thead>
            <tbody>
              {orphanDOs.map((orphan) => (
                <tr key={orphan.id}>
                  <td
                    style={{
                      border: "1px solid #ddd",
                      padding: "8px",
                      fontFamily: "monospace",
                      fontSize: "12px",
                      color: "red",
                    }}
                  >
                    {orphan.id}
                  </td>
                  <td
                    style={{
                      border: "1px solid #ddd",
                      padding: "8px",
                      fontFamily: "monospace",
                      fontSize: "12px",
                    }}
                  >
                    {orphan.namespace}
                  </td>
                  <td style={{ border: "1px solid #ddd", padding: "8px", textAlign: "center" }}>
                    {orphan.hasStoredData === undefined ? "?" : orphan.hasStoredData ? "💾" : "☐"}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

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
