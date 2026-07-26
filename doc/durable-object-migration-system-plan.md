# Durable Object Migration System Plan

## Status

In progress. The Phase 1 storage baseline, migration registry, load gate, retired KV runtime
removal, and declarative production Durable Object export are implemented.

## Core decision

Storage migrations are independent of deployment policy. Every Worker contains an ordered migration
chain that converts versioned local storage to the format required by that code.

Migrations run before `GameStorage::load` and before game logic can observe storage. The same chain
is used when opening an existing Durable Object and when restoring a dump into a new one.

Deployment chooses between:

- **normal deployment**: existing Durable Objects migrate lazily on their next request;
- **generation deployment**: games are copied into Durable Objects owned by a new Worker generation,
  verified, and routed there.

Cloudflare Worker versions and the Git commit identify deployed code; Arpeggio does not add a
separate release entity.

All production games already use the current SQLite schema. That schema is the new migration
baseline; historical KV-to-SQL and existing version migrations are not carried forward.

## Invariants

- A migration and its version update commit atomically.
- Migrations are ordered, idempotent, local to the Durable Object, and perform no external I/O.
- Failed migration prevents game loading and leaves the previous storage version intact.
- The complete migration chain from the new baseline remains available so games may skip releases.
- A dump records its format, all application-owned SQL schema and data, application KV entries,
  and checksum. Its storage version is contained in the dumped storage metadata.
- Dump export is a consistent storage snapshot.
- Restoring a dump is allowed only into an empty Durable Object.
- Restore is atomic, reconstructs storage at the dump version, verifies it, then invokes the
  normal migration chain.
- Migration data moves directly from the source Durable Object to the target Durable Object; the
  coordinator carries only control data.
- Production game logic only observes current-version storage.

## Phase 1: Core migrations and prereleases

### Establish the baseline

Delete the old KV-backed Durable Object class, binding, and KV-to-SQL migration. The provisioned
namespace remains unbound; deleting its stored data is a separate, explicitly reviewed Cloudflare
lifecycle operation.

On first open under the new system:

- an empty Durable Object initializes the current schema and SQL metadata table;
- an existing SQLite Durable Object with `DURABLEGAME_VERSION == 1` records SQL baseline version 1
  and removes that obsolete KV key atomically;
- other unversioned, nonempty storage fails closed.

### Migration API

Add a small migration registry and one entry point:

```rust
migrate_storage_to_current(storage) -> Result<StorageVersion>
```

Initialization must ensure that only one migration/load operation runs and that requests and
WebSocket events are not processed until it completes.

Add tests for:

- each migration from its immediate predecessor;
- migration across multiple skipped versions;
- retry after failure;
- atomic rollback;
- equivalence between opening existing storage and restoring the same storage from a dump;
- dump round trips for `NULL`, integer, real, text, BLOB, quoted identifiers, and application KV.

### Dumps and target-pull restore

Use the reusable crate's existing dump:

```rust
struct Dump {
    dump_format: u32,
    sql: Vec<String>,
    kv: Vec<KvEntry>,
    checksum: String,
}
```

Generate the ordered SQL statements by introspecting `sqlite_schema` and reading rows through the
Durable Object SQL API. Include application tables and preserve SQLite's `NULL`, integer, real,
text, and BLOB values. Create tables first, insert rows, then create indexes, triggers, and views.
Quote identifiers and values correctly.

Keep dump encoding, checksums, export, and atomic restore in the reusable `worker-sqlite-dump`
crate. Authentication, transport, migration policy, and domain validation remain in `worker`.
Reuse the full-state debug dump endpoint as the source export operation. Do not duplicate storage
version or game revision in the dump envelope: migration metadata is part of the dumped SQL or KV,
and revision fencing belongs to Phase 2.

Exclude Cloudflare and SQLite internal objects. Export application KV separately because
Cloudflare's hidden `__cf_kv` table is not readable through SQL. Store migration and new application
metadata in an ordinary SQL metadata table.

Add a target operation that accepts an allowlisted source binding and logical `GameID`. The target:

1. obtains the source stub from that binding using the same named `GameID`;
2. fetches the dump directly from the source Durable Object;
3. verifies the checksum and requires empty target storage;
4. restores SQL and KV atomically;
5. runs `migrate_storage_to_current` and loads the game to validate deserialization and log replay;
6. returns the resulting storage version and checksum.

The coordinator never downloads or uploads the dump. A failed target remains unrouted and may be
deleted and retried.

### Prerelease environment

Add a Wrangler `preprod` environment. It deploys a separate Worker with its own SQLite Durable
Object namespace, D1 database, secrets, routes, and frontend configuration. Configure:

- preprod `ARPEGGIOGAME` for its own `ArpeggioGameSql` namespace;
- preprod `PRODUCTION_ARPEGGIOGAME` as an external Durable Object binding to
  `ArpeggioGameSql` with `script_name = "arpeggio-backend"`;
- production `PREPROD_ARPEGGIOGAME` as an external binding to `ArpeggioGameSql` with
  `script_name = "arpeggio-backend-preprod"`.

Deploy the Dioxus frontend to a preprod Pages branch configured for that Worker. Production and
preprod D1 databases retain Cloudflare Time Travel as an operational recovery backstop.

Add a **Copy to preprod** action to `arpui/src/admin_view.rs`. The production admin endpoint
authenticates the superuser and invokes the named target through `PREPROD_ARPEGGIOGAME`, passing the
requesting user and `GameID`. The target pulls from `PRODUCTION_ARPEGGIOGAME`, restores, migrates,
creates the preprod D1 metadata/access for that administrator, and returns the playable URL,
storage version, and checksum. The Admin UI displays progress, failure details, and the result.
It also supports deleting or replacing an existing preprod copy and removes the retired KV-backed
status fields and columns.

Production is never mutated or paused for a preprod copy; the source dump transaction supplies a
point-in-time snapshot.

Every release, including releases without migrations, should be deployed to preprod first.

### Wrangler configuration

Use Cloudflare's declarative `[exports]` configuration instead of the legacy `[[migrations]]`
Durable Object lifecycle history:

- declare the existing `ArpeggioGameSql` namespace as `sqlite`;
- declare preprod's new namespaces as `sqlite`;
- configure the own-namespace and cross-environment Durable Object bindings above;
- configure D1 bindings, variables, and secrets explicitly per environment.

This changes namespace lifecycle configuration only; it does not migrate game data. Do not add a
`deleted` tombstone for the retired KV-backed class as part of this phase: Cloudflare would
permanently delete that namespace and its data on deployment.

### Phase 1 commands

```text
just migration-test
just deploy-to-preprod
just show-game-storage-version GAME_ID environment="production"
just deploy-to-production
```

Preprod game copy and cleanup are Admin UI workflows, not `just` commands.

`deploy-to-production` performs a normal deployment. Storage migration remains lazy and happens
when each Durable Object next receives a request.

### Phase 1 completion

- Existing and restored storage use the same migration chain.
- Real production games can be copied, migrated, and played in preprod without production writes.
- Production and preprod own separate storage; only the explicit copy bindings cross environments.
- Wrangler uses declarative Durable Object exports.
- A normal deployment requires no central migration coordinator.

## Phase 2: Multi-generation deployment

### Runtime generations

For a risky migration, deploy the new code as a separate Worker script and SQLite Durable Object
namespace, for example:

```text
arpeggio-game-g1
arpeggio-game-g2
```

Wrangler environments continue to represent preprod versus production; generation names represent
incompatible runtime/storage releases.

A stable gateway Worker binds to each supported generation's Durable Object namespace using
external Durable Object bindings with `script_name`.

Each target generation also has allowlisted external bindings to the source generations from which
it may pull dumps. Bindings are fixed at deployment rather than selected from arbitrary runtime
names.

### D1 routing

Add a central route for each logical `GameID`:

```text
game_id, generation, durable_object_id, state, migration_id
```

`state` is at least `active` or `migrating`. Conditional D1 updates prevent concurrent migrations
and block new connections while a game is migrating.

Record migration runs with source and target generations, object IDs, revisions, checksums, code
version, status, and error.

### Generation migration

For each game:

1. Mark its D1 route `migrating`.
2. Persistently fence mutations in the source Durable Object and close its WebSockets.
3. Record the fenced source revision `R` separately from the dump format.
4. Invoke a fresh target-generation Durable Object and tell it which allowlisted source binding and
   `GameID` to pull.
5. The target fetches the dump directly, restores it, runs the normal migration chain, and loads
   the resulting `Game`.
6. Compare revisions and domain checksums, resource counts, and invariants.
7. Atomically update the D1 route to the target and mark it `active`.
8. Retain the frozen source Durable Object and its Worker generation.

Retries resume from the recorded state or create another empty target. A failed target is never
routed to users.

After cutover, the source becomes stale as soon as the target accepts writes. Returning to it would
discard those writes unless a reverse migration is provided.

### Phase 2 commands

```text
just deploy-new-generation GENERATION
just plan-game-generation-migration GAME_ID GENERATION
just migrate-game-to-generation GAME_ID GENERATION
just migrate-all-games-to-generation GENERATION
just show-game-migration MIGRATION_ID
just show-game-route GAME_ID
```

### Phase 2 completion

- Deployment policy can select normal or generation deployment without changing migration code.
- A generation migration never mutates the source game's storage.
- Routing cutover is atomic and failed migrations leave the source authoritative.
- Old Worker generations remain available until explicitly retired.
