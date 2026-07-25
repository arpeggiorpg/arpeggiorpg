# Durable Object Migration System Plan

## Status

Proposed.

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

## Invariants

- A migration and its version update commit atomically.
- Migrations are ordered, idempotent, local to the Durable Object, and perform no external I/O.
- Failed migration prevents game loading and leaves the previous storage version intact.
- The complete supported migration chain remains available so games may skip releases.
- A dump records its storage version, game revision, logical storage contents, and checksum.
- Restoring a dump is allowed only into an empty Durable Object.
- Restore reconstructs storage at the dump version, then invokes the normal migration chain.
- Production game logic only observes current-version storage.

## Phase 1: Core migrations and prereleases

### Migration API

Replace the current imperative migration code with a small migration registry and one entry point:

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
- equivalence between opening existing storage and restoring the same storage from a dump.

### Versioned dumps

Define a logical, versioned `GameDump`; do not execute SQL supplied by a dump.

Add authenticated internal operations to:

- export a consistent dump from a game;
- import a dump into an empty Durable Object;
- migrate and validate the imported game;
- return its resulting version, revision, and checksum.

The importer creates the schema through the dump's version, restores its rows, then runs the
remaining migrations in one transaction.

### Prerelease environment

Add a Wrangler `preprod` environment. It deploys a separate Worker with separate SQLite Durable
Object namespaces, D1 database, secrets, routes, and frontend configuration. No preprod binding may
reference a production Durable Object namespace.

Deploy the Dioxus frontend to a preprod Pages branch configured for that Worker. Production and
preprod D1 databases retain Cloudflare Time Travel as an operational recovery backstop.

`copy-game-to-preprod` authenticates to production, exports a consistent dump, imports it into
preprod, grants only the requesting administrator access, and prints the playable URL and migration
result. Production remains writable because this is a point-in-time test copy.

Every release, including releases without migrations, should be deployed to preprod first.

### Wrangler configuration

Replace the legacy `[[migrations]]` Durable Object lifecycle history with Cloudflare's declarative
`[exports]` configuration:

- retain the existing `ArpeggioGame` namespace as `legacy-kv`;
- retain `ArpeggioGameSql` as `sqlite`;
- declare preprod's new namespaces as `sqlite`;
- configure Durable Object bindings, D1 bindings, variables, and secrets explicitly per
  environment.

This changes namespace lifecycle configuration only; it does not migrate game data.

### Phase 1 commands

```text
just migration-test
just deploy-to-preprod
just copy-game-to-preprod GAME_ID
just delete-game-from-preprod GAME_ID
just show-game-storage-version GAME_ID environment="production"
just deploy-to-production
```

`deploy-to-production` performs a normal deployment. Storage migration remains lazy and happens
when each Durable Object next receives a request.

### Phase 1 completion

- Existing and restored storage use the same migration chain.
- Real production games can be copied, migrated, and played in preprod without production writes.
- Production and preprod resources are isolated.
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
3. Export a dump at revision `R`.
4. Import it into a fresh target-generation Durable Object.
5. Run the normal migration chain and validate the loaded `Game`.
6. Compare revisions, checksums, resource counts, and domain invariants.
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
