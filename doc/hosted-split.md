# Hosted Repository Split Plan

## Status

In progress. Implementation work covers Phases 1 through 4 before the physical repository split.
Judgment calls and provisional implementation decisions are recorded in
[`hosted-split-decisions.md`](hosted-split-decisions.md).

## Core decision

Split the current repository into:

- `arpeggio`: the open-source game, protocol, reusable Dioxus UI, and a native web server;
- `arpeggio-hosted`: the proprietary hosted service, including Google authentication, user and
  game membership, the Cloudflare Worker, hosted administration, and deployment configuration.

`arpeggio-hosted` depends on `arpeggio`. The open-source repository never depends on
`arpeggio-hosted`.

The open-source UI is not a special "local UI." It is a standalone web UI configured to connect to
an Arpeggio server URL. The same UI may be served by the native server, hosted separately, or used
against another compatible implementation.

The native server and the Cloudflare Worker implement the same platform-neutral game protocol:

- the native server is the straightforward open-source and self-hosted runtime;
- the Cloudflare Worker remains the production runtime for `arpeggio-hosted`;
- `wrangler dev` remains available in `arpeggio-hosted` for production-parity development.

## Goals

- Make the core game genuinely usable from the open-source repository by itself.
- Keep Google login, hosted user accounts, Cloudflare infrastructure, and deployment policy
  proprietary.
- Preserve one shared game protocol and one shared implementation of the game rules.
- Let the standalone UI connect to a configured URL rather than assuming that its server is on the
  same machine.
- Avoid making Cloudflare, Wrangler, Node, or Google credentials prerequisites for running the
  open-source version.
- Keep the initial standalone experience simple: one game per server, a GM view, and named player
  views.

## Non-goals

- Reproduce hosted account management in the open-source server.
- Make the native server's internal persistence schema identical to Durable Object storage.
- Make the unauthenticated standalone server safe to expose directly to the public Internet.
- Share Cloudflare and Axum transport objects through a common abstraction.
- Hide ordinary game concepts merely because they currently live in `multitenant.rs`.
- Add broad CI checks that reject words or dependencies associated with the hosted implementation.

## Repository boundaries

### `arpeggio`

The public repository contains:

- `arptypes`, divided into domain types and platform-neutral protocol types;
- the `arpeggio` game simulation crate;
- `arp3d`;
- reusable Dioxus game UI and components;
- a standalone Dioxus application configured with a server URL;
- a native Axum server;
- native persistence and local image storage;
- `indexed`, `nonempty`, and any other libraries required by the core game;
- game assets, open-source documentation, and development configuration.

### `arpeggio-hosted`

The private repository contains:

- the Cloudflare Worker;
- Durable Object storage, storage migrations, dump, restore, and preproduction-copy code;
- `worker-sqlite-dump`;
- D1 user, membership, metadata, and superuser storage;
- Google OAuth and ID-token validation;
- hosted invitation acceptance and account membership;
- the hosted game list and authenticated application shell;
- the Cloudflare-specific administration UI;
- Cloudflare Images integration;
- `wrangler.toml`, D1 schema, production and preproduction frontend configuration, deployment
  commands, and other hosted infrastructure configuration;
- the legacy TypeScript UI, if it is retained at all.

The dependency direction is:

```text
arpeggio-hosted  ─────►  arpeggio
      private                public
```

The hosted repository should consume tagged releases or pinned revisions of the public crates.
During development, a sibling checkout may override those dependencies with local paths.

## Split `multitenant.rs` by responsibility

The current `arptypes/src/multitenant.rs` combines the game protocol with hosted identity and
membership. It should not move wholesale into the private repository.

### Public protocol and game-hosting types

Move or rename these into a public `protocol` module:

- `GameID`;
- `GameMetadata`;
- `GameIndex`;
- `Role`;
- `GameAndMetadata`;
- `PlayerGameAndMetadata`;
- image-purpose types;
- core game requests and responses;
- typed WebSocket request, response, error, and refresh envelopes.

`Role`, game IDs, history indices, and game requests are needed by both server implementations and
are not inherently multitenant.

### Hosted types

Move these into `arpeggio-hosted`:

- `UserID`;
- hosted user/game membership records;
- superuser response types;
- preproduction-copy result types;
- Google identity details;
- hosted invitation checks and acceptance results.

The current browser-facing `GameProfile` includes a `UserID` even though the game-list UI only needs
the game ID, profile name, role, and metadata. Replace it with a public `GameSummary` if both UIs
still need a common list representation. Keep the account-to-game membership record private.

### Hosted requests on a shared WebSocket

Do not force invitation and hosted-administration requests into the core game-request enum merely
because they travel over the same socket.

Make the RPC envelope generic over its request payload. The shared UI sends public
`GameRequest` values. Hosted components may send private `HostedGameRequest` values through the same
connection. The Worker dispatches the two request families to the appropriate handlers.

If standalone player creation later needs invitation-like capabilities, add a small public
game-hosting request based on that use case rather than exposing hosted account membership.

## Standalone UI

The public application should be called `arpeggio-ui` or simply remain `arpui`; it should not be
called `arpeggio-local-ui`.

Its server URL remains runtime or build configuration, initially through the existing `RPI_URL`
configuration. The websocket URL is derived from that base URL. Nothing in the UI assumes that the
server is running on `localhost` or on the same origin.

The initial standalone navigation model is one game per configured server:

- `/` opens the GM view;
- a `+ Player` button prompts for a player name;
- confirming the dialog registers that player in the game;
- the UI then navigates to `/Player/{name}`;
- `/Player/{name}` opens `PlayerView` for that player.

The `+ Player` action can use the existing GM `RegisterPlayer` command rather than introducing a
hosted user or membership record. Refreshing or directly opening `/Player/{name}` reconnects to the
configured server as that game player.

This deliberately has no Google login, account-level game list, invitation acceptance page, admin
page, or log-off action.

Because the route contains no game ID, the first native-server version is explicitly a single-game
server. If the standalone server later supports multiple games, that should be introduced as a
separate navigation decision rather than retaining the hosted account/game model by accident.

## Reusable Dioxus UI

Convert `arpui` from one hosted application binary into reusable game UI plus thin application
shells.

The public UI library owns:

- GM and player game views;
- catalog, chat, grid, history, and 3D components;
- common visual components;
- platform-neutral WebSocket request correlation and refresh handling;
- shared game state signals or contexts.

The standalone public application owns:

- the configured server URL;
- the GM route;
- the `+ Player` flow;
- the `/Player/{name}` route.

The private hosted application owns:

- Google OAuth initiation and callback;
- authentication cookies and headers;
- authenticated layout;
- hosted game list and game creation;
- invitation acceptance;
- superuser and Cloudflare administration;
- production and preproduction environment behavior.

Core game views must not depend directly on the hosted application's concrete route enum. Pass
navigation callbacks or route builders into the game views. In particular, scene selection in the
GM view should ask its application shell to update the URL rather than constructing a hosted route
itself.

The shared WebSocket connector should receive a connection URL or connection provider. It should
not know how Google authentication is exchanged for a hosted WebSocket token. The hosted shell
performs that exchange; the standalone shell obtains its connection information from the configured
server.

## Shared protocol and game execution

The two backends should share protocol and game behavior, not framework objects.

### Typed wire protocol

Define public serialized types for:

- request IDs and request envelopes;
- successful responses;
- structured errors;
- full GM refreshes;
- player-filtered refreshes.

Add serialization fixtures or round-trip tests so both implementations remain wire-compatible.
This replaces anonymous JSON construction and makes error handling part of the protocol.

### Platform-neutral request dispatcher

Extract the pure part of `worker/src/wsrpi.rs` into public code. Given:

- the current game;
- a role;
- an optional player ID;
- a core game request;

the dispatcher produces a query result, a `ChangedGame`, a rollback instruction, an image
operation, or an authorization error.

Persistence, socket management, and broadcasting remain in the platform adapters:

- the Worker adapter uses Durable Object state, hibernatable WebSockets, and Cloudflare Images;
- the Axum adapter uses Tokio tasks, native WebSockets, native persistence, and filesystem-backed
  images.

Do not begin by forcing Cloudflare's `Rc`-based WASM runtime and Axum's `Send + Sync` runtime behind
one large async trait. Keep the shared layer synchronous and domain-oriented where practical.

## Native server

Add an open-source native server, tentatively named `arpeggio-server`.

It:

- listens on a configurable address;
- hosts one game from a configured data directory;
- exposes the HTTP and WebSocket interface expected by `arpeggio-ui`;
- serializes changes to the game;
- broadcasts GM and player-filtered refreshes;
- persists game state and history;
- stores uploaded images under the data directory and serves them over HTTP;
- may optionally serve a compiled `arpeggio-ui` bundle, without requiring the UI to use the same
  origin.

The Durable Object's one-live-game execution model maps naturally to one native game actor. A Tokio
task or equivalent serialized runtime owns the game, processes commands in order, persists each
successful change, and publishes refresh events to connected clients.

The server should bind to loopback by default. Binding to a LAN or public interface is explicit.
Without authentication, anyone who can reach the GM connection can control the game. A generated GM
capability token may be added later without introducing external identity or hosted accounts.

## Native persistence

Do not port the full Durable Object typed-table and migration implementation before splitting the
repositories.

The native server can initially use ordinary SQLite with:

- game metadata;
- the current serialized `Game`;
- ordered game logs and `GameIndex` values;
- periodic snapshots for rollback;
- locally stored image metadata.

Write each successful change and its logs in one transaction. Preserve the existing history and
rollback behavior, but allow the native and Durable Object schemas to differ.

Define a portable public game export format separately from either storage implementation.
Eventually:

- the native server exports and imports that format;
- the hosted Worker exports and imports the same format;
- internal storage migrations stay private to each implementation.

## Image storage

Define a public image-store interface around the operations the game protocol requires.

Implementations:

- native server: save files beneath the configured data directory and return server URLs;
- hosted Worker: upload to Cloudflare Images and return delivery URLs.

The core request dispatcher should describe the image operation without depending on
`worker::Url`, Cloudflare account IDs, or filesystem paths.

## Development workflows

### Open-source development

The public repository should support:

```text
cargo test
cargo check --target wasm32-unknown-unknown
arpeggio-server --data-dir ./arpeggio-data
```

The Dioxus UI may be run separately and configured with the server URL.

### Hosted development

The private repository retains:

- `wrangler dev`;
- local D1 initialization;
- Durable Object migration and restore tests;
- hosted Dioxus configuration;
- preproduction and production deployment commands.

This provides production parity without making Wrangler part of the open-source runtime.

## Migration phases

### Phase 1: Establish boundaries inside the current repository

- [x] Split `multitenant.rs` into public protocol and hosted types.
- [x] Add typed RPC envelopes and refresh messages.
- [x] Separate core game requests from hosted requests.
- [x] Remove `UserID` from browser-facing game summaries.
- [x] Add protocol serialization and round-trip tests.

### Phase 2: Make the Dioxus UI reusable

- [ ] Move Google OAuth, auth cookies, and authenticated layout into a hosted shell.
- [ ] Move the hosted game list, invitation acceptance, and admin page into that shell.
- [ ] Make game views independent of the hosted route enum.
- [ ] Make the WebSocket connector accept connection information from the application shell.
- [ ] Add the standalone `/` GM route.
- [ ] Add the `+ Player` dialog and player-registration action.
- [ ] Add the `/Player/{name}` route and direct-load behavior.

### Phase 3: Extract shared game-session behavior

- [ ] Move the platform-neutral request dispatcher out of the Worker.
- [ ] Keep persistence and broadcasting behind the Worker adapter.
- [ ] Verify that the existing Worker still handles the shared protocol.
- [ ] Add end-to-end protocol tests covering one GM and at least two players.

### Phase 4: Add the native server

- [ ] Add Axum HTTP and WebSocket endpoints.
- [ ] Add the single-game actor and connection registry.
- [ ] Add native SQLite persistence and rollback.
- [ ] Add filesystem-backed image upload and serving.
- [ ] Allow the server and UI origins to be configured independently.
- [ ] Optionally serve the compiled Dioxus bundle from the native server.

### Phase 5: Split repositories

- [ ] Create `arpeggio` from the refactored public crates, UI, and native server.
- [ ] Create `arpeggio-hosted` from the Worker, hosted UI shell, hosted storage, and deployment
  configuration.
- [ ] Point `arpeggio-hosted` at a pinned public revision.
- [ ] Give each repository an explicit license.
- [ ] Update developer documentation and commands in both repositories.
- [ ] Verify fresh checkouts can run their documented workflows independently.

### Phase 6: Data portability

- [ ] Specify a versioned public game export format.
- [ ] Implement native export and import.
- [ ] Adapt Worker dump/restore or add a conversion layer for the public format.
- [ ] Test round trips between native and hosted implementations.

## Repository-history and licensing notes

The current repository is labeled MIT-licensed. Previously published versions remain under that
license; moving future hosted development into a private repository does not retract past grants.

Before publishing the new repository boundaries:

- decide whether the public repository retains filtered history or starts from a documented split
  commit;
- run a full history secret scan;
- remove hosted identifiers and personal account records that do not belong in the public tree;
- ensure the private repository has an appropriate proprietary license;
- preserve attribution and comply with the licenses of existing contributions and dependencies.

## Open questions

- Should the public crate and binary keep the existing `arpui` name or be renamed to
  `arpeggio-ui`?
- Should the native server serve the compiled UI by default, or should that be an optional bundle?
- What exact runtime configuration mechanism should set the UI's server URL outside the existing
  HTML meta tag?
- Should direct access to `/Player/{name}` create a missing player, or should only the GM's
  `+ Player` action be allowed to register one?
- Does the first native persistence format need snapshot-level compatibility with hosted rollback,
  or is portable export/import sufficient?
- Is a generated GM capability token required for the first LAN-capable release?
