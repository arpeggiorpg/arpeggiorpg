# Hosted Split Decision Log

This document records implementation-time judgment calls for
[`hosted-split.md`](hosted-split.md). It is intentionally retained in the repository so decisions
that are expedient, uncertain, or likely to need revision do not disappear into commit history.

## Decision format

- **Status** is `accepted`, `provisional`, `superseded`, or `open`.
- **Phase** identifies the first implementation phase affected.
- **Reasoning** records why the decision was made with the evidence available at the time.
- **Revisit when** states what new information should trigger another decision.

## D001: Keep the existing crate names during the in-repository refactor

- **Status:** provisional
- **Phase:** 1
- **Decision:** Keep `arptypes`, `arpeggio`, and `arpui` as crate names through Phases 1–4. Add new
  modules and crates without performing cosmetic package renames.
- **Reasoning:** Repository and package renames create broad manifest and lockfile churn without
  proving the hosted boundary. They are easier to evaluate during the physical repository split in
  Phase 5.
- **Revisit when:** Phase 5 starts or package publication requires final names.

## D002: Treat the standalone runtime as one game per server

- **Status:** accepted
- **Phase:** 2
- **Decision:** The standalone UI uses `/` for the GM and `/Player/{name}` for players. The native
  server owns exactly one game.
- **Reasoning:** The agreed route contains no game ID and the standalone experience deliberately
  excludes the hosted account-level game list. A single game also maps cleanly to the current
  Durable Object execution model.
- **Revisit when:** There is a concrete standalone multi-game use case and a corresponding route and
  storage design.

## D003: Direct player routes do not implicitly create players

- **Status:** provisional
- **Phase:** 2
- **Decision:** Only the GM's `+ Player` action registers a player. Opening `/Player/{name}` for an
  unknown player produces a clear not-found error.
- **Reasoning:** Implicit creation on a GET-like navigation makes typos mutate game state and lets
  any visitor create arbitrary players. The explicit button is the flow requested in the plan.
- **Revisit when:** Usability testing shows that shareable player URLs need a separate join flow.

## D004: Preserve hosted wire compatibility while introducing typed envelopes

- **Status:** provisional
- **Phase:** 1
- **Decision:** New typed Rust envelopes retain the current JSON field names and refresh tags.
- **Reasoning:** The Worker and Dioxus UI currently interoperate with those shapes. Changing the
  Rust types and the wire format simultaneously would make regressions harder to isolate and would
  complicate staged commits.
- **Revisit when:** Both server adapters use the typed protocol and a versioned protocol migration
  is justified.

## D005: Use a native SQLite schema optimized for the standalone server

- **Status:** provisional
- **Phase:** 4
- **Decision:** Native persistence stores the current serialized game, ordered logs, and periodic
  serialized snapshots in ordinary SQLite. It does not reproduce Durable Object typed tables.
- **Reasoning:** The plan explicitly permits different internal schemas. The public `Game` is
  serializable, and a compact native schema can preserve transactional updates and rollback without
  importing Cloudflare-specific migration machinery.
- **Revisit when:** Phase 6 defines the portable export format or native performance demonstrates a
  need for typed entity tables.

## D006: Keep the native server and UI independently deployable

- **Status:** accepted
- **Phase:** 4
- **Decision:** The native server accepts configured browser origins and may serve a compiled UI
  directory, but the UI continues to read an independently configured server URL.
- **Reasoning:** Serving one bundle is convenient, while independent origins are a stated
  requirement and are useful during development and static hosting.
- **Revisit when:** Runtime configuration moves away from the existing HTML metadata mechanism.

## D007: Defer standalone GM authentication

- **Status:** provisional
- **Phase:** 4
- **Decision:** The first native server binds to loopback by default and does not implement a GM
  capability token.
- **Reasoning:** Authentication is a documented non-goal for the initial standalone runtime.
  Loopback-by-default prevents accidental network exposure while the game and protocol split are
  established.
- **Revisit when:** LAN binding is tested for general use or a public-interface deployment is
  documented.

## D008: Do not migrate the legacy TypeScript UI

- **Status:** accepted
- **Phase:** 1
- **Decision:** Preserve `ui/` only as deprecated reference material. Do not update its handwritten
  codecs, request unions, build, or runtime behavior, and do not treat it as a completion
  dependency. The Dioxus UI is the supported client for the split.
- **Reasoning:** The owner confirmed that the TypeScript/React client is fully deprecated and
  unused. Maintaining a second manually decoded protocol would spend effort on code that is not
  shipped.
- **Revisit when:** Phase 5 decides whether to archive the TypeScript UI in either repository or
  remove it.

## D009: Keep heterogeneous response serialization in the shared dispatcher

- **Status:** provisional
- **Phase:** 3
- **Decision:** The shared dispatcher accepts a typed `GameRequest` and returns a small typed action
  enum, but immediate query results are represented as `serde_json::Value`. Persistence changes,
  rollback requests, and image operations remain distinct typed action variants.
- **Reasoning:** Each request has a different response type, while the existing RPC protocol
  correlates those responses dynamically and deserializes them into a type selected by the caller.
  Keeping JSON only at that response boundary avoids a large duplicate response enum while still
  preventing platform adapters from reimplementing authorization and game behavior.
- **Revisit when:** The protocol adopts request/response type pairing through traits or generated
  bindings, or a versioned response enum becomes useful for compatibility guarantees.

## D010: Use direct HTTP PUTs for standalone image uploads

- **Status:** provisional
- **Phase:** 4
- **Decision:** `RequestUploadImage` reserves a UUID-backed file and returns a same-server HTTP PUT
  URL plus its final `/images/{id}` URL. Content type is stored beside the file; SQLite records the
  image ID, purpose, and path.
- **Reasoning:** This preserves the existing two-URL image response without imitating Cloudflare's
  signed direct-upload API. UUID validation prevents path traversal, and the standalone server is
  already intentionally unauthenticated and loopback-only by default.
- **Revisit when:** Standalone authentication or public-network deployment requires expiring upload
  capabilities, or image processing needs a richer metadata schema.
