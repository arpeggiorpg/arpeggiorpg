# Catalog, Collections, Modules, and Storage Plan

## Status

In progress. Phases 0, 1, and 2 are complete, deployed to preproduction, and manually tested.
Phases 3 and 4 are implemented and verified locally but are not yet deployed to preproduction.
Module work has not started.

This is the canonical plan for replacing campaign folders with a resource catalog and collections,
for importing and exporting modules, and for eventually moving from whole-game snapshots to
piecemeal typed tables with one JSON blob per entity.

It supersedes `doc/piecemeal sql storage.md`. The useful parts of that plan are incorporated here,
but persistence work is deliberately sequenced after a UI experiment and the resource-model
change.

### Progress update — 2026-07-29

Completed in the Rust/Dioxus UI:

- Replaced the normal Campaign tree with a Catalog as the default GM view.
- Added Current Scene, session-local Recent, resource-type, search, collection overview, and
  individual collection views.
- Added one location-free create flow for scenes, creatures, notes, classes, abilities, and items.
- Flattened Manage Creatures, preserved search and grant/remove behavior, and placed creatures
  assigned when the dialog opens at the top of the list.
- Added existing creature icons and class emojis to catalog rows.
- Removed the legacy folder-tree UI instead of retaining it behind a development switch.
- Left the TypeScript UI untouched.
- Deployed the current prototype to `https://preprod.arpeggio-331.pages.dev`.

Completed in the Phase 1 domain model:

- Added first-class `CollectionID`, `NoteID`, `Collection`, and top-level `Note` types.
- Made note ownership and visibility explicit instead of deriving authorization from folder paths.
- Added `Game.collections` and `Game.notes` while retaining whole-game snapshot persistence.
- Added collection validation for stale references and duplicate membership within a collection;
  resources may belong to multiple collections.
- Added deterministic folder-to-collection and embedded-note-to-top-level-note conversion.
- Implemented Worker-only catalog-domain conversion for every existing snapshot and log before a
  current `Game` is created or replay begins.
- Moved every folder-era wire type and conversion rule into a Worker-only migration module.
- Removed `campaign`, folder commands, folder logs, and FolderTree dependencies from `arptypes`,
  `arpeggio`, and `arpui`.
- Added folder-free resource, note, collection, rename, copy, and catalog-deletion logs.
- Added pathless resource and note commands; player note authorization now uses explicit ownership.
- Switched catalog and player serialization to the new top-level notes and collections.
- Verified the migration in the local Durable Object suite and verified the workspace and WASM
  builds.

Completed in Phase 2:

- Added explicit typed commands and logs for renaming collections, adding and removing membership,
  and merging collections.
- Added Dioxus controls to create, rename, merge, and delete collections.
- Added per-resource collection membership management with multiple membership.
- Made collection removal distinct from confirmed catalog deletion; neither deleting nor merging
  collections deletes their resources.
- Kept manual resource reordering deferred.

Implemented locally in Phases 3 and 4:

- Added separate JSONB tables for scenes, creatures, notes, items, abilities, classes, collections,
  and players plus singleton game state, all keyed by `snapshot_idx`.
- Reserved `snapshot_idx = -1` for mutable current state and nonnegative indices for immutable
  snapshots.
- Consolidated catalog-domain conversion, typed-table creation, and historical snapshot conversion
  into one atomic storage migration.
- Converted every legacy snapshot and log before constructing a current `Game`; the migration
  removes `game_snapshots` and advances the storage version only after complete validation.
- Switched `GameStorage` cold loading to the typed tables.
- Made `GameStorage::update_game` transactionally persist before-and-after entity deltas and logs.
- Replaced periodic whole-game serialization with transactional copies of current typed rows.
- Added storage/RPI rollback to an exact snapshot/log prefix, followed immediately by a new
  immutable typed snapshot.
- Removed rollback from core `GMCommand` and `GameLog`.
- Added migration, all-entity round-trip, snapshot parity, typed-authority, rollback, transaction
  atomicity, dump/restore, and recovery coverage in the local Durable Object suite.

Not yet implemented:

- Preprod deployment and validation of the unified migration and rollback path.
- The new module format, dependency-aware import/export, and module provenance.

Current decisions and constraints:

- Legacy folder-based module imports do not need to remain available. There are no known module
  files in active use, and migration fails transactionally with a precise error if it encounters a
  legacy `LoadModule` log.
- Folder compatibility exists only in the Worker storage migration. Current domain types and logs
  have no folder representation.
- Manual resource reordering is deferred. Catalog views sort resources for presentation while
  preserving the typed membership vectors in storage.
- Piecemeal persistence will use a dedicated table for each entity type with one JSON blob per row.
  It will not add a repository abstraction or fully normalize nested entity fields initially.
- Current and historical entities will share those typed tables. `snapshot_idx = -1` identifies
  mutable current state; nonnegative indices identify immutable snapshots.
- Rollback will be a storage/RPI operation rather than a core `Game` command. Restoring a point in
  history will immediately create a new immutable snapshot before normal logging resumes.
- The catalog-domain, typed-table, and snapshot-indexing changes have not established separate
  deployed migration boundaries. They will ship as one migration directly from legacy
  folder/monolithic storage to the final snapshot-indexed typed schema.

## Implementation phases

### Phase 0: UI prototype

- [x] Add catalog navigation to the Rust/Dioxus GM UI.
- [x] Add type views and search.
- [x] Add Current Scene and Recent where practical.
- [x] Present folder paths as temporary read-only collections.
- [x] Remove the folder tree from the normal UI.
- [x] Add a location-free prototype create flow.
- [x] Gather experience before changing serialized state.
- [x] Do not touch the TypeScript UI or generated TypeScript.

### Phase 1: Domain model

- [x] Add `CollectionID` and `NoteID`.
- [x] Make notes top-level resources.
- [x] Add `Collection` with separate typed vectors.
- [x] Add `Game.collections`.
- [x] Add collection validation and helper methods.
- [x] Continue storing whole-game snapshots.
- [x] Implement Worker-only conversion logic for legacy snapshots and logs for use by the unified
  storage migration.
- [x] Remove folder compatibility from the core domain and replay path.

### Phase 2: Collection editing and Dioxus UI

- [x] Add foundational collection commands and logs (completed during Phase 1 migration work).
- [x] Remove folder selection from new-resource flows.
- [x] Support add, remove, create, rename, merge, and delete collection operations.
- [x] Make catalog deletion explicitly different from collection removal.
- [x] Replace the campaign tree as the normal UI (completed early during Phase 0).

### Phase 3: Piecemeal typed-table storage

- [x] Add one JSON-blob table per entity type plus singleton game-state storage.
- [x] Replace the local intermediate migration with the unified final-schema migration described
  in Phase 4.
- [x] Make `GameStorage` compute and persist changed and deleted top-level entities.
- [x] Temporarily dual-write typed tables, logs, and whole-game snapshots during local verification.
- [x] Add full reconstruction parity, atomicity, dump, restore, and recovery tests.
- [x] Switch cold loading to the typed tables.

### Phase 4: Typed snapshots, rollback, and cleanup

- [x] Add `snapshot_idx` to every typed entity table, using `-1` for mutable current state and
  nonnegative indices for immutable snapshots.
- [x] Store singleton game state and snapshot metadata by `snapshot_idx`.
- [x] Consolidate catalog-domain conversion, typed-table creation, and snapshot indexing into one
  unpublished migration.
- [x] In that migration, convert every legacy monolithic snapshot and log into the final typed
  historical representation before constructing a `Game`.
- [x] Create a snapshot transactionally by copying all current typed rows into a new immutable
  generation.
- [x] Reintroduce rollback as a storage/RPI operation:
  - load the selected typed snapshot;
  - replay logs to the selected point;
  - transactionally replace the current rows;
  - immediately create a new immutable snapshot of the restored state;
  - continue logging from that new snapshot.
- [x] Port the recent-history UI to Dioxus and add confirmed restore actions for individual log
  positions.
- Validate the complete migration and rollback path in preprod before production deployment; do
  not deploy an intermediate ID-only typed schema or dual-write storage version.
- [x] Stop writing monolithic snapshots and remove the legacy snapshot table as part of the unified
  migration after its transactional validation succeeds.
- [x] Update dumps and recovery tests for current rows, historical typed rows, snapshot metadata,
  logs, and rollback.
- [x] Remove `campaign` and normal-use folder code (completed during Phase 1).
- [x] Remove folder UI (completed early during Phase 0).
- [x] Remove legacy commands and logs from core replay (completed during Phase 1).
- Consider removing the standalone `foldertree` crate if no other code uses it.

### Phase 5: Modules

- Define the versioned typed module format.
- Implement reference discovery and dependency classification.
- Implement export preview and validation.
- Implement ID remapping and transactional import.
- Automatically create a collection for imported content.
- Add round-trip and dependency-closure tests.

## Deferred UI polish backlog

Address these together after the main implementation phases unless one blocks testing:

- Show the full collection name when hovering any truncated collection name, including names in
  the catalog collection list.
- Reduce the visual emphasis of the `+ Collection` action.
- Replace the `+ Resource` action with a resource-type menu.
- Make the main panes resizable.
- Add convenient navigation between scenes connected by scene links.
- Remove redundant inner cards and headings from the Catalog, Players, and Invitations tabs.
- Provide one easy, consistent way to rename resources and collections.

## Decision summary

Arpeggio will move toward three separate concepts:

1. **Catalog**: all resources owned by a game, browsable primarily by resource type and search.
2. **Collection**: an optional, non-nested grouping of resource IDs.
3. **Module**: a portable set of complete resources that can be imported into or exported from a
   game.

These concepts replace the folder tree's current combination of navigation, ownership, permission
scoping, and module boundaries.

The intended implementation order is:

1. Experiment with the catalog and collection experience entirely in the Rust/Dioxus UI.
2. Add collections and top-level notes to the Rust domain model while retaining whole-game snapshot
   blobs.
3. Replace folder-based commands with collection-oriented commands and complete the Dioxus UI.
4. Introduce piecemeal persistence using one typed table per entity type and one JSON blob per
   entity.
5. Store mutable current state and immutable snapshots in the same typed tables, restore rollback,
   and retire monolithic snapshots.
6. Implement dependency-aware module import and export against the final catalog and collection
   model.

The old TypeScript UI and `arptypes/src/bin/gen-ts.rs` are effectively dead. They do not need to be
updated as part of this work. New design and implementation work should target the Rust/Dioxus UI in
`arpui`.

## Motivation

The current `Game` already stores abilities, creatures, classes, scenes, and items in top-level
indexed maps. `campaign: FolderTree<Folder>` is a second structure that assigns those resources to
paths. Each `Folder` contains sets of resource IDs, except that notes are embedded directly and are
identified by name.

The tree currently determines:

- how users navigate resources;
- where new resources must be created;
- whether every resource is considered part of the campaign;
- the boundary used for module export;
- the destination used for module import;
- where player-authored notes are stored.

This coupling creates user-facing filing work and implementation work:

- most create commands require a `FolderPath`;
- folders require create, rename, move, copy, and delete behavior;
- moving or renaming a folder rewrites descendant paths;
- campaign validation requires every resource to appear in exactly one folder;
- module export copies a subtree but does not correctly resolve references outside that subtree;
- piecemeal persistence needs a special structural representation for campaign nodes;
- player permissions are partially encoded in paths such as `/Players/{player_id}`.

The new model separates these concerns. Resources belong to the game because they are in the
catalog. Collections only organize them. Modules only transfer them. Authorization is explicit.

## Goals

- Make creating and finding resources require less organization from users.
- Allow one resource to appear in multiple useful groupings without copying it.
- Keep collections understandable by disallowing nested collections.
- Make module export dependency-aware.
- Preserve the ability to share systems, rules, scenes, creatures, items, notes, and adventures.
- Permit a UI-only experiment before committing to data migrations.
- Keep the first domain implementation compatible with whole-game blob snapshots.
- Make later typed-table persistence simpler than the proposed campaign adjacency-list design.
- Preserve deterministic game logs and a safe migration path for existing games.

## Non-goals

- Updating or reviving the old TypeScript UI.
- Updating generated TypeScript declarations solely for this work.
- Building a public module registry or marketplace.
- Implementing automatic updates for imported modules in the first version.
- Designing a full package manager with dependency versions and overrides.
- Making collections hierarchical.
- Normalizing every resource field into relational SQL columns.

## Product model

### Catalog

The catalog is not a new container in the domain model. It is the user-facing interpretation of the
resource maps already present on `Game`.

The initial catalog UI should provide built-in views such as:

- Current scene
- Recent
- Scenes
- Creatures
- Rules
  - Classes
  - Abilities
  - Items
- Notes
- Collections
- Imported content, if module provenance is later exposed

Creating a resource should not require choosing a location. A newly created resource immediately
exists in its type view and can optionally be added to one or more collections.

Search should work across resource names and types. Rich tags and saved queries can be considered
later; collections are sufficient for the first model.

### Collection

A collection is an optional grouping of resources. It is not an ownership boundary.

```rust
uuid_id!(CollectionID);
uuid_id!(NoteID);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Collection {
    pub id: CollectionID,
    pub name: String,
    pub scenes: Vec<SceneID>,
    pub creatures: Vec<CreatureID>,
    pub notes: Vec<NoteID>,
    pub items: Vec<ItemID>,
    pub abilities: Vec<AbilityID>,
    pub classes: Vec<ClassID>,
}
```

The exact derives should follow what the Rust code needs at implementation time. TypeScript
generation is not a design constraint.

Collection rules:

- A resource may be in zero, one, or multiple collections.
- Each resource ID must occur at most once in each typed vector.
- Membership vectors preserve their stored order, but Phase 2 does not expose manual reordering.
- Collections cannot contain collections.
- Removing a resource from a collection does not delete the resource.
- Deleting a collection does not delete its resources.
- Merging collections unions their typed memberships into one destination, deduplicates IDs, and
  deletes only the source collections—not their resources.
- Deleting a resource removes its ID from every collection.
- Copying a resource creates a new resource ID; adding an existing resource to another collection
  does not copy it.
- Collection membership must not affect game mechanics or authorization.

The public data model should retain separate typed fields. A private `ResourceRef` enum may still be
useful for generic helper code, dependency traversal, or affected-entity tracking, but collections
should not serialize as a heterogeneous `Vec<ResourceRef>`.

### Notes

Notes should become top-level resources:

```rust
pub struct Note {
    pub id: NoteID,
    pub name: String,
    pub content: String,
    // Ownership and visibility fields to be decided separately.
}
```

Player ownership and visibility should become explicit fields or explicit authorization metadata.
They must not depend on a collection name or a path convention.

### Game

The eventual in-memory shape is approximately:

```rust
pub struct Game {
    pub current_combat: Option<Combat>,
    pub abilities: IndexedHashMap<Ability>,
    pub creatures: IndexedHashMap<Creature>,
    pub classes: IndexedHashMap<Class>,
    pub tile_system: TileSystem,
    pub scenes: IndexedHashMap<Scene>,
    pub items: IndexedHashMap<Item>,
    pub notes: IndexedHashMap<Note>,
    pub collections: IndexedHashMap<Collection>,
    pub players: IndexedHashMap<Player>,
    pub active_scene: Option<SceneID>,
}
```

`campaign: FolderTree<Folder>` has been removed from the current in-memory and serialized model.
Only the Worker migration has a private representation of that legacy field.

## Modules

### Definition

A module is a portable artifact containing complete resource values. It is not a live folder,
collection, or mounted database.

A module should contain:

- a format version;
- a stable module ID;
- a name;
- optional descriptive metadata;
- typed lists or maps of complete scenes, creatures, notes, items, abilities, and classes;
- a typed root selection identifying what the user explicitly chose to export;
- enough information to distinguish required dependencies from explicit roots;
- optional provenance fields reserved for future update support.

The serialized format should use typed fields rather than one heterogeneous resource list.

The first module version is a copy-based format:

- imported resources become editable local resources;
- all imported IDs are remapped when necessary;
- all internal references are rewritten consistently;
- importing the same module twice is allowed and creates another local copy;
- no live relationship to the source module is required.

The manifest should nevertheless reserve stable module and resource-origin identifiers so that a
future "install read-only module" or "update imported module" feature does not require another
format replacement.

### Export

Users may export:

- a collection;
- a manual typed selection;
- a single resource, such as a scene;
- eventually, an automatically generated view.

The explicit selection becomes the module's roots. Export then walks resource references and adds
dependencies.

Dependency traversal should distinguish at least:

1. **Required dependencies**: omitting these would make the exported resource invalid or unusable.
2. **Contextual contents**: resources normally expected to accompany the selected root.
3. **Optional links**: references that may legitimately point outside the module.

Examples:

- `Creature.class` is required.
- A class's abilities are required.
- Item definitions referenced by an included inventory are required.
- An ability referenced by `Condition::ActivateAbility` is required.
- Creatures placed in an exported scene are contextual contents.
- Related scenes and scene hotspots are optional links unless explicitly selected.

The exporter must:

- handle cycles;
- deduplicate resources;
- validate that every required reference resolves;
- report optional references that will not be included;
- produce a preview before download.

An internal helper such as `dependencies()` or `references()` should live with each resource type so
the export rules do not become one large, fragile match statement in the UI.

### Import

Module import should:

1. Parse and validate the module format.
2. Build a complete old-ID to new-ID mapping.
3. Rewrite all internal references.
4. Validate required references after rewriting.
5. Insert all resources atomically.
6. Create a collection named after the module by default.
7. Put all newly imported resources in that collection using its typed fields.
8. Record provenance if the module supplies it.

The import destination is the game catalog; users do not choose a folder.

Existing folder-based `.arpeggiogame` and JSON modules do not need a compatibility adapter. There
are no known module files in active use, and the new typed module format is expected to differ
substantially.

## UI experiment

The first phase should intentionally avoid domain, command, log, and persistence changes.

### Experiment scope

Build a catalog-oriented view in the Rust/Dioxus GM UI using the current `Game`:

- Show flat resource views sourced from the existing top-level maps.
- Add global search across names and resource types.
- Add Current Scene and Recent views where practical.
- Present each existing folder as a temporary collection named by its full path.
- Hide recursive folder navigation in the experimental view.
- Do not expose folder creation, rename, move, or delete in the experimental view.
- Allow UI-local prototype collections if testing multiple membership is useful; they may be
  intentionally ephemeral.
- Remove the existing campaign tree from the normal UI.

The experiment need not perfectly emulate collection editing because the old tree cannot represent
zero-to-many collection membership. Its purpose is to validate navigation, terminology, discovery,
and whether users miss hierarchical folders.

### Questions the experiment should answer

- Can users find a scene or creature faster by type and search than by browsing a tree?
- Are flat, non-nested collections sufficient for adventure and rules organization?
- Does "Current scene" remove the need for much manual organization?
- Is the distinction between removing from a collection and deleting a resource understandable?
- When importing a module, is one automatically created collection a good default?

### Experiment success criteria

- Creating a resource does not require a location decision.
- All resources remain discoverable even if they are in no collection.
- A user can understand that the same resource may appear in multiple collections.
- A user can distinguish catalog deletion from collection removal.
- The UI does not need nested collections for the tested campaign workflows.

## Commands and logs

The core has folder-free resource and collection commands and logs, and the Dioxus UI exposes the
Phase 2 collection-editing operations.

Current collection commands include:

- `CreateCollection`
- `EditCollection`
- `DeleteCollection`
- `MergeCollections`
- `RenameCollection`
- `AddResourcesToCollection`
- `RemoveResourcesFromCollection`

Resource creation commands no longer require a path. Future module work will add `ImportModule`.

The exact command granularity should favor simple validation and deterministic logs. Separate typed
fields should be used for bulk membership operations rather than public heterogeneous resource
lists.

Folder commands and their log variants are not part of the current domain. The unified Worker
migration owns private legacy wire types, expands each old row into zero or more current logs, and
renumbers the resulting rows before normal replay begins.

`ImportModule` is allowed to affect many resources. It is rare and should be transactional. Its
affected resources can be enumerated from the module rather than treated as an unknowable
whole-game mutation.

Rollback is a required storage capability, but it is not a core `Game` command: a materialized
`Game` does not own its historical snapshots or logs. The Worker RPI/storage layer will select a
typed snapshot, replay logs to the requested point, replace current typed rows transactionally,
and immediately checkpoint the restored state as a new immutable snapshot. This makes a rollback
the start of a new linear history rather than a mutation of an old snapshot.

## Persistence plan

### Storage entering Phase 4

Before Phase 3, each game Durable Object:

- keeps a complete `Game` in memory while awake;
- stores a full game JSONB snapshot;
- appends deterministic `GameLog` JSONB values;
- writes another full snapshot after a log threshold;
- reconstructs a cold game from the latest snapshot and subsequent logs.

The locally implemented Phase 3 path now reconstructs cold games from typed current rows and
persists entity deltas transactionally. It still dual-writes periodic monolithic snapshots only as
a temporary verification mechanism.

The main cost is serializing the complete game for each snapshot. Because the Durable Object keeps
the game cached while awake, piecemeal typed-table persistence is primarily valuable for smaller
writes, simpler exports and debugging, and clearer resource-level storage—not for optimizing every
read during normal play.

### Migration sequencing decision

The implementation was developed in stages, but none of those intermediate storage shapes needs to
become a deployed migration boundary. Before deployment, combine the existing catalog-domain and
typed-table migration code with snapshot indexing into one atomic migration.

That migration goes directly from legacy folder-era monolithic snapshots and logs to the final
snapshot-indexed typed schema. It must:

- keep all legacy deserialization and stateful folder interpretation inside Worker migration code;
- transform all snapshots and logs before constructing or replaying a current `Game`;
- create the final composite-key typed tables directly, without first deploying ID-only tables;
- materialize mutable current state only after all historical data has been converted;
- validate the converted representation before advancing the single storage version;
- leave the original storage untouched if any conversion or validation fails.

### Typed current and snapshot schema

Use a separate table for each top-level entity type. Each row stores the entity ID and its complete
Serde representation as a SQLite JSONB blob. Current and historical data use the same tables:
`snapshot_idx = -1` is the mutable current generation, while nonnegative indices are immutable
snapshots.

```sql
CREATE TABLE scenes (
    snapshot_idx INTEGER NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (snapshot_idx, id)
);

CREATE TABLE creatures (
    snapshot_idx INTEGER NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (snapshot_idx, id)
);

CREATE TABLE notes (
    snapshot_idx INTEGER NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (snapshot_idx, id)
);

CREATE TABLE items (
    snapshot_idx INTEGER NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (snapshot_idx, id)
);

CREATE TABLE abilities (
    snapshot_idx INTEGER NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (snapshot_idx, id)
);

CREATE TABLE classes (
    snapshot_idx INTEGER NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (snapshot_idx, id)
);

CREATE TABLE collections (
    snapshot_idx INTEGER NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (snapshot_idx, id)
);

CREATE TABLE players (
    snapshot_idx INTEGER NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (snapshot_idx, id)
);

CREATE TABLE game_state (
    snapshot_idx INTEGER PRIMARY KEY,
    body BLOB NOT NULL
);

CREATE TABLE snapshots (
    snapshot_idx INTEGER PRIMARY KEY CHECK (snapshot_idx >= 0),
    created_at TEXT NOT NULL,
    cause TEXT NOT NULL
);
```

The `game_state` body contains the singleton fields that are not top-level entities:
`current_combat`, `tile_system`, and `active_scene`. Normal cold loads and entity updates always
filter on `snapshot_idx = -1`. The existing `logs` table remains keyed by snapshot and log index.

Creating snapshot `N` is a single transaction that inserts the metadata row and copies the current
rows in each table, for example:

```sql
INSERT INTO scenes (snapshot_idx, id, body)
SELECT ?1, id, body FROM scenes WHERE snapshot_idx = -1;
```

The same copy is performed for each entity table and `game_state`. Snapshot rows are never updated
in place.

Do not initially normalize nested entity fields such as scene contents, creature inventories,
class abilities, or collection membership. For example, editing a collection rewrites one
collection row, merging collections rewrites the destination row and deletes the source row, and
moving a creature can rewrite the affected creature and scene rows.

Benefits of this schema:

- concrete SQL boundaries match the Rust domain types;
- each entity can be inserted, replaced, loaded, or deleted independently;
- current state and snapshots share one schema and one deserialization path;
- snapshots preserve rollback checkpoints without serializing a monolithic `Game`;
- table identity prevents mixing different resource kinds under one discriminator;
- no `campaign_nodes` table or path rewrite operations;
- dumps, debugging, module export, and future type-specific migrations are straightforward;
- nested domain structures remain type-checked by Rust and Serde.

Tradeoffs:

- SQL cannot conveniently query arbitrary nested fields without JSON extraction;
- most referential integrity remains application-level;
- adding a new top-level entity type requires a table and corresponding load/write code;
- each snapshot initially duplicates every entity blob; content-addressing or copy-on-write can be
  considered later if snapshot volume demonstrates a need.

These tradeoffs are acceptable while each Durable Object owns one game and reconstructs the
complete in-memory `Game` on cold load.

### Changed-entity persistence

Keep `GameStorage` as the facade used by the Durable Object and game sessions. Do not add a separate
repository abstraction.

When `GameStorage::update_game` receives a `ChangedGame`, compare the cached `Game` with the changed
value and calculate a top-level delta:

- current (`snapshot_idx = -1`) entity rows to insert or replace in each typed table;
- current entity rows to delete from each typed table;
- whether the current singleton `game_state` row changed.

Persist that delta and append all corresponding logs in one transaction before replacing the
in-memory cached game. This comparison must exhaustively cover every top-level `Game` field so that
adding a new field cannot silently omit persistence. Module import uses the same path and may
change many rows in one transaction.

This deliberately derives persistence from the before-and-after materialized states rather than
maintaining a second mapping from every `GameLog` variant to affected rows. Logs remain the
deterministic history of the change, but they are not the source of truth for choosing SQL rows.

### Unified storage migration

Treat the existing catalog-domain and typed-table migrations as unpublished implementation pieces,
not as sequential deployed versions. Replace them with one storage-version transition for each
game Durable Object:

1. Create the final typed entity tables with composite `(snapshot_idx, id)` keys, plus versioned
   `game_state` and snapshot metadata.
2. Read every legacy monolithic snapshot into migration-only wire types, convert its folder-era
   domain values, and write its resources and singleton state under the original nonnegative
   snapshot index.
3. Convert and renumber every legacy log into the current folder-free log representation while
   preserving its snapshot association.
4. Validate all converted typed snapshots and logs. Do not construct or replay a core `Game` until
   this complete historical conversion has succeeded.
5. Construct the authoritative current `Game` from the latest converted typed snapshot and its
   subsequent converted logs.
6. Write that materialized current state under `snapshot_idx = -1`.
7. Validate typed historical reconstruction, current-state parity, and collection/reference
   integrity.
8. Remove the legacy monolithic snapshot table and advance the single storage version only when
   the entire transaction succeeds.

After migration, cold loads use only current `-1` rows, snapshot creation uses transactional
typed-row copies, and logs remain the history after each immutable baseline. There is no deployed
ID-only typed-table version, no deployed dual-write period, and no follow-up snapshot-index
migration.

The Durable Object per game makes this migration naturally incremental: each game migrates when
its object next wakes.

### Dump and recovery

Storage dumps should include:

- schema and migration version;
- every typed entity table with snapshot indices, IDs, and decoded JSON bodies;
- current and historical singleton game-state rows;
- snapshot metadata;
- logs.

A recovery test should prove that a dump can reconstruct a `Game` identical to the in-memory value.

## Domain migration

This conversion is an internal stage of the unified storage migration, not its own deployed storage
version.

The folder-to-collection migration should:

1. Assign stable IDs to all notes.
2. Move notes into the top-level note map.
3. Create one collection for each non-empty folder.
4. Use the folder's full path as the initial collection name to avoid ambiguity.
5. Preserve each folder's typed resource membership.
6. Choose a deterministic initial order because current folder membership uses unordered sets.
   The implemented migration sorts typed IDs and sorts notes by name and ID.
7. Drop empty folders unless retaining one has demonstrated user value.
8. Translate `/Players/{player_id}` note placement into explicit ownership and visibility metadata.
9. Validate that all catalog resources still exist and all collection references resolve.
10. Rewrite every stored log into current folder-free logs before normal replay.
11. Keep the compatibility reader and stateful folder interpreter private to the Worker migration.

Because the old validation requires every resource to appear in exactly one folder, normal games
should migrate without orphaned or multiply assigned resources. Migration code must still detect
and report malformed legacy data rather than assume it is valid.

## Testing strategy

### Collection tests

- A resource may be in no collections.
- A resource may be in multiple collections.
- Duplicate IDs in one typed vector are rejected or normalized.
- Membership survives serialization and persistence.
- Removing membership does not delete the resource.
- Deleting a resource removes every stale collection reference.
- Deleting a collection preserves all resources.
- Merging collections deduplicates membership and preserves all resources.

### Migration tests

- A representative folder game converts to the expected catalog and collections.
- Nested folder paths become non-nested collections with unambiguous names.
- Notes receive stable IDs and preserve content.
- Player notes preserve authorization semantics.
- Malformed folder games fail with useful errors.

### Module tests

- Export includes required dependencies.
- Export deduplicates shared and cyclic dependencies.
- Optional links are reported correctly.
- Import remaps every ID and internal reference.
- Importing the same module twice succeeds.
- Imported resources are placed in the generated collection.
- Export followed by import produces equivalent resources apart from remapped IDs.
- Missing required dependencies fail before mutation.

### Persistence tests

- Before-and-after comparison reports every changed and deleted entity row.
- The top-level comparison is exhaustive over all `Game` fields.
- Full-snapshot and typed-table loads produce identical `Game` values.
- Current writes only affect `snapshot_idx = -1` and cannot mutate historical rows.
- Creating a typed snapshot reproduces the complete current `Game`.
- The single migration expands every monolithic snapshot and converts every log before constructing
  a current `Game`.
- No intermediate storage version is committed if any part of conversion or validation fails.
- Rollback works within one snapshot, across snapshot boundaries, and after a previous rollback.
- A rollback immediately produces a new immutable baseline and subsequent logs attach to it.
- A failed multi-resource update does not partially persist.
- Module import is atomic.
- Cold load handles zero resources and zero collections.
- Dumps reconstruct both current state and historical snapshots.
- Local predeployment dual-write divergence is detected.

## Risks and safeguards

### Users may recreate folder hierarchies in collection names

Allow names such as "Rules / Classes" during migration, but do not add parent IDs or nested
collection behavior. Search and type views should make deep manual taxonomy unnecessary.

### Module boundaries may remain ambiguous

Keep explicit roots in the module manifest and distinguish them from dependency closure. Always
preview what will be included.

### Stateful creatures and reusable templates may diverge

Do not solve creature templates in this project. Modules initially copy complete creature values.
A later template/instance distinction can build on module provenance if needed.

### Multiple collection membership was not representable during the UI prototype

The final domain and Phase 2 UI now support multiple membership.

### JSON entity bodies may become limiting

Only normalize entity fields or add indexed columns in response to demonstrated query or integrity
requirements. Keeping one table per entity type allows those changes to be introduced for one type
without redesigning storage for every other type.

### Old logs require stateful conversion

Keep the frozen folder-era wire types and tree interpreter isolated in the Worker migration. The
migration must rewrite both tables transactionally and fail without advancing the storage version
if any snapshot or log cannot be converted. Core Arpeggio must never replay a folder-era log.

### Typed snapshots duplicate entity blobs

The initial design favors simple, transactional checkpoints over storage deduplication. Monitor
snapshot size and count; introduce retention, content-addressed bodies, or copy-on-write only if
real game data makes duplication material.

## Open design questions

These do not block the next phase:

- Should the automatically created import collection contain all imported resources or only the
  module's explicit roots? The initial recommendation is all imported resources.
- Which scene references are contextual contents versus optional links?
- Should imported provenance be visible in the first catalog UI or merely stored?
- When should a read-only installed module be introduced in addition to copy-based import?
- What snapshot retention policy, if any, is needed after typed rollback is deployed?

## Completion criteria

This initiative is complete when:

- folders are no longer the normal user-facing resource paradigm;
- resources can exist without collection membership;
- resources can appear in multiple non-nested collections;
- creating a resource requires no path;
- modules import and export with validated dependency closure;
- old folder-based games have a supported migration path;
- the Rust/Dioxus UI implements the catalog and collections experience;
- the TypeScript UI remains intentionally untouched;
- typed-table persistence can reconstruct a `Game` exactly and is authoritative;
- immutable typed snapshots and logs can restore any supported rollback point without a monolithic
  game blob;
- current state remains writable without mutating historical snapshots;
- new monolithic snapshots are no longer written;
- obsolete folder persistence and UI code can be safely removed.
