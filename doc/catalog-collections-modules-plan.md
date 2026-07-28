# Catalog, Collections, Modules, and Storage Plan

## Status

In progress. Phase 0 is complete and deployed to preprod. Phase 1, the domain-model change, is
next. Module and storage work has not started; a few eventual Phase 2 UI outcomes were pulled
forward into the prototype.

This is the canonical plan for replacing campaign folders with a resource catalog and collections,
for importing and exporting modules, and for eventually moving from whole-game snapshots to
granular blob persistence.

It supersedes `doc/piecemeal sql storage.md`. The useful parts of that plan are incorporated here,
but persistence work is deliberately sequenced after a UI experiment and the resource-model
change.

### Progress update — 2026-07-28

Completed in the Rust/Dioxus UI:

- Replaced the normal Campaign tree with a Catalog as the default GM view.
- Added Current Scene, session-local Recent, resource-type, search, collection overview, and
  individual collection views.
- Projected existing folder paths as temporary read-only collections.
- Added one location-free create flow for scenes, creatures, notes, classes, abilities, and items.
  For compatibility with the current commands and domain model, it creates resources under the
  hidden default path `/catalog`.
- Flattened Manage Creatures, preserved search and grant/remove behavior, and placed creatures
  assigned when the dialog opens at the top of the list.
- Added existing creature icons and class emojis to catalog rows.
- Removed the legacy folder-tree UI instead of retaining it behind a development switch.
- Left the TypeScript UI untouched.
- Deployed the current prototype to `https://preprod.arpeggio-331.pages.dev`.

Not yet implemented:

- First-class `Collection` and top-level `Note` domain types.
- Collection editing, multiple membership, manual ordering, or catalog deletion.
- Location-free resource commands; the current UI adapts existing folder-based commands.
- The new module format, dependency-aware import/export, and module provenance.
- Repository abstraction, granular persistence, migration, or dual-write work.

Current decisions and prototype constraints:

- `/catalog` is an implementation detail of the compatibility layer, not a user-facing location.
- Legacy folder-based module imports do not need to remain available. There are no known module
  files in active use, and the format will be replaced during Phase 5.
- Existing folder-based games still require a migration path when the domain model changes.

## Implementation phases

### Phase 0: UI prototype

- [x] Add catalog navigation to the Rust/Dioxus GM UI.
- [x] Add type views and search.
- [x] Add Current Scene and Recent where practical.
- [x] Present folder paths as temporary read-only collections.
- [x] Remove the folder tree from the normal UI.
- [x] Add a location-free prototype create flow backed by `/catalog`.
- [x] Gather experience before changing serialized state.
- [x] Do not touch the TypeScript UI or generated TypeScript.

### Phase 1: Domain model

- Add `CollectionID` and `NoteID`.
- Make notes top-level resources.
- Add `Collection` with separate typed vectors.
- Add `Game.collections`.
- Add collection validation and helper methods.
- Continue storing whole-game snapshots.
- Add legacy `Game` deserialization or an explicit migration adapter.

### Phase 2: Commands and Dioxus UI

- Add collection commands and logs.
- Remove folder selection from new-resource flows.
- Support add, remove, reorder, create, rename, and delete collection operations.
- Make catalog deletion explicitly different from collection removal.
- [x] Replace the campaign tree as the normal UI (completed early during Phase 0).
- Keep folder commands only for legacy replay and migration.

### Phase 3: Repository and granular blobs

- Add the repository boundary.
- Implement it first with current snapshot storage.
- Add generic resource, collection, and game-state blob tables.
- Implement and test affected-entity projection.
- Backfill and dual-write.
- Add full parity and recovery tests.
- Switch cold loading to granular blobs.

### Phase 4: Cleanup

- Stop writing full snapshots when recovery and rollback permit it.
- Remove `campaign` and normal-use folder code.
- [x] Remove folder UI (completed early during Phase 0).
- Remove legacy commands and logs only after old replay is no longer needed.
- Consider removing the standalone `foldertree` crate if no other code uses it.

### Phase 5: Modules

- Define the versioned typed module format.
- Implement reference discovery and dependency classification.
- Implement export preview and validation.
- Implement ID remapping and transactional import.
- Automatically create a collection for imported content.
- Add round-trip and dependency-closure tests.

## Decision summary

Arpeggio will move toward three separate concepts:

1. **Catalog**: all resources owned by a game, browsable primarily by resource type and search.
2. **Collection**: an optional, non-nested, ordered grouping of resource IDs.
3. **Module**: a portable set of complete resources that can be imported into or exported from a
   game.

These concepts replace the folder tree's current combination of navigation, ownership, permission
scoping, and module boundaries.

The intended implementation order is:

1. Experiment with the catalog and collection experience entirely in the Rust/Dioxus UI.
2. Add collections and top-level notes to the Rust domain model while retaining whole-game snapshot
   blobs.
3. Replace folder-based commands with collection-oriented commands and complete the Dioxus UI.
4. Introduce granular persistence using generic JSON blob rows, then remove remaining normal-use
   folder code.
5. Implement dependency-aware module import and export against the final catalog and collection
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
- Make later granular persistence simpler than the proposed campaign adjacency-list design.
- Preserve deterministic game logs and a safe migration path for existing games.

## Non-goals

- Updating or reviving the old TypeScript UI.
- Updating generated TypeScript declarations solely for this work.
- Building a public module registry or marketplace.
- Implementing automatic updates for imported modules in the first version.
- Designing a full package manager with dependency versions and overrides.
- Making collections hierarchical.
- Normalizing every resource field into relational SQL columns.
- Implementing true rollback semantics as part of the first persistence change.

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
- Vector order is user-visible and should be preserved.
- Collections cannot contain collections.
- Removing a resource from a collection does not delete the resource.
- Deleting a collection does not delete its resources.
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

`campaign: FolderTree<Folder>` is removed after migration and compatibility work is complete.

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
- Allow UI-local prototype collections if testing multiple membership or ordering is useful; they
  may be intentionally ephemeral.
- Remove the existing campaign tree from the normal UI. Backend folder compatibility remains until
  the domain and storage migrations are complete.

The experiment need not perfectly emulate collection editing because the old tree cannot represent
zero-to-many collection membership. Its purpose is to validate navigation, terminology, discovery,
and whether users miss hierarchical folders.

### Questions the experiment should answer

- Can users find a scene or creature faster by type and search than by browsing a tree?
- Are flat, non-nested collections sufficient for adventure and rules organization?
- Does "Current scene" remove the need for much manual organization?
- Is the distinction between removing from a collection and deleting a resource understandable?
- Should collection ordering be manual everywhere or only in selected views?
- When importing a module, is one automatically created collection a good default?

### Experiment success criteria

- Creating a resource does not require a location decision.
- All resources remain discoverable even if they are in no collection.
- A user can understand that the same resource may appear in multiple collections.
- A user can distinguish catalog deletion from collection removal.
- The UI does not need nested collections for the tested campaign workflows.

## Commands and logs

After the UI model is accepted, introduce collection-oriented commands and logs.

Likely commands include:

- `CreateCollection`
- `EditCollection`
- `DeleteCollection`
- `AddResourcesToCollection`
- `RemoveResourcesFromCollection`
- `ReorderCollectionResources`
- `ImportModule`
- resource creation commands without `FolderPath`

The exact command granularity should favor simple validation and deterministic logs. Separate typed
fields should be used for bulk membership operations rather than public heterogeneous resource
lists.

Folder commands and their log variants must remain deserializable while old snapshots and logs can
still be encountered. They can be marked legacy and removed only after the storage migration makes
replay from folder-era logs unnecessary.

`ImportModule` is allowed to affect many resources. It is rare and should be transactional. Its
affected resources can be enumerated from the module rather than treated as an unknowable
whole-game mutation.

Rollback is currently not a strong enough semantic guarantee to drive this design. Keep the
existing logs and snapshots until rollback behavior is specified separately.

## Persistence plan

### Current storage

Today, each game Durable Object:

- keeps a complete `Game` in memory while awake;
- stores a full game JSONB snapshot;
- appends deterministic `GameLog` JSONB values;
- writes another full snapshot after a log threshold;
- reconstructs a cold game from the latest snapshot and subsequent logs.

The main cost is serializing the complete game for each snapshot. Because the Durable Object keeps
the game cached while awake, granular persistence is primarily valuable for smaller writes, simpler
exports and debugging, and clearer resource-level storage—not for optimizing every read during
normal play.

### Persistence sequencing decision

Do not implement piecemeal SQL storage before the catalog and collection model is accepted.

During the UI experiment and the initial domain migration:

- keep `game_snapshots` unchanged;
- keep whole-`Game` JSONB blobs;
- keep log append and replay unchanged where possible;
- take a new full snapshot after migrating a game to the new domain shape;
- prefer compatibility adapters over a simultaneous storage rewrite.

This isolates product-model risk from storage-migration risk.

### Repository boundary

Before switching authority away from full snapshots, introduce a repository boundary around durable
storage. It should support:

- loading materialized game state;
- upserting and deleting a resource blob by kind and ID;
- loading all resource blobs for a game;
- loading and storing game-level state;
- loading and storing collection blobs;
- appending logs;
- transactional import of a module;
- retaining a whole-snapshot implementation as a migration adapter.

The first interface should reflect actual call sites rather than attempt to define a repository
method for every future resource operation.

### Granular blob schema

Once the domain shape is stable, prefer generic JSON blob rows:

```sql
CREATE TABLE resources (
    kind TEXT NOT NULL,
    id TEXT NOT NULL,
    body BLOB NOT NULL,
    PRIMARY KEY (kind, id)
);

CREATE TABLE collections (
    id TEXT PRIMARY KEY,
    body BLOB NOT NULL
);

CREATE TABLE game_state (
    key TEXT PRIMARY KEY,
    body BLOB NOT NULL
);
```

The existing `logs` table remains.

Candidate `resources.kind` values are:

- `scene`
- `creature`
- `note`
- `item`
- `ability`
- `class`
- `player`, if players are later treated through the same repository

Combat, tile system, active scene, and other singleton values may live in one `game_state` blob or
separate keyed blobs. Choose whichever makes affected-state tracking clearer; do not normalize
their internal fields prematurely.

Collections remain typed Rust values serialized as one blob per collection. Collection membership
does not require a join table initially. Reordering or editing a collection rewrites that small
collection blob.

Benefits of this schema:

- one persistence path for all catalog resources;
- granular writes without per-type schema migrations;
- no `campaign_nodes` table;
- no path or descendant rewrite operations;
- easy module dump and import;
- resource type safety remains in Rust and Serde;
- typed tables can still be introduced later if real query requirements appear.

Tradeoffs:

- SQL cannot conveniently query arbitrary typed resource fields without JSON extraction;
- corrupt or mismatched `kind` and `body` values are detected during deserialization rather than by
  relational constraints;
- application-level reference validation remains necessary.

These tradeoffs are acceptable while each Durable Object owns one game and normally reconstructs
the complete in-memory game on cold load.

### Affected entities

Add an internal affected-entity projection for logs after the new domain commands stabilize.

It should identify:

- resource blobs to upsert;
- resource blobs to delete;
- collection blobs to upsert or delete;
- game-state blobs to rewrite;
- bulk module imports.

Most current logs already identify a creature, scene, item, class, ability, player, combat, or
game-level field. Collection logs will identify their collection directly. Module import can
enumerate all contained IDs.

This projection must be exhaustively tested. It is a materialized-state projection and must not
silently omit a new log variant.

Per-entity `apply_log` methods may still be a useful later refactor, but they are not required
before granular blob persistence. Start with the explicit affected-entity mapping and evolve only
if the duplication becomes costly.

### Granular persistence migration

For each game Durable Object:

1. Add the new blob tables alongside `game_snapshots`.
2. On first migration load, read the latest authoritative snapshot and replay its remaining logs.
3. Convert any remaining folder model to catalog resources and collections.
4. Backfill `resources`, `collections`, and `game_state` in one transaction.
5. Mark the granular representation with a schema or migration version.
6. During a verification period, dual-write granular blobs and normal full snapshots.
7. Add parity tests that load both representations and compare complete `Game` values.
8. Switch cold load to granular blobs after parity is established.
9. Continue keeping logs for history and debugging.
10. Stop writing new full snapshots only after rollback and recovery requirements are settled.
11. Drop old snapshots, folder compatibility code, and legacy log variants only in a later,
    explicitly irreversible migration.

The Durable Object per game makes migration naturally incremental: each game can migrate when its
object next wakes.

### Dump and recovery

Storage dumps should include:

- schema and migration version;
- all resource rows with `kind`, `id`, and decoded JSON body;
- collection blobs;
- game-state blobs;
- logs;
- any retained legacy snapshots during migration.

A recovery test should prove that a dump can reconstruct a `Game` identical to the in-memory value.

## Domain migration

The folder-to-collection migration should:

1. Assign stable IDs to all notes.
2. Move notes into the top-level note map.
3. Create one collection for each non-empty folder.
4. Use the folder's full path as the initial collection name to avoid ambiguity.
5. Preserve each folder's typed resource membership.
6. Choose a deterministic initial order because current folder membership uses unordered sets.
   Sorting by display name is a reasonable default.
7. Drop empty folders unless retaining one has demonstrated user value.
8. Translate `/Players/{player_id}` note placement into explicit ownership and visibility metadata.
9. Validate that all catalog resources still exist and all collection references resolve.
10. Retain a compatibility reader for old serialized games.

Because the old validation requires every resource to appear in exactly one folder, normal games
should migrate without orphaned or multiply assigned resources. Migration code must still detect
and report malformed legacy data rather than assume it is valid.

## Testing strategy

### Collection tests

- A resource may be in no collections.
- A resource may be in multiple collections.
- Duplicate IDs in one typed vector are rejected or normalized.
- Order survives serialization and persistence.
- Removing membership does not delete the resource.
- Deleting a resource removes every stale collection reference.
- Deleting a collection preserves all resources.

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

- Every log variant reports the correct affected blobs.
- Full-snapshot and granular-blob loads produce identical `Game` values.
- A failed multi-resource update does not partially persist.
- Module import is atomic.
- Cold load handles zero resources and zero collections.
- Dumps reconstruct the complete game.
- Dual-write divergence is detected.

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

### Multiple collection membership is not representable during the UI prototype

Treat prototype collections as read-only folder projections or ephemeral UI state. Do not distort
the final model to fit the compatibility layer.

### Generic blobs may become limiting

Only introduce typed SQL tables or indexed columns in response to demonstrated queries. The
repository boundary should allow that change without changing the domain or UI model.

### Old logs may require folder-era types

Keep legacy enum variants and compatibility types until the migration no longer relies on replaying
them. Avoid an early cleanup that makes old games unrecoverable.

## Open design questions

These do not block the UI experiment:

- What explicit ownership and visibility fields should notes have?
- Should all collections be manually ordered, or should some sort automatically by name?
- Should the automatically created import collection contain all imported resources or only the
  module's explicit roots? The initial recommendation is all imported resources.
- Which scene references are contextual contents versus optional links?
- Should imported provenance be visible in the first catalog UI or merely stored?
- When should a read-only installed module be introduced in addition to copy-based import?
- What rollback guarantee is actually desired once full snapshots stop being authoritative?

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
- granular blob persistence can reconstruct a `Game` exactly and is authoritative;
- obsolete folder persistence and UI code can be safely removed.
