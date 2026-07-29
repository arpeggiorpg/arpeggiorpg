<p align="center"><img width="500" src="https://github.com/arpeggiorpg/arpeggiorpg/assets/227068/622c375c-3f34-4373-8b76-65eb98937837"></p>

# ArpeggioRPG

ArpeggioRPG is a TTRPG (tabletop roleplaying game). Think of a game like Dungeons & Dragons or
Pathfinder, but instead of needing books and character sheets, the app takes care of everything for
you. It provides a tools for Game Masters to create and organize their campaigns, and a tactical
combat map that makes it easy for players and the Game Master to use their abilities and skills.

# Status: Early development. Not a game yet.

# License

MIT-licensed: http://opensource.org/licenses/MIT

# Building and running the standalone game

Run the native single-game server:

```shell
just standalone-server
```

In another terminal, run the standalone Dioxus UI:

```shell
just standalone-ui
```

The UI reads its independently configured server URL from
`arpui/index.standalone.html`. The server stores the game and uploaded images beneath
`./arpeggio-data` and binds to loopback by default.

Run the public Rust tests and WASM checks with:

```shell
cargo test
cargo check --workspace --exclude arpeggio-server --target wasm32-unknown-unknown
(cd arpui && cargo check --target wasm32-unknown-unknown --bin arpui)
```

## Hosted development

The authenticated Cloudflare runtime remains available while the hosted repository split is in
progress. Create `worker/.dev.vars` with the required Google and frontend configuration, initialize
the local D1 schema with `just create-schema-local`, and then run:

```shell
just worker
just ui
```
