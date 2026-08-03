<p align="center"><img width="500" src="https://github.com/arpeggiorpg/arpeggiorpg/assets/227068/622c375c-3f34-4373-8b76-65eb98937837"></p>

# ArpeggioRPG

ArpeggioRPG is a TTRPG (tabletop roleplaying game). Think of a game like Dungeons & Dragons or
Pathfinder, but instead of needing books and character sheets, the app takes care of everything for
you. It provides a tools for Game Masters to create and organize their campaigns, and a tactical
combat map that makes it easy for players and the Game Master to use their abilities and skills.

# Status: Early development. Not a game yet.

# License

ArpeggioRPG is available under the [MIT License](LICENSE).

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
`arpui/index.standalone.html`. The server stores the game and uploaded images
beneath `./arpeggio-data`.

Run the Rust tests and WASM checks with:

```shell
just test
just check
```
