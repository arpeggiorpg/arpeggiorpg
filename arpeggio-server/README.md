# Arpeggio server

`arpeggio-server` runs one persistent game. There's no authentication in this
server; it's for private use or development.

```bash
cargo run -p arpeggio-server -- --data-dir ./arpeggio-data
```

The standalone Dioxus UI connects to the configured server URL and uses:

- `/ws/GM` for the GM view;
- `/ws/Player/{name}` for an explicitly registered player;
- `/api/images/{id}` and `/images/{id}` for native image storage.

The UI may be served from a separate origin. Pass `--allowed-origin` once with a comma-separated
list of permitted origins. Pass `--ui-dir` to optionally serve a compiled Dioxus bundle from this
server.
