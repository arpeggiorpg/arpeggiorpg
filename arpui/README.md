# Dioxus UI development

From the repository root, run the standalone application with:

```bash
just standalone-ui
```

It reads the Arpeggio server URL from the `RPI_URL` meta tag in
`index.standalone.html`. The UI and server may use different origins.

Check the standalone WASM application with:

```shell
cargo check --target wasm32-unknown-unknown --bin arpui
```
