set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

default:
    just --list

standalone-ui:
    cd arpui; cp index.standalone.html index.html
    cd arpui; dx serve --bin arpui

standalone-server:
    cargo run -p arpeggio-server -- --data-dir ./arpeggio-data

test:
    cargo test --workspace

check:
    cargo check --workspace
    cargo check --workspace --exclude arpeggio-server --target wasm32-unknown-unknown
    cd arpui; cargo check --target wasm32-unknown-unknown --bin arpui
