set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

dioxus_bundle_dir := `cd arpui && cargo metadata --format-version 1 --no-deps | jq -r '.target_directory + "/dx/arpui/release/web/public"'`

default:
    just --list

legacy-ui:
    cd ui; npm run dev

ui:
    cd arpui; cp index.dev.html index.html
    cd arpui; dx serve

# This "ARP_LOCAL_DEV" is used in wrangler.toml (actually, worker/build.js)
worker $ARP_LOCAL_DEV="--dev":
    cd worker; npm run dev

gen-ts:
    cd arptypes; cargo run --bin gen-ts

deploy-ui:
    cd ui; npm run build
    cd ui; ./node_modules/.bin/wrangler pages deployment create --env production ./dist/

deploy-dioxus branch="dioxus":
    cd arpui; cp index.prod.html index.html
    # debug-symbols=false is a workaround for some DWARF error from dx. hopefully this can be removed after upgrading
    cd arpui; dx build --release --debug-symbols=false
    cd arpui; ../worker/node_modules/.bin/wrangler pages deploy "{{ dioxus_bundle_dir }}" --project-name arpeggio --branch {{ branch }} --commit-dirty=true

deploy-dioxus-preprod:
    cd arpui; cp index.preprod.html index.html
    # debug-symbols=false is a workaround for some DWARF error from dx. hopefully this can be removed after upgrading
    cd arpui; dx build --release --debug-symbols=false
    cd arpui; ../worker/node_modules/.bin/wrangler pages deploy "{{ dioxus_bundle_dir }}" --project-name arpeggio --branch preprod --commit-dirty=true

deploy-backend:
    just deploy-to-production

deploy-to-preprod:
    cd worker; ./node_modules/.bin/wrangler deploy --env preprod

deploy-to-production:
    cd worker; ./node_modules/.bin/wrangler deploy --env=""

create-schema-local:
    cd worker; ./node_modules/.bin/wrangler d1 execute DB --local --file=./schema.sql

create-schema-production:
    cd worker; ./node_modules/.bin/wrangler d1 execute DB --env="" --remote --file=./schema.sql

create-schema-preprod:
    cd worker; ./node_modules/.bin/wrangler d1 execute DB --env preprod --remote --file=./schema.sql

worker-tests:
    curl http://localhost:8787/test
