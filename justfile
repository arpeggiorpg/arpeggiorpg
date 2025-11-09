set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

default:
    just --list

ui:
    cd ui; npm run dev

dxui:
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

deploy-dioxus:
    cd arpui; cp index.prod.html index.html
    cd arpui; dx build --release
    cd arpui; ../ui/node_modules/.bin/wrangler pages deploy ./dist --project-name arpeggio

deploy-backend:
    cd worker;  ./node_modules/.bin/wrangler deploy

create-schema-local:
    cd worker; ./node_modules/.bin/wrangler d1 execute DB --local --file=./schema.sql

create-schema-production:
    cd worker; ./node_modules/.bin/wrangler d1 execute DB --file=./schema.sql
