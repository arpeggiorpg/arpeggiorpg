set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

default:
    just --list

run-ui:
    cd ui; npm run dev

run-worker:
    cd worker; npm run dev

deploy-ui:
    cd ui; npm run build
    cd ui; ./node_modules/.bin/wrangler pages deployment create --env production ./dist/

deploy-backend:
    cd worker;  ./node_modules/.bin/wrangler deploy

create-schema-local:
    cd worker; ./node_modules/.bin/wrangler d1 execute DB --local --file=./schema.sql

create-schema-production:
    cd worker; ./node_modules/.bin/wrangler d1 execute DB --file=./schema.sql
