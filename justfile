set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

default:
    just --list

deploy-ui:
    cd ui; npm run build
    cd ui; ./node_modules/.bin/wrangler pages deployment create --env production ./dist/

deploy-backend:
    cd worker;  ./node_modules/.bin/wrangler deploy
