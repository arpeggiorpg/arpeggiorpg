set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]


# TODO: rename these google cloud registries from ptrpi to rpi, or something
# build-docker:
#     docker build . -t us-east1-docker.pkg.dev/pandt-400420/ptrpi/ptrpi:latest
#
# push-docker:
#     gcloud auth print-access-token | docker login -u oauth2accesstoken --password-stdin https://us-east1-docker.pkg.dev
#     docker push us-east1-docker.pkg.dev/pandt-400420/ptrpi/ptrpi:latest

deploy-ui:
    cd ui; npm run build
    cd ui; ./node_modules/.bin/wrangler pages deployment create --env production ./dist/

deploy-backend:
    cd worker;  ./node_modules/.bin/wrangler deploy
