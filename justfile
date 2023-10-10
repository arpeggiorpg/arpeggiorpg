set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

build-docker:
    docker build . -t us-east1-docker.pkg.dev/pandt-400420/ptrpi/ptrpi:latest

push-docker:
    gcloud auth print-access-token | docker login -u oauth2accesstoken --password-stdin https://us-east1-docker.pkg.dev
    docker push us-east1-docker.pkg.dev/pandt-400420/ptrpi/ptrpi:latest

upload-ui:
    cd ui; npm run build
    wrangler do something
