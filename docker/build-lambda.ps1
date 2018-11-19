# for now this needs to be run in the root directory
mkdir -Force lambda-target
docker build --force-rm -t pandt-lambda:latest -f .\docker\lambda-builder.dockerfile .
docker run -v "$pwd/lambda-target:/export" --rm pandt-lambda:latest
