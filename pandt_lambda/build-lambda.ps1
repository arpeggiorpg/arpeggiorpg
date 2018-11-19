# for now this needs to be run in the same directory
mkdir -Force lambda-target
docker build --force-rm -t pandt-lambda:latest --build-arg SRC=. -f .\docker\lambda-builder.dockerfile .
docker run -v "$pwd/lambda-target:/export" --rm pandt-lambda:latest
