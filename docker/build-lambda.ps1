# for now this needs to be run in the root directory
mkdir -Force lambda-target
docker build --force-rm -t pandt-lambda:latest -f .\docker\lambda-builder.dockerfile .
# rm -Force CIDFILE
docker run --cidfile CIDFILE pandt-lambda:latest
$container_id = get-content CIDFILE
rm -Force CIDFILE
docker cp "${container_id}:/home/rust/output/pandt_lambda" lambda-target/
