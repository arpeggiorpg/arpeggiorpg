# for now this needs to be run in the root directory

function CheckLastExitCode {
    param ([int[]]$SuccessCodes = @(0), [scriptblock]$CleanupScript=$null)

    if ($SuccessCodes -notcontains $LastExitCode) {
        if ($CleanupScript) {
            "Executing cleanup script: $CleanupScript"
            &$CleanupScript
        }
        $msg = @"
EXE RETURNED EXIT CODE $LastExitCode
CALLSTACK:$(Get-PSCallStack | Out-String)
"@
        throw $msg
    }
}

mkdir -Force lambda-target
mkdir -Force docker/cache/cargo_git
mkdir -Force docker/cache/cargo_registry
mkdir -Force docker/cache/cargo_target
Remove-Item CIDFILE -ErrorAction Ignore

docker build --force-rm -t pandt-lambda:latest -f .\docker\lambda-builder.dockerfile .
docker run -it `
    -v "$pwd/docker/cache/cargo_git:/home/rust/.cargo/git" `
    -v "$pwd/docker/cache/cargo_registry:/home/rust/.cargo/registry" `
    -v "$pwd/docker/cache/cargo_target:/home/rust/src/target" `
    --cidfile CIDFILE pandt-lambda:latest `
    bash
CheckLastExitCode

$container_id = get-content CIDFILE
rm -Force CIDFILE

cp ./docker/cache/cargo_target/x86_64-unknown-linux-musl/release/pandt_lambda lambda-target/
