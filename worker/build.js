// this only exists as a way to run a command using an environment variable in a way that works
// whether you're on Windows or *nix.

const { spawnSync } = require("child_process");
const devmode = process.env.ARP_LOCAL_DEV;

run("cargo", ["install", "-q", "worker-build"]);
run("worker-build", devmode ? [devmode] : []);


function run(f, args) {
    const result = spawnSync(f, args, {stdio: 'inherit'});
    if (result.status !== 0) process.exit(result.status);
}

