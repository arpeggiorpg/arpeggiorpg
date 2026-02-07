<p align="center"><img width="500" src="https://github.com/arpeggiorpg/arpeggiorpg/assets/227068/622c375c-3f34-4373-8b76-65eb98937837"></p>

# ArpeggioRPG

ArpeggioRPG is a TTRPG (tabletop roleplaying game). Think of a game like Dungeons & Dragons or
Pathfinder, but instead of needing books and character sheets, the app takes care of everything for
you. It provides a tools for Game Masters to create and organize their campaigns, and a tactical
combat map that makes it easy for players and the Game Master to use their abilities and skills.

# Status: Early development. Not a game yet.

# License

MIT-licensed: http://opensource.org/licenses/MIT

# Building/running (for dev/test)

## First-time setup

First, create worker/.dev.vars with:

```
GOOGLE_CLIENT_SECRET=xyz
GOOGLE_CLIENT_ID=abc (only if you're not me)
FRONTEND_URL=http://127.0.0.1:8080/
```

Then run

```shell
just create-schema-local
```

## Running

To start the backend, which is implemented as a CloudFlare worker written in Rust:

```shell
just worker
```

Then start the frontend:

```shell
just dxui
```

Hit it at http://localhost:5173/
