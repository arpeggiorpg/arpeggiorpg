# ArpeggioRPG

ArpeggioRPG is a table-top role-playing game. Think of a RPG like Dungeons & Dragons, but instead of
needing books and character sheets, the app takes care of everything for you. It provides a tools
for Game Masters to create and organize their campaigns, and a tactical combat map that makes it
easy for players and the Game Master to use their abilities and skills.

# Status: Early development. Not a game yet.

# License

MIT-licensed: http://opensource.org/licenses/MIT


# Building/running (for dev/test)

To start the backend (this defaults to serving on all network interfaces on port 1337):

WARNING: the "--saved-games" argument specifies a directory that users of the web endpoint will
be able to write arbitrary files to (for now).

```shell
cd ptrpi
cargo run -- serve --storage ./storage/
```

This will start an RPI server which will load & save games to the `sample_games` directory, and
automatically load up the `testgame.yaml` file as the initial state.

To build the UI, you must have npm installed (ideally npm >=5).

```shell
cd ui
npm install
npm run dev
```

That will start the frontend server. Hit it at http://localhost:5173/
