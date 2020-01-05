# P&T

P&T is a table-top role-playing game. Think of a RPG like Dungeons & Dragons, but instead of needing
books and character sheets, the app takes care of everything for you. It provides a tactical combat
map and makes it easy for players and the Game Master to use their abilities and skills.

# Status: Early development. Not a game yet.

# License

MIT-licensed: http://opensource.org/licenses/MIT


# Building/running (for dev/test)

To start the backend (this defaults to serving on all network interfaces on port 1337):

WARNING: the "--saved-games" argument specifies a directory that users of the web endpoint will
be able to write arbitrary files to.

```shell
cd ptrpi; cargo run -- --saved-games sample_games --init testgame.yaml
```

This will start an RPI server which will load & save games to the `sample_games` directory, and
automatically load up the `testgame.yaml` file as the initial state.

To build the UI, you must have npm installed (ideally npm 5).

```
cd ptui;
npm install;
./node_modules/.bin/webpack --config ./webpack.vendor.js
./node_modules/.bin/webpack --env.rpi_url=http://localhost:1337 # NOTE: do NOT put a / at the end of this URL
```

That will generate the UI in the `ptui/build` directory -- now host that on a static web server and
hit the "GM.html" or "Player.html" files in it.
