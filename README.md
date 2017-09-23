# P&T

P&T is a table-top role-playing game. Think of a RPG like Dungeons & Dragons, but instead of needing
books and character sheets, the app takes care of everything for you. It provides a tactical combat
map and makes it easy for players and the Game Master to use their abilities and skills.

# Status: Early development. Not a game yet.

# License

MIT-licensed: http://opensource.org/licenses/MIT


# Building/running (for dev/test)

To start the backend (this defaults to serving on all network interfaces on port 1337):

```shell
cd ptrpi; cargo run -- saved_games
```

To build the UI, you must have npm installed (ideally npm 5).

```
cd ptui;
  npm install;
  ./node_modules/.bin/webpack --config ./webpack.vendor.js
  ./node_modules/.bin/webpack --env.rpi_url=http://localhost:1337/
```

That will generate the UI in the `ptui/build` directory -- now host that on a static web server and
hit the "GM.html" or "Player.html" files in it.
