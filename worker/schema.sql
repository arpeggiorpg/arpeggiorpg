/* remember to include NOT NULL directives when creating new tables! */

/* Also: Don't put semicolons in comments! Wrangler has a bug. */

CREATE TABLE IF NOT EXISTS game_metadata (
    game_id text PRIMARY KEY NOT NULL,
    name text NOT NULL
);

CREATE TABLE IF NOT EXISTS superusers (
    user_id text PRIMARY KEY NOT NULL
);

INSERT OR IGNORE INTO superusers (user_id) VALUES ('google_105096330625444095578');


CREATE TABLE IF NOT EXISTS user_games (
    user_id text NOT NULL,
    game_id text NOT NULL,
    role text NOT NULL,
    profile_name text NOT NULL
);
CREATE INDEX IF NOT EXISTS user_games_by_user_idx ON user_games (user_id);


/* This UNIQUE constraint ensures that each *user* can only be associated to a game once as a GM and
once as a player.

(really, the only reason it includes "role" is for testing purposes. I don't think there are any
real use-cases for a user to be both a GM and a player at the same time)
*/
CREATE UNIQUE INDEX IF NOT EXISTS user_game_role_unique ON user_games(user_id, game_id, role);
