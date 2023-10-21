/* remember to include NOT NULL directives when creating new tables! */

CREATE TABLE IF NOT EXISTS game_metadata (
    game_id text PRIMARY KEY NOT NULL,
    name text NOT NULL
);


CREATE TABLE IF NOT EXISTS user_games (
    user_id text NOT NULL,
    game_id text NOT NULL,
    profile_name text NOT NULL,
    role text NOT NULL
);
CREATE INDEX IF NOT EXISTS user_games_by_user_idx ON user_games (user_id);
