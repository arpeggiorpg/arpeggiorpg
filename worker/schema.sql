CREATE TABLE IF NOT EXISTS game_metadata (
    game_id text PRIMARY KEY,
    name text
);


CREATE TABLE IF NOT EXISTS user_games (
    user_id text,
    game_id text,
    profile_name text,
    role text
);
CREATE INDEX IF NOT EXISTS user_games_by_user_idx ON user_games (user_id);
