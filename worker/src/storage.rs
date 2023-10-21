use serde::Deserialize;
use worker::Env;

use arpeggio::types::PlayerID;
use mtarp::types::{GameID, Role, UserID, GameProfile};

pub async fn list_games_with_names(env: &Env, user_id: UserID) -> worker::Result<Vec<GameInfo>> {
  let db = env.d1("DB")?;
  let statement =
  db.prepare("SELECT UG.user_id, UG.game_id, UG.profile_name, UG.role, meta.name FROM user_games UG, game_metadata meta WHERE UG.game_id = meta.game_id AND user_id = ?");
  let statement = statement.bind(&[user_id.to_string().into()])?;
  let game_infos: Vec<GameInfo> = statement.all().await?.results()?;
  Ok(game_infos)
}

#[derive(Deserialize)]
pub struct GameInfo {
  pub user_id: UserID,
  pub game_id: GameID,
  pub profile_name: PlayerID,
  pub role: Role,
  pub name: String,
}

pub async fn create_game(env: &Env, game_id: GameID, user_id: UserID, name: String) -> worker::Result<()> {
  create_profile(env, game_id, user_id, PlayerID("GM".to_string()), Role::GM).await?;
  let db = env.d1("DB")?;
  let statement = db.prepare("INSERT INTO game_metadata (game_id, name) VALUES (?, ?)");
  let statement = statement.bind(&[game_id.to_string().into(), name.into()])?;
  statement.run().await?;
  Ok(())
}

pub async fn check_game_access(
  env: &Env, user_id: UserID, game_id: GameID, role: Role,
) -> worker::Result<Option<GameProfile>> {
  let db = env.d1("DB")?;
  let statement =
    db.prepare("SELECT user_id, game_id, profile_name, role FROM user_games WHERE user_id = ? AND game_id = ? AND role = ?");
  let statement = statement.bind(&[
    user_id.to_string().into(),
    game_id.to_string().into(),
    role.to_string().into(),
  ])?;
  Ok(statement.first(None).await?)
}

pub async fn create_profile(
  env: &Env, game_id: GameID, user_id: UserID, profile_name: PlayerID, role: Role,
) -> worker::Result<()> {
  let db = env.d1("DB")?;
  let statement =
    db.prepare("INSERT INTO user_games (user_id, game_id, profile_name, role) VALUES (?, ?, ?, ?)");
  let statement = statement.bind(&[
    user_id.to_string().into(),
    game_id.to_string().into(),
    profile_name.0.to_string().into(),
    role.to_string().into(),
  ])?;
  statement.run().await?;
  Ok(())
}
