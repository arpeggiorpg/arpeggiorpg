use dioxus::prelude::*;
use reqwest::StatusCode;
use wasm_cookies::CookieOptions;

use arptypes::{
    hosted::{CopyToPreprodResult, GameList, InvitationCheck, InvitationID},
    protocol::{GameID, Role},
};

use crate::rpi::{rpi_url, websocket_base_url};

pub static AUTH_TOKEN: GlobalSignal<String> = Signal::global(String::new);

pub fn auth_token() -> String {
    wasm_cookies::get("arpeggio-token")
        .unwrap_or(Ok(String::new()))
        .unwrap_or_default()
}

pub fn is_preprod() -> bool {
    let Some(document) = web_sys::window().and_then(|window| window.document()) else {
        return false;
    };
    let Ok(Some(meta)) = document.query_selector("meta[name='ARPEGGIO_ENVIRONMENT']") else {
        return false;
    };
    meta.get_attribute("content").as_deref() == Some("preprod")
}

#[derive(Clone, Debug, serde::Deserialize, PartialEq)]
pub struct CurrentUser {
    pub is_superuser: bool,
}

pub async fn list_games() -> anyhow::Result<GameList> {
    rpi_get("g/list").await
}

pub async fn current_user() -> anyhow::Result<CurrentUser> {
    rpi_get("me").await
}

pub async fn create_game(name: String) -> anyhow::Result<GameID> {
    #[derive(serde::Deserialize)]
    struct CreateGameResponse {
        game_id: GameID,
    }
    let resp: CreateGameResponse = rpi_post("g/create", &name).await?;
    Ok(resp.game_id)
}

pub async fn copy_game_from_production(game_id: &str) -> anyhow::Result<CopyToPreprodResult> {
    rpi_post(&format!("superuser/copy-from-production/{game_id}"), &()).await
}

pub async fn delete_preprod_copy(game_id: &str) -> anyhow::Result<()> {
    let _: serde_json::Value =
        rpi_post(&format!("superuser/delete-preprod-copy/{game_id}"), &()).await?;
    Ok(())
}

pub async fn check_invitation(
    game_id: GameID,
    invitation_id: InvitationID,
) -> anyhow::Result<InvitationCheck> {
    rpi_get(&format!("g/invitations/{game_id}/{invitation_id}")).await
}

pub async fn accept_invitation(
    game_id: GameID,
    invitation_id: InvitationID,
    profile_name: String,
) -> anyhow::Result<()> {
    let _: serde_json::Value = rpi_post(
        &format!("g/invitations/{game_id}/{invitation_id}/accept"),
        &profile_name,
    )
    .await?;
    Ok(())
}

pub async fn game_websocket_url(role: Role, game_id: GameID) -> anyhow::Result<String> {
    let response =
        rpi_get::<serde_json::Value>(&format!("request-websocket/{game_id}/{role}")).await?;
    let token = response
        .get("token")
        .and_then(serde_json::Value::as_str)
        .ok_or_else(|| anyhow::anyhow!("No token found in request-websocket response"))?;
    let ws_base = websocket_base_url()?;
    Ok(format!("{ws_base}/ws/{game_id}/{token}"))
}

fn logout_for_auth_failure() {
    *AUTH_TOKEN.write() = String::new();
    let options: CookieOptions = Default::default();
    wasm_cookies::set("arpeggio-token", "", &options.with_path("/"));
}

async fn ensure_successful_response(
    response: reqwest::Response,
    context: &str,
) -> anyhow::Result<reqwest::Response> {
    let status = response.status();
    if status == StatusCode::UNAUTHORIZED || status == StatusCode::FORBIDDEN {
        let body = response.text().await.unwrap_or_default();
        logout_for_auth_failure();
        return Err(anyhow::anyhow!("{context} failed with {status}: {body}"));
    }
    if !status.is_success() {
        let body = response.text().await.unwrap_or_default();
        return Err(anyhow::anyhow!("{context} failed with {status}: {body}"));
    }
    Ok(response)
}

pub(crate) async fn rpi_get<T: serde::de::DeserializeOwned>(path: &str) -> anyhow::Result<T> {
    let url = format!("{}/{path}", rpi_url().trim_end_matches('/'));
    let response = reqwest::Client::new()
        .get(url)
        .header("x-arpeggio-auth", AUTH_TOKEN())
        .send()
        .await?;
    ensure_successful_response(response, "GET request")
        .await?
        .json()
        .await
        .map_err(Into::into)
}

async fn rpi_post<B: serde::Serialize, T: serde::de::DeserializeOwned>(
    path: &str,
    body: &B,
) -> anyhow::Result<T> {
    let url = format!("{}/{path}", rpi_url().trim_end_matches('/'));
    let response = reqwest::Client::new()
        .post(url)
        .header("x-arpeggio-auth", AUTH_TOKEN())
        .json(body)
        .send()
        .await?;
    ensure_successful_response(response, "POST request")
        .await?
        .json()
        .await
        .map_err(Into::into)
}
