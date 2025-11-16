use std::collections::HashMap;

use anyhow;
use serde::{de::Error as _, Deserialize, Deserializer, Serialize};
use serde_json::Value;
use tracing::info;
use worker::*;

use arptypes::{multitenant::GameID, GameLog};

/// This is the legacy KV-backed Durable Object. We have since switched to ArpeggioGameSql. This DO
/// still exists only to export its legacy storage, to be used in domigrations/v2_sqlite.rs.
#[durable_object]
pub struct ArpeggioGame {
    state: durable::State,
}

impl DurableObject for ArpeggioGame {
    fn new(state: durable::State, _env: Env) -> Self {
        Self { state }
    }

    async fn fetch(&self, req: Request) -> Result<Response> {
        let url = req.url()?;

        // Only support a simple dump endpoint in the legacy DO
        if url.path() == "/dump" {
            info!("LegacyKV: Dumping storage!");
            let storage = self.state.storage();
            let map = storage.list().await?;

            // `list()` returns a JS Map; convert it into a serde_json::Value
            let js_val = wasm_bindgen::JsValue::from(map);
            let data: Value = serde_wasm_bindgen::from_value(js_val)?;

            return Response::from_json(&data);
        }

        Response::error("Legacy ArpeggioGame DO: only /dump is supported", 410)
    }
}


// The following code is all used on the client side to fetch/process the data that the above
// durable object returns.

#[derive(Deserialize, Serialize)]
pub struct LegacyKVStorage {
    #[serde(
        rename = "snapshot-0-chunk-0",
        deserialize_with = "deserialize_double_encoded"
    )]
    pub snapshot_0: arptypes::Game,

    #[serde(default)]
    pub invitations: Vec<String>,

    #[serde(rename = "images-BackgroundImage", default)]
    pub background_images: Vec<String>,

    #[serde(rename = "images-CreatureIcon", default)]
    pub creature_icons: Vec<String>,

    #[serde(flatten)]
    pub logs: HashMap<String, DoubleEncoded<GameLog>>,
}

pub async fn fetch_raw_legacy_dump(
    env: Env,
    game_id: GameID,
) -> anyhow::Result<Option<serde_json::Value>> {
    let legacy_ns = env.durable_object("ARPEGGIOGAME_LEGACY")?;
    let legacy_id = legacy_ns.id_from_name(&game_id.to_string())?;
    let stub = legacy_id.get_stub()?;
    let mut resp = stub.fetch_with_str("https://dummy/dump").await?;

    if resp.status_code() == 404 {
        return Ok(None);
    }
    Ok(resp.json().await?)
}

pub async fn fetch_legacy_dump(
    env: Env,
    game_id: GameID,
) -> anyhow::Result<Option<LegacyKVStorage>> {
    let json = fetch_raw_legacy_dump(env, game_id).await?;
    if let Some(json) = json {
        let x: LegacyKVStorage = serde_json::from_value(json)?;
        return Ok(Some(x));
    }
    Ok(None)
}

#[derive(Debug, Serialize)]
pub struct DoubleEncoded<T>(pub T);

impl<'de, T> Deserialize<'de> for DoubleEncoded<T>
where
    T: for<'a> Deserialize<'a>,
{
    fn deserialize<D>(deser: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserialize_double_encoded(deser).map(DoubleEncoded)
    }
}

fn deserialize_double_encoded<'de, D, T>(deser: D) -> std::result::Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: for<'a> Deserialize<'a>,
{
    let s = String::deserialize(deser)?;
    serde_json::from_str(&s).map_err(D::Error::custom)
}
