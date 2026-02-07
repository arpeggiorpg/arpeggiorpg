use std::collections::HashMap;

use serde::{de::Error as _, Deserialize, Deserializer, Serialize};
use serde_json::Value;
use tracing::info;
use worker::*;

use arptypes::{
    multitenant::{GameID, GameIndex},
    GameLog,
};

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
    pub extra: HashMap<String, serde_json::Value>,
}

impl LegacyKVStorage {
    pub fn logs(&self) -> anyhow::Result<Vec<(GameIndex, GameLog)>> {
        let logs: anyhow::Result<Vec<(GameIndex, GameLog)>> = self
            .extra
            .iter()
            .filter(|(k, _v)| k.starts_with("log"))
            .map(|(k, v)| {
                let key = parse_log_key(k)?;
                let value: String = serde_json::from_value(v.clone())?;
                let value: GameLog = serde_json::from_str(&value)?;
                Ok((key, value))
            })
            .collect();
        let mut logs = logs?;
        logs.sort_by_key(|(k, _v)| *k);
        Ok(logs)
    }
}

pub fn parse_log_key(key: &str) -> anyhow::Result<GameIndex> {
    // Expected format: "log-{snapshot_idx}-idx-{log_idx}"
    let parts: Vec<&str> = key.split('-').collect();
    if parts.len() != 4 || parts[0] != "log" || parts[2] != "idx" {
        return Err(anyhow::anyhow!("not a log key: {key}"));
    }

    let snapshot_idx = parts[1].parse()?;
    let log_idx = parts[3].parse()?;

    Ok(GameIndex {
        game_idx: snapshot_idx,
        log_idx,
    })
}

pub async fn fetch_raw_legacy_dump_str(
    env: Env,
    game_id: GameID,
) -> anyhow::Result<Option<String>> {
    let legacy_ns = env.durable_object("ARPEGGIOGAME_LEGACY")?;
    let legacy_id = legacy_ns.id_from_name(&game_id.to_string())?;
    let stub = legacy_id.get_stub()?;
    let mut resp = stub.fetch_with_str("https://dummy/dump").await?;

    if resp.status_code() == 404 {
        return Ok(None);
    }
    Ok(Some(resp.text().await?))
}

pub async fn fetch_raw_legacy_dump(
    env: Env,
    game_id: GameID,
) -> anyhow::Result<Option<serde_json::Value>> {
    let text = fetch_raw_legacy_dump_str(env, game_id).await?;
    if let Some(text) = text {
        let json = serde_json::from_str(&text)?;
        return Ok(Some(json));
    }
    Ok(None)
}

pub async fn fetch_legacy_dump(
    env: Env,
    game_id: GameID,
) -> anyhow::Result<Option<LegacyKVStorage>> {
    let text = fetch_raw_legacy_dump_str(env, game_id).await?;
    if let Some(text) = text {
        // An empty legacy DO returns "{}" — there's no legacy data to migrate.
        let parsed: serde_json::Value = serde_json::from_str(&text)?;
        if parsed.as_object().is_some_and(|o| o.is_empty()) {
            return Ok(None);
        }
        let mut deserializer = serde_json::Deserializer::from_str(&text);
        let x: LegacyKVStorage = serde_path_to_error::deserialize(&mut deserializer)?;
        return Ok(Some(x));
    }
    Ok(None)
}

fn deserialize_double_encoded<'de, D, T>(deser: D) -> std::result::Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: for<'a> Deserialize<'a>,
{
    let s = String::deserialize(deser)?;
    serde_json::from_str(&s).map_err(D::Error::custom)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_log_key() {
        let result = parse_log_key("log-000000000-idx-000000001");
        let parts = result.unwrap();
        assert_eq!(
            parts,
            GameIndex {
                game_idx: 0,
                log_idx: 1
            }
        );

        let result = parse_log_key("log-000000005-idx-000000123");
        let parts = result.unwrap();
        assert_eq!(
            parts,
            GameIndex {
                game_idx: 5,
                log_idx: 123
            }
        );

        // Invalid formats
        assert!(parse_log_key("invalid-key").is_err());
        assert!(parse_log_key("log-abc-idx-123").is_err());
        assert!(parse_log_key("log-123-invalid-456").is_err());
    }

    #[test]
    fn test_parse_legacy_storage() {
        let json = include_str!("domigrations/dumped-legacy-kv.json");
        let jd = &mut serde_json::Deserializer::from_str(json);

        let legacy_kv: LegacyKVStorage = serde_path_to_error::deserialize(jd).unwrap();
        assert_eq!(legacy_kv.snapshot_0, Default::default());
        assert_eq!(legacy_kv.logs().unwrap().len(), 110);
        assert_eq!(
            legacy_kv.background_images,
            vec![
                "https://arpeggiogame.com/cdn-cgi/imagedelivery/0DU4Tw-CmEbMyEuTLNg6Xg/50125cee-5033-447a-a311-24c9f3d4a994/7e9868fb-457f-4cc7-9864-8a03cf37a0ce",
                "https://arpeggiogame.com/cdn-cgi/imagedelivery/0DU4Tw-CmEbMyEuTLNg6Xg/50125cee-5033-447a-a311-24c9f3d4a994/99e68793-4bb6-4cca-9a08-8a9305a2a432",
                "https://arpeggiogame.com/cdn-cgi/imagedelivery/0DU4Tw-CmEbMyEuTLNg6Xg/50125cee-5033-447a-a311-24c9f3d4a994/175349e5-2d31-4bca-8113-82f380d5d109"
        ]);
    }

    #[test]
    fn test_parse_buggydump() {
        let json = include_str!("domigrations/buggydump.json");
        let jd = &mut serde_json::Deserializer::from_str(json);

        let _legacy_kv: LegacyKVStorage = serde_path_to_error::deserialize(jd).unwrap();
    }
}
