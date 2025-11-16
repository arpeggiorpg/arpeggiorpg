use serde_json::Value;
use tracing::info;
use worker::*;

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
            info!("Dumping storage!");
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
