use tracing::info;
use worker::*;

/// The retired KV-backed Durable Object.
///
/// Its namespace is retained temporarily as a recovery artifact, but normal game loading and dump
/// generation never read from it.
#[durable_object]
pub struct ArpeggioGame {
    state: durable::State,
}

impl DurableObject for ArpeggioGame {
    fn new(state: durable::State, _env: Env) -> Self {
        Self { state }
    }

    async fn fetch(&self, req: Request) -> Result<Response> {
        if req.url()?.path() == "/dump" {
            info!(event = "legacy-kv-recovery-dump");
            let map = self.state.storage().list().await?;
            let value: serde_json::Value =
                serde_wasm_bindgen::from_value(wasm_bindgen::JsValue::from(map))?;
            return Response::from_json(&value);
        }

        Response::error(
            "Legacy game storage: only recovery dumps are supported",
            410,
        )
    }
}
