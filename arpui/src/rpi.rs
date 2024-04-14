use arptypes::multitenant;
use dioxus::signals::{GlobalSignal, Signal};

pub static AUTH_TOKEN: GlobalSignal<String> = Signal::global(String::new);

pub fn auth_token() -> String {
  wasm_cookies::get("arpeggio-token").unwrap_or(Ok(String::new())).unwrap_or(String::new())
}

fn rpi_url() -> String {
  let window = web_sys::window().expect("global window doesn't exist");
  let document = window.document().expect("document must exist");
  let meta = document.query_selector("meta[name='RPI_URL']").expect("meta RPI_URL tag must exist in index.html (1)").expect("meta RPI_URL tag must exist in index.html (2)");
  meta.get_attribute("content").expect("meta RPI_URL tag must have content")
}

pub async fn list_games() -> Result<multitenant::GameList, reqwest::Error> {
  let rpi_url = rpi_url();
  let url = format!("{rpi_url}/g/list");
  let client = reqwest::Client::new();
  let response = client.get(url).header("x-arpeggio-auth", AUTH_TOKEN()).send().await?;
  response.json().await
}
