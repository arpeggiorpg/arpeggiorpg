#![allow(non_snake_case)]

use dioxus::prelude::*;
use log::{info, LevelFilter};

use arptypes::types::Dice;
use arptypes::multitenant;

#[derive(Clone, Routable, Debug, PartialEq)]
enum Route {
  #[route("/")]
  Main,
  #[route("/test")]
  Test,
  #[route("/blog/:id")]
  Blog { id: i32 },
}

fn main() {
  // Init debug
  dioxus_logger::init(LevelFilter::Info).expect("failed to init logger");
  console_error_panic_hook::set_once();

  launch(App);
}

fn App() -> Element {
  rsx! { Router::<Route> {} }
}

#[component]
fn Blog(id: i32) -> Element {
  rsx! {
    Link { to: Route::Main, "Go Home" }
    "Blog post {id}"
  }
}

static AUTH_TOKEN: GlobalSignal<String> = Signal::global(String::new);

fn auth_token() -> String {
  wasm_cookies::get("arpeggio-token").unwrap_or(Ok(String::new())).unwrap_or(String::new())
}

#[component]
fn Main() -> Element {

  // Mirror the cookie to the GlobalSignal so things can reactively respond to it changing.
  if AUTH_TOKEN() != auth_token() {
    *AUTH_TOKEN.write() = auth_token();
  }

  rsx! {
    div { Link { to: Route::Test, "Test stuff" } }
    div { "Oh no, the cookie {AUTH_TOKEN():?} "}
    div {
      "Enter your auth token:"
      input {
        value: "{AUTH_TOKEN}",
        oninput: move |event| {
          *AUTH_TOKEN.write() = event.value();
          wasm_cookies::set("arpeggio-token", &event.value(), &Default::default())
        }
      }
    }
    GameList {}
  }
}

fn rpi_url() -> String {
  let window = web_sys::window().expect("global window doesn't exist");
  let document = window.document().expect("document must exist");
  let meta = document.query_selector("meta[name='RPI_URL']").expect("meta RPI_URL tag must exist in index.html (1)").expect("meta RPI_URL tag must exist in index.html (2)");
  meta.get_attribute("content").expect("meta RPI_URL tag must have content")
}

async fn list_games() -> Result<multitenant::GameList, reqwest::Error> {
  let rpi_url = rpi_url();
  let url = format!("{rpi_url}/g/list");
  let client = reqwest::Client::new();
  let response = client.get(url).header("x-arpeggio-auth", AUTH_TOKEN()).send().await?;
  response.json().await
}

#[component]
fn GameList() -> Element {
  let list = use_resource(move || list_games());

  match &*list.read_unchecked() {
    Some(Ok(list)) => rsx! {
      "got a list!"
      ul {
        for (profile, metadata) in &list.games {
          li { "{profile.profile_name} @ {metadata.name} (as {profile.role})" }
        }
      }
    },
    Some(Err(e)) => rsx! { "Error! {e:?}" },
    None => rsx! {"Promise not fulfilled"}
  }

}

#[component]
fn Test() -> Element {
  let mut count = use_signal(|| 0);

  let dice = Dice::Expr { num: 3, size: 6 };

  rsx! {
    ul {
      li { Link { to: Route::Blog { id: count() }, "Go to blog" } }
      li { Link { to: Route::Main, "Go Home" } }
    }
    div {
      h1 { "High-Five counter: {count}" }
      div { "Dice expression: {dice:?}" }
      button {
        onclick: move |_| {
            count += 1;
            info!("UP {count}");
        },
        "Up high!"
      }
      button {
        onclick: move |_| {
            count -= 1;
            info!("DOWN {count}");
        },
        "Down low!"
      }
    }
  }
}
