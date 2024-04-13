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

const RPI_URL: &'static str = "http://localhost:8787";

static AUTH_TOKEN: GlobalSignal<String> = Signal::global(|| "".to_owned());

#[component]
fn Main() -> Element {
  rsx! {
    div { Link { to: Route::Test, "Test stuff" } }
    div {
      "Enter your auth token:"
      input {
        value: "{AUTH_TOKEN}",
        oninput: move |event| *AUTH_TOKEN.write() = event.value()
      }
    }
    GameList {}
  }
}

async fn list_games() -> Result<multitenant::GameList, reqwest::Error> {
  let url = format!("{RPI_URL}/g/list");
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
