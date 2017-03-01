extern crate clap;
extern crate handlebars;

use std::collections::HashMap;
use std::fs;
use std::io::{Read, Write};
use std::path;

fn main() {
  let matches = clap::App::new("PT Builder Tool")
    .arg(clap::Arg::with_name("rpi-url")
      .long("rpi-url")
      .value_name("URL")
      .required(true)
      .help("URL pointing at the P&T RPI"))
    .arg(clap::Arg::with_name("ptui-dir")
      .long("ptui-dir")
      .value_name("DIR")
      .required(true)
      .help("Directory of PTUI root"))
    .get_matches();

  let rpi = matches.value_of("rpi-url").expect("rpi-url required");
  let ptui_dir = path::Path::new(matches.value_of("ptui-dir").expect("ptui-dir required"));
  let build_dir = ptui_dir.join("build");

  if !build_dir.exists() {
    fs::create_dir(&build_dir)
      .expect(&format!("Couldn't create directory {:?}", build_dir.to_str()));
  }

  build_js(ptui_dir, build_dir.as_path());

  build_html(ptui_dir, build_dir.as_path(), rpi);
}

fn build_js(ptui_dir: &path::Path, build_dir: &path::Path) {
  use std::process::Command;
  for &(elm, js) in [("Player.elm", "Player.js"), ("GM.elm", "GM.js")].iter() {
    let mut child = Command::new("elm")
      .arg("make")
      .arg(ptui_dir.join("src").join(elm))
      .arg("--output")
      .arg(build_dir.join(js))
      .current_dir(ptui_dir)
      .spawn()
      .expect(&format!("Couldn't build {}", elm));
    let code = child.wait().expect(&format!("Failed to wait on elm make for {}", elm));
    if !code.success() {
      panic!("elm make {} was unsuccessful: {:?}", elm, code);
    }
  }
}

fn build_html(ptui_dir: &path::Path, build_dir: &path::Path, rpi: &str) {
  let template = load_template(ptui_dir);
  let mut handlebars = handlebars::Handlebars::new();
  handlebars.register_template_string("template.html", template)
    .expect("Couldn't register_template_string");

  for &(js_fn, html_fn) in [("GM.js", "GM.html"), ("Player.js", "Player.html")].iter() {
    let data = template_data(rpi.to_string(), js_fn.to_string());
    let populated = handlebars.render("template.html", &data).expect("Couldn't render template");
    let html_path = build_dir.join(html_fn);
    let mut outfile = fs::File::create(&html_path)
      .expect(&format!("Couldn't create {:?}", html_path.to_str()));
    outfile.write_all(populated.as_bytes())
      .expect(&format!("Couldn't write populated data to {:?}", html_path.to_str()));
    println!("Wrote file {:?}", html_path.to_str());
  }
}

fn template_data(rpi: String, js_source: String) -> HashMap<String, String> {
  let mut data = HashMap::new();
  data.insert("rpi-url".to_string(), rpi);
  data.insert("js-source".to_string(), js_source);
  data
}


fn load_template(ptui_dir: &path::Path) -> String {
  let path = ptui_dir.join("template.html");
  let mut f = fs::File::open(&path).expect(&format!("Couldn't open {:?}", path.to_str()));
  let mut s = String::new();
  f.read_to_string(&mut s).expect(&format!("Couldn't read data from {:?}", path.to_str()));
  s
}
