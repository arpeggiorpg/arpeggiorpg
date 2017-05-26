extern crate clap;
extern crate handlebars;

use std::collections::HashMap;
use std::fs;
use std::io::{Read, Write};
use std::io;
use std::path;
use std::env;
use std::process::Command;
use std::thread;

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
    .arg(clap::Arg::with_name("watch")
           .long("watch")
           .help("If specified, continuously watch for changes to Elm code and rebuild. Requires \
             watchexec."))
    .get_matches()
    .clone();

  let rpi: String = matches.value_of("rpi-url").expect("rpi-url required").to_string();
  let ptui_dir: path::PathBuf =
    path::Path::new(matches.value_of("ptui-dir").expect("ptui-dir required")).to_path_buf();
  let build_dir: path::PathBuf = ptui_dir.join("build");

  if !build_dir.exists() {
    fs::create_dir(&build_dir).expect(&format!("Couldn't create directory {:?}",
                                               build_dir.to_str()));
  }

  if matches.is_present("watch") {
    watch(&ptui_dir, &rpi);
  } else {
    let thread1 = {
      let ptui_dir = ptui_dir.clone();
      thread::spawn(move || build_js(&ptui_dir))
    };
    let thread2 = {
      let ptui_dir = ptui_dir.clone();
      let build_dir = build_dir.clone();
      thread::spawn(move || build_html(&ptui_dir, build_dir.as_path(), &rpi))
    };
    let thread3 = {
      let ptui_dir = ptui_dir.clone();
      thread::spawn(move || webpack(&ptui_dir))
    };
    thread1.join().expect("build_js thread failed");
    thread2.join().expect("build_html thread failed");
    thread3.join().expect("webpack thread failed");

    copy_others(&ptui_dir, build_dir.as_path()).expect("Couldn't copy other files to build dir");
    println!("Done! Build successful!");
  }
}

fn watch(ptui_dir: &path::Path, rpi: &str) {
  println!("Starting watchexec...");
  let me = env::current_exe().expect("Couldn't get current executable!");
  let mut child = Command::new("watchexec")
    .arg("-r")
    .arg("-w")
    .arg(ptui_dir)
    .arg("--")
    .arg(me)
    .arg("--ptui-dir")
    .arg(ptui_dir)
    .arg("--rpi-url")
    .arg(rpi)
    .spawn()
    .expect(&format!("Couldn't run watchexec :("));
  child.wait().expect("watchexec exited");
}

fn build_js(ptui_dir: &path::Path) {
  for &(elm, js) in [("Player.elm", "Player.js"), ("GM.elm", "GM.js")].iter() {
    let mut child = Command::new("elm")
      .arg("make")
      .arg(path::Path::new("src").join(elm))
      .arg("--output")
      .arg(path::Path::new("build").join(js))
      .current_dir(ptui_dir)
      .spawn()
      .expect(&format!("Couldn't build {}", elm));
    let code = child.wait().expect(&format!("Failed to wait on elm make for {}", elm));
    if !code.success() {
      panic!("elm make {} was unsuccessful: {:?}", elm, code);
    }
  }
}

fn webpack(ptui_dir: &path::Path) {
  // This unfortunately doesn't work if I just use `webpack`, so for now this code is
  // windows-specific :(
  let mut child =
    Command::new("webpack.cmd").current_dir(ptui_dir).spawn().expect("Couldn't build command?");
  let code = child.wait().expect("webpack failed!");
  if !code.success() {
    panic!("webpack failed! {:?}", code);
  }
}

fn copy_others(ptui_dir: &path::Path, build_dir: &path::Path) -> Result<(), io::Error> {
  /// Copy all javascript (.js) files in ptui_dir/src to the build dir.
  for direntry in fs::read_dir(ptui_dir.join("src"))? {
    let direntry = direntry?;
    if direntry.file_name().to_str().expect("Couldn't parse file as utf-8").ends_with(".js") {
      let filename = direntry.path();
      fs::copy(filename, build_dir.join(direntry.file_name()))?;
    }
  }
  Ok(())
}

fn build_html(ptui_dir: &path::Path, build_dir: &path::Path, rpi: &str) {
  let template = load_template(ptui_dir);
  let mut handlebars = handlebars::Handlebars::new();
  handlebars
    .register_template_string("template.html", template)
    .expect("Couldn't register_template_string");

  for &(js_fn, html_fn) in [("GM.js", "GM.html"), ("Player.js", "Player.html")].iter() {
    let data = template_data(rpi.to_string(), js_fn.to_string());
    let populated = handlebars.render("template.html", &data).expect("Couldn't render template");
    let html_path = build_dir.join(html_fn);
    let mut outfile =
      fs::File::create(&html_path).expect(&format!("Couldn't create {:?}", html_path.to_str()));
    outfile
      .write_all(populated.as_bytes())
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
  let path = ptui_dir.join("src/template.html");
  let mut f = fs::File::open(&path).expect(&format!("Couldn't open {:?}", path.to_str()));
  let mut s = String::new();
  f.read_to_string(&mut s).expect(&format!("Couldn't read data from {:?}", path.to_str()));
  s
}
