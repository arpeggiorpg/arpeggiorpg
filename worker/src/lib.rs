use anyhow::anyhow;
use tracing_subscriber::{
  fmt::{format::Pretty, time::UtcTime},
  prelude::*,
};
use tracing_web::{performance_layer, MakeConsoleWriter};
use worker::*;

mod cfworker;
mod domigrations;
mod durablegame;
mod storage;
mod wsrpi;

// Things I've learned about error-handling in workers-rs:
// - any Err returned from the main worker doesn't seem to do anything other than "Error: The script
//   will never generate a response.". So there *must* be an error handler in main that produces a
//   custom Response. (turns out I'm wrong: see `respond_with_errors`)
//   - The same is not true for the Durable Object. When I return an Err from the DO, it ends up in
//     the HTTP response.

// - Panics in the worker do not get printed anywhere, as far as I can tell, and they result in the
//   same "The script will never generate a response" message.
//   - Panics in the Durable Object are even worse; the DO seems to disappear from the face of the
//     net and the Worker just hangs waiting from a response instead of getting some sort of error.

// So, we can just use a panic hook to log the panics. However, I'm still concerned about the fact
// that a panicking DO does not immediately return a 500 or even seem to drop the connection to the
// waiting Worker. I'll have to see what the behavior is in actual production; maybe this is just a
// behavior of the local dev environment.

#[event(start)]
fn start() {
  console_error_panic_hook::set_once();
  let fmt_layer = tracing_subscriber::fmt::layer()
    .json()
    .with_ansi(false) // Only partially supported across JavaScript runtimes
    .with_timer(UtcTime::rfc_3339()) // std::time is not available in browsers
    .with_writer(MakeConsoleWriter); // write events to the console
  let perf_layer = performance_layer().with_details_from_fields(Pretty::default());
  tracing_subscriber::registry().with(fmt_layer).with(perf_layer).init();
}

/// For some reason I can't just convert a workers::Error to an anyhow::Error because I get crazy
/// errors about how a *mut u8 might escape an async closure or something. So this converts the
/// error to a string before converting it to an anyhow Error.
pub fn anyhow_str<T: std::fmt::Debug>(e: T) -> anyhow::Error { anyhow!("{e:?}") }

pub fn rust_error<T: std::fmt::Debug>(e: T) -> Error { Error::RustError(format!("{e:?}")) }
