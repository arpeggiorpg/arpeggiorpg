use lambda_runtime::{self, error::HandlerError, start};
use serde::Serialize;
use serde_json::Value;

#[derive(Serialize, Debug)]
struct Response {
  secret: String,
  input_event: Value,
}

#[allow(clippy::needless_pass_by_value)]
fn main() -> Result<(), failure::Error> {
  let handler = move |event: Value, _ctx: lambda_runtime::Context| -> Result<Response, HandlerError> {
    Ok(Response {
      secret: "foo".to_string(),
      input_event: event,
    })
  };

  start(handler, None);
  Ok(())
}
