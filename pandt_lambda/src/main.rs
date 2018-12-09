use std::default::Default;

use failure::{format_err, self};
use graphql_client; // I need to send a mutation to AppSync's GraphQL in order to notify waiting clients.
use lambda_runtime::{self, error::HandlerError, start};
use rusoto_cognito_identity; // I need to be able to verify cognito tokens
use rusoto_core::Region;
use rusoto_ssm::{self, Ssm}; // I need to be able to fetch parameters so I can authenticate against AppSync
use serde::Serialize;
use serde_json::Value;

#[derive(Serialize, Debug)]
struct Response {
  secret: String,
  input_event: serde_json::Value,
}

#[allow(clippy::needless_pass_by_value)]
fn main() -> Result<(), failure::Error> {
  let region = Region::default();
  let ssm = rusoto_ssm::SsmClient::new(region.clone());
  let appsync_secret_param = ssm
    .get_parameter(rusoto_ssm::GetParameterRequest {
      name: "pandt-backend-cognito-password".to_string(),
      with_decryption: Some(true),
    })
    .sync()?;
  let appsync_secret = appsync_secret_param
    .parameter
    .ok_or_else(|| format_err!("Couldn't find pandt-backend-cognito-password parameter?"))?
    .value
    .ok_or_else(|| format_err!("pandt-backend-cognito-password didn't have a value"))?;

  let handler = move |event: Value, _ctx: lambda_runtime::Context| -> Result<Response, HandlerError> {
    Ok(Response {
      secret: appsync_secret.clone(),
      input_event: event,
    })
  };

  start(handler, None);
  Ok(())
}
