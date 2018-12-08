use std::default::Default;

use lambda_runtime::{self, lambda, error::HandlerError};
use failure;
use graphql_client; // I need to send a mutation to AppSync's GraphQL in order to notify waiting clients.
use rusoto_cognito_identity; // I need to be able to verify cognito tokens
use rusoto_core::Region;
use rusoto_ssm::{self, Ssm}; // I need to be able to fetch parameters so I can authenticate against AppSync
use serde::Serialize;
use serde_json::Value;

fn main() {
    // TODO: when the next lambda_runtime is released (0.2.0 probably), change
    // this so handler is a closur (or otherwise implements the Handler trait:
    // https://github.com/awslabs/aws-lambda-rust-runtime/blob/master/lambda-runtime/src/runtime.rs#L15)
    // and save the `region` and `ssm` client and `appsync_secret` between
    // requests.
    lambda!(handler);
}

#[derive(Serialize, Debug)]
struct Response {
    secret: String,
    input_event: serde_json::Value,
}

#[allow(clippy::needless_pass_by_value)]
fn handler(event: Value, ctx: lambda_runtime::Context) -> Result<Response, HandlerError> {
    let region = Region::default();
    let ssm = rusoto_ssm::SsmClient::new(region.clone());
    let appsync_secret_param = ssm
        .get_parameter(rusoto_ssm::GetParameterRequest {
            name: "pandt-backend-cognito-password".to_string(),
            with_decryption: Some(true),
        })
        .sync()
        .map_err(|_e| ctx.new_error("Couldn't get pandt-backend-cognito-password"))?;
    let appsync_secret = appsync_secret_param
        .parameter
        .ok_or_else(|| {
            ctx.new_error("Couldn't find pandt-backend-cognito-password parameter?")
        })?
        .value
        .ok_or_else(|| ctx.new_error("pandt-backend-cognito-password didn't have a value"))?;
    Ok(Response {
        secret: appsync_secret,
        input_event: event,
    })
}
