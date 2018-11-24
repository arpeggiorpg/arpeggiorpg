use std::default::Default;

use aws_lambda::start;
use failure;
use graphql_client; // I need to send a mutation to AppSync's GraphQL in order to notify waiting clients.
use rusoto_cognito_identity; // I need to be able to verify cognito tokens
use rusoto_core::Region;
use rusoto_ssm::{self, Ssm}; // I need to be able to fetch parameters so I can authenticate against AppSync

fn main() -> Result<(), failure::Error> {
    let region = Region::default();
    let ssm = rusoto_ssm::SsmClient::new(region.clone());
    let appsync_secret_param = ssm
        .get_parameter(rusoto_ssm::GetParameterRequest {
            name: "pandt-backend-cognito-password".to_string(),
            with_decryption: Some(true),
        })
        .sync()?;
    let appsync_secret = appsync_secret_param.parameter.ok_or_else(|| failure::format_err!("Couldn't find pandt-backend-cognita-password parameter?"))?.value;

    Ok(start(move |()| Ok(format!("Hello {:?}!", appsync_secret))))
}
