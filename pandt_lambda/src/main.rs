use std::default::Default;

use aws_lambda::start;
use rusoto_core::Region;
use rusoto_ssm; // I need to be able to fetch parameters so I can authenticate against AppSync
use rusoto_cognito_identity; // I need to be able to verify cognito tokens
use graphql_client; // I need to send a mutation to AppSync's GraphQL in order to notify waiting clients.

fn main() {

    let region = Region::default();

    // This is our cached "pandt-backend" password from SSM. It should hopefully
    // outlive individual Lambda invocations so we don't have to make an SSM
    // request per Lambda invocation.
    let appsync_secret: Option<String> = None;
    start(move |()| Ok(format!("Hello {:?}!", region)))
}
