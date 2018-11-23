use std::default::Default;

use aws_lambda::start;
use rusoto_core::Region;
use rusoto_ssm; // I need to be able to fetch parameters so I can authenticate against AppSync
use graphql_client; // I need to send a mutation to AppSync's GraphQL in order to notify waiting clients.

fn main() {
    let region = Region::default();
    start(move |()| Ok(format!("Hello {:?}!", region)))
}
