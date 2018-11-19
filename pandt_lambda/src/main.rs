use std::default::Default;

use aws_lambda::start;
use rusoto_core::Region;

fn main() {
    let region = Region::default();
    start(move |()| Ok(format!("Hello {:?}!", region)))
}
