FROM ekidd/rust-musl-builder:stable

COPY . ./
CMD cargo build --package pandt_lambda --bin pandt_lambda
