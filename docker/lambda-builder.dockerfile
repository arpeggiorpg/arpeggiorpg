FROM ekidd/rust-musl-builder:beta

ENV USER=root
COPY . ./
CMD cargo build --package pandt_lambda --bin pandt_lambda
