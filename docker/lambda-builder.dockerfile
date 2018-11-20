
##
## Builder
##

# FROM rust-builder-base as builder
FROM ekidd/rust-musl-builder:beta

# RUN mkdir .cargo
# COPY docker/cargo_config .cargo/config

# ENV OPENSSL_DIR=$PREFIX \
#     OPENSSL_STATIC=true

# Some caching tricks! We want all the build dependencies to be cached as long
# as Cargo.lock hasn't changed.
# So we make a bunch of fake, empty packages, but with real Cargo.toml & Cargo.lock,
# and build them. This causes all their dependencies to be built & cached.

ENV USER=root
RUN    cargo new pandt_lambda \
    && cargo new --lib pandt \
    && cargo new --lib foldertree \
    && cargo new --lib nonempty \
    && cargo new --lib indexed \
    && cargo new --bin ptrpi
COPY Cargo.lock Cargo.toml ./
COPY foldertree/Cargo.toml foldertree
COPY nonempty/Cargo.toml nonempty
COPY indexed/Cargo.toml indexed
COPY ptrpi/Cargo.toml ptrpi
COPY pandt/Cargo.toml pandt
COPY pandt_lambda/Cargo.toml pandt_lambda/
RUN cargo build --release --package pandt_lambda

# Ok, now move on to building our actual code.
# We replace the fake crates with our real code.

# We need to delete the *.rlib files for cargo to properly rebuild all the
# dependencies. I believe this is because the "dummy" libraries we created above
# usually  have a *newer* timestamp than the real ones on disk.

RUN rm -rf pandt foldertree nonempty indexed ptrpi \
           target/x86_64-unknown-linux-musl/release/deps/libindexed-*.rlib \
           target/x86_64-unknown-linux-musl/release/deps/libfoldertree-*.rlib \
           target/x86_64-unknown-linux-musl/release/deps/libnonempty-*.rlib

# TODO build times are still really bad when I only change the lambda function.
# it's rebuilding all of pandt. Figure out a way to cache that.
# It might involve building each library individually?
COPY indexed indexed
COPY foldertree foldertree
COPY nonempty nonempty
COPY pandt pandt
COPY ptrpi ptrpi
COPY pandt_lambda pandt_lambda
RUN cargo build --release --package pandt_lambda

RUN mkdir /home/rust/output

RUN find target/x86_64-unknown-linux-musl/release -maxdepth 1 -type f -executable -exec cp '{}' /home/rust/output \;

