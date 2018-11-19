
##
## Builder
##

FROM rust-builder-base as builder

RUN mkdir .cargo
COPY docker/cargo_config .cargo/config

ENV OPENSSL_DIR=$PREFIX \
    OPENSSL_STATIC=true

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
COPY Cargo.lock .
COPY pandt_lambda/Cargo.toml pandt_lambda/
COPY pandt/Cargo.toml pandt/
COPY foldertree/Cargo.toml foldertree
COPY nonempty/Cargo.toml nonempty
COPY indexed/Cargo.toml indexed
COPY ptrpi/Cargo.toml ptrpi
COPY Cargo.toml .
RUN cargo build --release --package pandt_lambda

# Ok, now move on to building our actual code.
# We replace the fake crates with our real code.

# I have no idea why I need to delete all the *.rlib files, but cargo isn't
# rebuilding my local dependencies if I don't, which leads to pandt compile
# errors (unable to `use foldertree::FolderPath` because it's still trying to
# use the old blank lib). But pandt was fine????
# TODO: investigate timestamp shenanigans
RUN rm -rf pandt foldertree nonempty indexed ptrpi \
           target/x86_64-unknown-linux-musl/release/deps/libindexed-*.rlib \
           target/x86_64-unknown-linux-musl/release/deps/libfoldertree-*.rlib \
           target/x86_64-unknown-linux-musl/release/deps/libnonempty-*.rlib

# TODO build times are still really bad when I only change the lambda function.
# it's rebuilding all of pandt. Figure out a way to cache that.
# It might involve COPYing much smaller chunks progressively?
COPY . /build
RUN cargo build --target $BUILD_TARGET --release --package pandt_lambda

RUN find target/$BUILD_TARGET/release -maxdepth 1 -type f -executable -exec cp '{}' $OUTPUT_DIR \;


##
## Package
##

FROM amazonlinux:latest AS package

ENV OUTPUT_DIR=/output \
    ARTIFACTS_DIR=/artifacts

RUN mkdir -p $ARTIFACTS_DIR

COPY --from=builder $OUTPUT_DIR $ARTIFACTS_DIR

WORKDIR $ARTIFACTS_DIR

RUN yum -y install zip

RUN find . -maxdepth 1 -type f -executable -exec zip aws_lambda.zip '{}' \;

RUN ls -a $ARTIFACTS_DIR


##
## Final
##

FROM package

ENV ARTIFACTS_DIR=/artifacts \
    EXPORT_DIR=/export

RUN mkdir -p $EXPORT_DIR

#Snapshot the directory
VOLUME $EXPORT_DIR

CMD find $ARTIFACTS_DIR -type f -name "aws_lambda.zip" -exec cp '{}' $EXPORT_DIR \;
