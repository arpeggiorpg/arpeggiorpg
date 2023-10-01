FROM lukemathwalker/cargo-chef:latest-rust-1 AS chef
WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder
COPY --from=planner /app/recipe.json recipe.json
# Build dependencies - this is the caching Docker layer!
RUN cargo chef cook --release --recipe-path recipe.json
# Build application
RUN apt-get update && apt-get install -y protobuf-compiler
COPY . .
RUN cargo build --release --bin ptrpi

# We do not need the Rust toolchain to run the binary!
FROM debian:bookworm-slim AS runtime
RUN apt-get update && apt-get install -y libssl3
WORKDIR /app
COPY --from=builder /app/target/release/ptrpi /usr/local/bin
ENTRYPOINT ["/usr/local/bin/ptrpi"]
