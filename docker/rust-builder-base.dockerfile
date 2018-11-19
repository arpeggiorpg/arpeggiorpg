##
## Base
##

FROM amazonlinux:latest as rust-builder-base

ENV BUILD_DIR=/build \
    OUTPUT_DIR=/output \
    RUST_BACKTRACE=1 \
    RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH \
    PREFIX=/musl \
    MUSL_VERSION=1.1.19 \
    OPENSSL_VERSION=1.1.0i

RUN mkdir -p /usr/local/cargo/bin \
  && mkdir -p $BUILD_DIR \
  && mkdir -p $OUTPUT_DIR \
  && mkdir -p $PREFIX

RUN curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain beta -y

ENV BUILD_TARGET=x86_64-unknown-linux-musl

RUN rustup target add $BUILD_TARGET

RUN yum -y groupinstall "Development Tools"

WORKDIR $PREFIX

# Build any dependencies that aren't part of your build, e.g. thrift compiler

# Build Musl
ADD http://www.musl-libc.org/releases/musl-$MUSL_VERSION.tar.gz .
RUN tar -xvzf musl-$MUSL_VERSION.tar.gz \
    && cd musl-$MUSL_VERSION \
    && ./configure --prefix=$PREFIX \
    && make install \
    && cd ..

# Set environment for musl
ENV CC=$PREFIX/bin/musl-gcc \
    C_INCLUDE_PATH=$PREFIX/include/ \
    CPPFLAGS=-I$PREFIX/include \
    LDFLAGS=-L$PREFIX/lib

# Build OpenSSL
ADD https://www.openssl.org/source/openssl-$OPENSSL_VERSION.tar.gz .

RUN echo "Building OpenSSL" \
    && tar -xzf "openssl-$OPENSSL_VERSION.tar.gz" \
    && cd openssl-$OPENSSL_VERSION \
    && ./Configure no-async no-afalgeng no-shared no-zlib -fPIC --prefix=$PREFIX --openssldir=$PREFIX/ssl linux-x86_64 \
    && make depend \
    && make install

WORKDIR $BUILD_DIR

