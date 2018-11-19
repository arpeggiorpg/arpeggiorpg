
##
## Builder
##

FROM rust-builder-base as builder

ARG SRC
ARG CRATE

RUN mkdir .cargo
ADD docker/cargo_config .cargo/config

ENV OPENSSL_DIR=$PREFIX \
    OPENSSL_STATIC=true

ADD $SRC/ .

RUN cargo build --target $BUILD_TARGET --release --package $CRATE

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
