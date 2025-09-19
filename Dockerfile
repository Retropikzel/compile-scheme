FROM schemers/chicken:5 AS build
RUN apt-get update && apt-get install -y \
    gcc wget ca-certificates xz-utils make git libffi-dev unzip lbzip2 cmake \
    g++ python3 locate zlib1g-dev
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1
WORKDIR /chibi-scheme
RUN make
RUN make install

WORKDIR /build

ENV SCHEME=chicken
RUN snow-chibi --impls=${SCHEME} --always-yes install "(foreign c)"
RUN snow-chibi --impls=${SCHEME} --always-yes install "(srfi 170)"

COPY Makefile .
COPY compile-r7rs.scm .
COPY test-r7rs.sh .
COPY libs libs

RUN make PREFIX=/opt/compile-r7rs build-static
RUN make PREFIX=/opt/compile-r7rs install

FROM debian:trixie-slim
COPY --from=build /opt/compile-r7rs /opt/compile-r7rs
ENV PATH=/opt/compile-r7rs/bin:${PATH}
