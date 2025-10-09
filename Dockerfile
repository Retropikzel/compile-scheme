FROM debian:trixie-slim
RUN apt-get update && apt-get install -y gcc make git libffi-dev docker.io
WORKDIR /build
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1 chibi
WORKDIR /build/chibi
RUN make
RUN make install
RUN mkdir -p ${HOME}/.snow && echo "()" > ${HOME}/.snow/config.scm
RUN snow-chibi install --impls=chibi --always-yes "(foreign c)"
RUN snow-chibi install --impls=chibi --always-yes "(srfi 170)"
COPY Makefile .
COPY compile-r7rs.scm .
COPY libs libs
RUN make build-chibi
RUN make install
