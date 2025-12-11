FROM debian:trixie
RUN apt-get update && apt-get install -y \
    gcc make git libffi-dev docker.io
WORKDIR /cache
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1
WORKDIR /cache/chibi-scheme
RUN make
RUN make install
WORKDIR /cache
RUN mkdir -p ${HOME}/.snow && echo "()" > ${HOME}/.snow/config.scm
RUN snow-chibi install --always-yes "(foreign c)"
RUN snow-chibi install --always-yes "(srfi 170)"
COPY Makefile .
COPY compile-r7rs.scm .
COPY libs libs
RUN make build-chibi
RUN make install
