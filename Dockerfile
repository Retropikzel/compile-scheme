FROM debian:trixie-slim AS build
RUN apt-get update && apt-get install -y gcc make git chicken-bin
RUN chicken-install r7rs

WORKDIR /build
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1 chibi

WORKDIR /build/chibi
RUN make DESTDIR=/opt/compile-r7rs
RUN make DESTDIR=/opt/compile-r7rs install

RUN mkdir -p /opt/compile-r7rs/bin
RUN echo "#!/bin/sh" > /opt/compile-r7rs/bin/snow-chibi
RUN echo "PATH=/opt/compile-r7rs/usr/local/bin:${PATH} LD_LIBRARY_PATH=/opt/compile-r7rs/usr/local/lib:/opt/compile-r7rs/usr/local/lib/chibi CHIBI_MODULE_PATH=/opt/compile-r7rs/usr/local/share/chibi:/opt/compile-r7rs/usr/local/lib/chibi /opt/compile-r7rs/usr/local/bin/chibi-scheme -I /opt/compile-r7rs/usr/local/share/chibi -I /opt/compile-r7rs/usr/local/lib/chibi -I /opt/compile/snow -mchibi.snow.commands -mchibi.snow.interface -mchibi.snow.package -mchibi.snow.utils /opt/compile-r7rs/usr/local/bin/snow-chibi.scm \"\$@\"" >> /opt/compile-r7rs/bin/snow-chibi
RUN chmod +x /opt/compile-r7rs/bin/snow-chibi
ENV PATH=/opt/compile-r7rs/bin:${PATH}

WORKDIR /build
RUN snow-chibi install --impls=chicken --always-yes "(foreign c)"
RUN snow-chibi install --impls=chicken --always-yes "(srfi 170)"
COPY Makefile .
COPY compile-r7rs.scm .
COPY libs libs
RUN make build-chicken
RUN make PREFIX=/opt/compile-r7rs install

FROM debian:trixie-slim
RUN apt-get update && apt-get install -y \
    build-essential libffi-dev docker.io make cmake
COPY --from=build /opt/compile-r7rs /opt/compile-r7rs
ENV PATH=/opt/compile-r7rs/bin:${PATH}
RUN mkdir -p ${HOME}/.snow && echo "()" > ${HOME}/.snow/config.scm
