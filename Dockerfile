FROM schemers/chicken:5 AS build
RUN mkdir -p ${HOME}/.snow && echo "()" > ${HOME}/.snow/config.scm
RUN apt-get update && apt-get install -y \
    gcc wget ca-certificates xz-utils make git libffi-dev unzip lbzip2 cmake \
    g++ python3 locate zlib1g-dev
WORKDIR /build
ENV PATH=/opt/compile-r7rs/bin:${PATH}
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1
RUN cd chibi-scheme && make DESTDIR=/opt/compile-r7rs/chibi all
RUN cd chibi-scheme && make DESTDIR=/opt/compile-r7rs/chibi install
RUN mkdir -p /opt/compile-r7rs/bin
RUN echo "#!/bin/sh" > /opt/compile-r7rs/bin/snow-chibi
RUN echo "PATH=${PATH}:/opt/compile-r7rs/chibi/usr/local/bin CHIBI_MODULE_PATH=/opt/compile-r7rs/chibi/usr/local/share/chibi:/opt/compile-r7rs/chibi/usr/local/lib/chibi LD_LIBRARY_PATH=/opt/compile-r7rs/chibi/usr/local/lib exec /opt/compile-r7rs/chibi/usr/local/bin/chibi-scheme -mchibi.snow.commands -mchibi.snow.interface -mchibi.snow.package -mchibi.snow.utils /opt/compile-r7rs/chibi/usr/local/bin/snow-chibi.scm \"\$@\"" >> /opt/compile-r7rs/bin/snow-chibi
RUN chmod +x /opt/compile-r7rs/bin/snow-chibi
ENV SCHEME=chicken
RUN snow-chibi --impls=${SCHEME} --always-yes install "(foreign c)"
RUN snow-chibi --impls=${SCHEME} --always-yes install "(srfi 170)"
COPY Makefile .
COPY compile-r7rs.scm .
COPY libs libs
RUN make PREFIX=/opt/compile-r7rs build-static
RUN make PREFIX=/opt/compile-r7rs install

FROM debian:trixie-slim
COPY --from=build /opt/compile-r7rs /opt/compile-r7rs
ENV PATH=${PATH}:/opt/compile-r7rs/bin
ENV LD_LIBRARY_PATH=/opt/compile-r7rs/lib
