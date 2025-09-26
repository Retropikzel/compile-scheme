FROM debian:trixie-slim AS build
RUN apt-get update && apt-get install -y build-essential ca-certificates wget \
    git autoconf automake libtool texinfo

WORKDIR /build
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1 chibi

WORKDIR /build/chibi
RUN make DESTDIR=/opt/compile-r7rs
RUN make DESTDIR=/opt/compile-r7rs install

WORKDIR /build
RUN echo "#!/bin/sh" > /opt/compile-r7rs/snow-chibi
RUN echo "PATH=/opt/compile-r7rs/usr/local/bin:${PATH} LD_LIBRARY_PATH=/opt/compile-r7rs/usr/local/lib:/opt/compile-r7rs/usr/local/lib/chibi CHIBI_MODULE_PATH=/opt/compile-r7rs/usr/local/share/chibi:/opt/compile-r7rs/usr/local/lib/chibi /opt/compile-r7rs/usr/local/bin/chibi-scheme -I /opt/compile-r7rs/usr/local/share/chibi -I /opt/compile-r7rs/usr/local/lib/chibi -I /opt/compile/snow -mchibi.snow.commands -mchibi.snow.interface -mchibi.snow.package -mchibi.snow.utils /opt/compile-r7rs/usr/local/bin/snow-chibi.scm \"\$@\"" >> /opt/compile-r7rs/snow-chibi
RUN chmod +x /opt/compile-r7rs/snow-chibi

ENV PATH=/opt/compile-r7rs:${PATH}

RUN git clone https://github.com/libffi/libffi.git --branch=v3.5.2 --depth=1
WORKDIR /build/libffi
RUN sh autogen.sh
RUN ./configure --prefix=/usr/local
RUN make DESTDIR=/opt/compile-r7rs PREFIX=/usr/local
RUN make DESTDIR=/opt/compile-r7rs PREFIX=/usr/local install

WORKDIR /build
RUN mkdir -p ${HOME}/.snow && echo "()" > ${HOME}/.snow/config.scm
RUN snow-chibi install \
    --cflags="-I/opt/compile-r7rs/usr/local/include -L/opt/compile-r7rs/usr/local/lib" \
    --install-source-dir=/opt/compile-r7rs/usr/local/share/chibi \
    --install-library-dir=/opt/compile-r7rs/usr/local/lib/chibi \
    "(foreign c)"
RUN snow-chibi install --install-source-dir=/opt/compile-r7rs/usr/local/share/chibi --install-library-dir=/opt/compile-r7rs/usr/local/lib/chibi "(retropikzel system)"
RUN snow-chibi install --install-source-dir=/opt/compile-r7rs/usr/local/share/chibi --install-library-dir=/opt/compile-r7rs/usr/local/lib/chibi "(srfi 170)"

COPY compile-r7rs.scm /opt/compile-r7rs/
COPY test-r7rs.scm /opt/compile-r7rs/
RUN mkdir -p /opt/compile-r7rs/usr/local/share/chibi/libs
COPY libs/*.sld /opt/compile-r7rs/usr/local/share/chibi/libs/
COPY libs/*.scm /opt/compile-r7rs/usr/local/share/chibi/libs/

RUN echo "#!/bin/sh" > /opt/compile-r7rs/compile-r7rs
RUN echo "LD_LIBRARY_PATH=/opt/compile-r7rs/usr/local/lib:/opt/compile-r7rs/usr/local/lib/chibi CHIBI_MODULE_PATH=/opt/compile-r7rs/usr/local/share/chibi:/opt/compile-r7rs/usr/local/lib/chibi /opt/compile-r7rs/usr/local/bin/chibi-scheme -mchibi.snow.commands -mchibi.snow.interface -mchibi.snow.package -mchibi.snow.utils /opt/compile-r7rs/compile-r7rs.scm \"\$@\"" >> /opt/compile-r7rs/compile-r7rs
RUN chmod +x /opt/compile-r7rs/compile-r7rs

RUN echo "#!/bin/sh" > /opt/compile-r7rs/test-r7rs
RUN echo "LD_LIBRARY_PATH=/opt/compile-r7rs/usr/local/lib:/opt/compile-r7rs/usr/local/lib/chibi CHIBI_MODULE_PATH=/opt/compile-r7rs/usr/local/share/chibi:/opt/compile-r7rs/usr/local/lib/chibi /opt/compile-r7rs/usr/local/bin/chibi-scheme -mchibi.snow.commands -mchibi.snow.interface -mchibi.snow.package -mchibi.snow.utils /opt/compile-r7rs/test-r7rs.scm \"\$@\"" >> /opt/compile-r7rs/test-r7rs
RUN chmod +x /opt/compile-r7rs/test-r7rs

FROM debian:trixie-slim
RUN apt-get update && apt-get install -y libffi-dev docker.io locate
COPY --from=build /opt/compile-r7rs /opt/compile-r7rs
RUN updatedb
RUN locate foreign-c.so
ENV PATH=/opt/compile-r7rs:${PATH}
