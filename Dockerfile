FROM debian:trixie-slim AS build
RUN apt-get update && apt-get install -y make gcc chicken-bin git
RUN chicken-install r7rs

WORKDIR /build
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1 chibi

WORKDIR /build/chibi
RUN make
RUN make install

WORKDIR /build
RUN mkdir -p ${HOME}/.snow && echo "()" > ${HOME}/.snow/config.scm
RUN snow-chibi install --always-yes --impls=chicken "(foreign c)"
RUN snow-chibi install --always-yes --impls=chicken "(srfi 170)"
COPY Makefile .
COPY compile-r7rs.scm .
COPY libs ./libs
RUN make PREFIX=/opt/compile-r7rs build-chicken
RUN make PREFIX=/opt/compile-r7rs install

FROM debian:trixie-slim
COPY --from=build /opt/compile-r7rs /opt/compile-r7rs
ENV PATH=/opt/compile-r7rs/bin:${PATH}
