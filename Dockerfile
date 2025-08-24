FROM debian:trixie-slim AS build
RUN mkdir -p ${HOME}/.snow && echo "()" > ${HOME}/.snow/config.scm
RUN apt-get update && apt-get install -y \
    build-essential \
    ca-certificates \
    git \
    make \
    libffi-dev \
    pandoc \
    chicken-bin \
    libc-dev
RUN mkdir -p /opt/compile-r7rs
RUN mkdir -p /opt/compile-r7rs/bin
RUN mkdir -p /opt/compile-r7rs/lib
RUN chicken-install r7rs
WORKDIR /builddir
COPY Makefile .
COPY compile-r7rs.scm .
COPY libs/ libs/
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1
RUN cd chibi-scheme && make -j 32 && make -j 32 install
RUN make PREFIX=/opt/compile-r7rs build-chicken-static && make PREFIX=/opt/compile-r7rs install

FROM debian:trixie-slim
COPY --from=build /opt/compile-r7rs /opt/compile-r7rs
ENV PATH=/opt/compile-r7rs/bin:${PATH}
