FROM schemers/chibi:head
RUN apt-get update && apt-get install -y \
    build-essential ca-certificates git make libffi-dev
RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1 \
    && cd chibi-scheme && make -j 16 && make -j 16 install
WORKDIR /builddir
COPY Makefile .
COPY compile-r7rs.scm .
COPY libs/ libs/
RUN make && make install
WORKDIR /workdir
