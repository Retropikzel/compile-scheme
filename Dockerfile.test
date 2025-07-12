ARG COMPILE_R7RS=chibi
FROM schemers/sagittarius AS build

RUN apt-get update && apt-get install -y wget build-essential make cmake libgc-dev zlib1g-dev libffi-dev libssl-dev
RUN wget https://github.com/ktakashi/sagittarius-scheme/archive/refs/tags/version_0.9.12.tar.gz && tar -xf version_0.9.12.tar.gz
RUN cd sagittarius-scheme-version_0.9.12 && ./dist.sh gen && mkdir build && cd build && cmake -DCMAKE_INSTALL_PREFIX=/usr/local-other .. && make && make install

WORKDIR /workdir
COPY Makefile .
COPY compile-r7rs.scm .
COPY snow/ snow/
COPY libs/ libs/
RUN ls -1
RUN make PREFIX=/usr/local-other && make install PREFIX=/usr/local-other

FROM schemers/${COMPILE_R7RS}
RUN apt-get update && apt-get install -y \
    build-essential \
    make \
    libfcgi-dev \
    sqlite3 \
    libsqlite3-dev \
    libffi8 \
    libffi-dev \
    libgc1 \
    libssl3 \
    libuv1
COPY --from=build /usr/local-other/ /usr/local-other/
ENV PATH=${PATH}:/usr/local-other/bin
ARG COMPILE_R7RS=chibi
ENV COMPILE_R7RS=${COMPILE_R7RS}
