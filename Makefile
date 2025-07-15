PREFIX=/usr/local
SCHEME=chibi
R6RSTMP=tmp/${SCHEME}-r6rs
R7RSTMP=tmp/${SCHEME}-r7rs

all: build

container:
	docker build -f Dockerfile.test --tag=compile-r7rs

build: deps
	echo "#!/bin/sh" > compile-r7rs
	echo "chibi-scheme -A ${PREFIX}/lib/compile-r7rs ${PREFIX}/lib/compile-r7rs/main.scm \$$@" >> compile-r7rs

deps:
	mkdir -p deps
	git clone https://git.sr.ht/~retropikzel/foreign-c deps/foreign-c --depth=1
	git clone https://git.sr.ht/~retropikzel/foreign-c-srfi-170 deps/foreign-c-srfi-170 --depth=1

install:
	cd deps/foreign-c && make all install
	cd deps/foreign-c-srfi-170 && make all install
	mkdir -p ${PREFIX}/lib/compile-r7rs
	cp -r libs ${PREFIX}/lib/compile-r7rs/
	cp compile-r7rs.scm ${PREFIX}/lib/compile-r7rs/main.scm
	install compile-r7rs ${PREFIX}/bin/compile-r7rs

uninstall:
	rm -rf ${PREFIX}/lib/compile-r7rs
	rm -rf ${PREFIX}/bin/compile-r7rs

test-r6rs: ${R6RSTMP}
	mkdir -p ${R6RSTMP}
	mkdir -p ${R6RSTMP}/libs
	printf "(library (foo bar) (export baz) (import (rnrs)) (define baz (lambda () (display \"Test successfull\") (newline))))" > ${R6RSTMP}/libs/foo/bar.sls
	printf "(import (rnrs) (foo bar)) (baz)" > ${R6RSTMP}/main.sps
	cd ${R6RSTMP} && COMPILE_R7RS=${SCHEME} compile-r7rs -I ./libs -o main main.sps
	-cd ${R6RSTMP} && timeout 60 ./main > ${R6RSTMP}/compile-r7rs-test-result.txt 2>&1
	@grep "Test successfull" ${R6RSTMP}/compile-r7rs-test-result.txt || (echo "Test failed, output: " && cat ${R6RSTMP}/compile-r7rs-test-result.txt && exit 1)

test-r6rs-docker:
	docker build -f Dockerfile.test --build-arg SCHEME=${SCHEME} --tag=compile-r7rs-test-${SCHEME} .
	docker run -v "${PWD}":/workdir -w /workdir -t compile-r7rs-test-${SCHEME} sh -c "make && make install && make clean-test SCHEME=${SCHEME} test-r6rs"

test-r7rs:
	mkdir -p ${R7RSTMP}
	mkdir -p ${R7RSTMP}/libs
	echo "(import (scheme base) (foo bar)) (baz)" > ${R7RSTMP}/main.scm
	echo "(define baz (lambda () (display \"Test successfull\") (newline)))" > ${R7RSTMP}/libs/foo/bar.scm
	echo "(define-library (foo bar) (import (scheme base) (scheme write)) (export baz) (include \"bar.scm\"))" > ${R7RSTMP}/libs/foo/bar.sld
	cd ${R7RSTMP} && COMPILE_R7RS=${SCHEME} compile-r7rs -I ./snow -o main main.scm
	-cd ${R7RSTMP} && timeout 60 ./main > ${R7RSTMP}/compile-r7rs-test-result.txt 2>&1
	@grep "Test successfull" ${R7RSTMP}/compile-r7rs-test-result.txt || (echo "Test failed, output: " && cat ${R7RSTMP}/compile-r7rs-test-result.txt && exit 1)

test-r7rs-docker:
	docker build -f Dockerfile.test --build-arg SCHEME=${SCHEME} --tag=compile-r7rs-test-${SCHEME} .
	docker run -v "${PWD}":/workdir -w /workdir -t compile-r7rs-test-${SCHEME} sh -c "make && make install && make clean-test SCHEME=${SCHEME} test-r7rs"

clean-test:
	rm -rf test

clean:
	find . -name "*.so" -delete
	find . -name "*.o*" -delete
	find . -name "*.a*" -delete
	find . -name "*.rkt" -delete
	find . -name "*.link" -delete
	find . -name "*.meta" -delete
	find . -name "*.import.*" -delete
	rm -rf dist
	rm -rf test
	rm -rf deps


