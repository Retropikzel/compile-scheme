PREFIX=/usr/local
SCHEME=chibi
R6RSTMP=tmp/${SCHEME}-r6rs
R7RSTMP=tmp/${SCHEME}-r7rs
DOCKERIMG=${SCHEME}
ifeq "${SCHEME}" "chicken"
DOCKERIMG="chicken:5"
endif

all: build

container:
	docker build -f Dockerfile.test --tag=compile-r7rs

build: deps
	echo "#!/bin/sh" > compile-r7rs
	echo "chibi-scheme -A ${PREFIX}/lib/compile-r7rs ${PREFIX}/lib/compile-r7rs/main.scm \"\$$@\"" >> compile-r7rs

build-chicken-static: deps
	csc -R r7rs -X r7rs -I snow/foreign/c -static -c -J -unit foreign.c -o foreign.c.o deps/foreign-c/foreign/c.sld
	ar rcs foreign.c.a foreign.c.o
	csc -R r7rs -X r7rs -static -c -J -unit srfi-170 -o srfi-170.o deps/foreign-c-srfi-170/srfi/170.sld
	ar rcs srfi-170.a srfi-170.o
	csc -R r7rs -X r7rs -static -c -J -unit libs.util -o libs.util.o libs/util.sld
	ar rcs libs.util.a libs.util.o
	csc -R r7rs -X r7rs -static -c -J -unit libs.library-util -o libs.library-util.o libs/library-util.sld
	ar rcs libs.library-util.a libs.library-util.o
	csc -R r7rs -X r7rs -static -c -J -unit libs.data -o libs.data.o libs/data.sld
	ar rcs libs.data.a libs.data.o
	csc -R r7rs -X r7rs -static \
		-o compile-r7rs \
		-uses libs.util \
		-uses libs.library-util \
		-uses libs.data \
		-uses foreign.c \
		-uses srfi-170 \
		compile-r7rs.scm

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

test-r6rs:
	rm -rf ${R6RSTMP}
	mkdir -p ${R6RSTMP}
	mkdir -p ${R6RSTMP}/libs
	mkdir -p ${R6RSTMP}/libs/foo
	printf "#!r6rs\n(library (foo bar) (export baz) (import (rnrs)) (define baz (lambda () (display \"Test successfull\") (newline))))" > ${R6RSTMP}/libs/foo/bar.sls
	printf "#!r6rs\n(import (rnrs) (foo bar)) (baz)" > ${R6RSTMP}/main.sps
	cd ${R6RSTMP} && COMPILE_R7RS=${SCHEME} compile-r7rs -I ./libs -o main main.sps
	-cd ${R6RSTMP} && timeout 60 ./main > compile-r7rs-test-result.txt 2>&1
	@grep "Test successfull" ${R6RSTMP}/compile-r7rs-test-result.txt || (echo "Test failed, output: " && cat ${R6RSTMP}/compile-r7rs-test-result.txt && exit 1)

test-r6rs-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=compile-r7rs-test-${SCHEME} .
	docker run -v "${PWD}":/workdir -w /workdir -t compile-r7rs-test-${SCHEME} sh -c "make && make install && make SCHEME=${SCHEME} test-r6rs"

test-r7rs:
	rm -rf ${R7RSTMP}
	mkdir -p ${R7RSTMP}
	mkdir -p ${R7RSTMP}/libs
	mkdir -p ${R7RSTMP}/libs/foo
	mkdir -p ${R7RSTMP}/libs/hello
	echo "(import (scheme base) (foo bar) (hello world) (srfi 9001)) (baz) (hello-world) (over-9000)" > ${R7RSTMP}/main.scm
	echo "(define baz (lambda () (display \"Test successfull\") (newline)))" > ${R7RSTMP}/libs/foo/bar.scm
	echo "(define-library (foo bar) (import (scheme base) (scheme write) (hello world)) (export baz) (include \"bar.scm\"))" > ${R7RSTMP}/libs/foo/bar.sld
	echo "(define hello-world (lambda () (+ 1 1)))" > ${R7RSTMP}/libs/hello/world.scm
	echo "(define-library (hello world) (import (scheme base) (scheme write)) (export hello-world) (include \"world.scm\"))" > ${R7RSTMP}/libs/hello/world.sld
	mkdir -p ${R7RSTMP}/libs/srfi
	echo "(define over-9000 (lambda () (+ 1 1)))" > ${R7RSTMP}/libs/srfi/9001.scm
	echo "(define-library (srfi 9001) (import (scheme base) (scheme write)) (export over-9000) (include \"9001.scm\"))" > ${R7RSTMP}/libs/srfi/9001.sld
	cd ${R7RSTMP} && COMPILE_R7RS=${SCHEME} compile-r7rs -I ./libs -o main main.scm
	-cd ${R7RSTMP} && timeout 60 ./main > compile-r7rs-test-result.txt 2>&1
	@grep "Test successfull" ${R7RSTMP}/compile-r7rs-test-result.txt || (echo "Test failed, output: " && cat ${R7RSTMP}/compile-r7rs-test-result.txt && exit 1)

test-r7rs-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=compile-r7rs-test-${SCHEME} .
	docker run -v "${PWD}":/workdir -w /workdir -t compile-r7rs-test-${SCHEME} sh -c "make && make install && make SCHEME=${SCHEME} test-r7rs"

clean:
	find . -name "*.so" -delete
	find . -name "*.o*" -delete
	find . -name "*.a*" -delete
	find . -name "*.rkt" -delete
	find . -name "*.link" -delete
	find . -name "*.meta" -delete
	find . -name "*.import.*" -delete
	rm -rf dist
	rm -rf deps


