PREFIX=/usr/local
SCHEME=chibi
R6RSTMP=tmp/${SCHEME}-r6rs
R7RSTMP=tmp/${SCHEME}-r7rs
DOCKERIMG=${SCHEME}:head
ifeq "${SCHEME}" "chicken"
DOCKERIMG="chicken:5"
endif

STATIC_LIBS=libs.util.a libs.library-util.a libs.data.a libs.srfi-64-util.a

build-chibi:
	echo "#!/bin/sh" > compile-r7rs
	echo "chibi-scheme -A ${PREFIX}/lib/compile-r7rs ${PREFIX}/lib/compile-r7rs/compile-r7rs.scm \"\$$@\"" >> compile-r7rs

build-chicken:
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

build-gauche:
	echo "#!/bin/sh" > compile-r7rs
	echo "gosh -r -I ${PREFIX}/lib/compile-r7rs -I ${PREFIX}/lib/compile-r7rs/libs ${PREFIX}/lib/compile-r7rs/compile-r7rs.scm \"\$$@\"" >> compile-r7rs

build-guile:
	echo "#!/bin/sh" > compile-r7rs
	echo "guile --r7rs --auto-compile -I -q -L ${PREFIX}/lib/compile-r7rs ${PREFIX}/lib/compile-r7rs/compile-r7rs.scm \"\$$@\" 2> /dev/null" >> compile-r7rs

build-kawa:
	echo "#!/bin/sh" > compile-r7rs
	echo "kawa -J--add-exports=java.base/jdk.internal.foreign.abi=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign.layout=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign=ALL-UNNAMED -J--enable-native-access=ALL-UNNAMED -Dkawa.import.path=/usr/local/share/kawa/lib/*.sld:${PREFIX}/lib/compile-r7rs/*.sld --r7rs ${PREFIX}/lib/compile-r7rs/compile-r7rs.scm \"\$$@\" 2> /dev/null" >> compile-r7rs

build-racket:
	echo "#!/bin/sh" > compile-r7rs
	echo "racket -I r7rs -S ${PREFIX}/lib/compile-r7rs --script ${PREFIX}/lib/compile-r7rs/compile-r7rs.scm \"\$$@\"" >> compile-r7rs

build-sagittarius:
	echo "#!/bin/sh" > compile-r7rs
	echo "sash -A ${PREFIX}/lib/compile-r7rs ${PREFIX}/lib/compile-r7rs/compile-r7rs.scm \"\$$@\"" >> compile-r7rs

build-stklos:
	echo "#!/bin/sh" > compile-r7rs
	echo "stklos -I ${PREFIX}/lib/compile-r7rs ${PREFIX}/lib/compile-r7rs/compile-r7rs.scm \"\$$@\"" >> compile-r7rs

docker-images: build-docker-image-debian build-docker-image-alpine

docker-image-debian:
	docker build . -f Dockerfile --tag=retropikzel1/compile-r7rs:latest

docker-image-debian-push:
	docker push retropikzel1/compile-r7rs:latest

docker-image-alpine:
	docker build . -f Dockerfile.alpine --tag=retropikzel1/compile-r7rs:alpine-latest

docker-image-alpine-push:
	docker push retropikzel1/compile-r7rs:alpine-latest

install:
	mkdir -p ${PREFIX}/bin
	mkdir -p ${PREFIX}/lib/compile-r7rs
	cp -r libs ${PREFIX}/lib/compile-r7rs/
	cp compile-r7rs.scm ${PREFIX}/lib/compile-r7rs/compile-r7rs.scm
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
	-cd ${R6RSTMP} && ./main > compile-r7rs-test-result.txt 2>&1
	@grep "Test successfull" ${R6RSTMP}/compile-r7rs-test-result.txt || (echo "Test failed, output: " && cat ${R6RSTMP}/compile-r7rs-test-result.txt && exit 1)

test-r6rs-docker: build-local-docker
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=compile-r7rs-test-${SCHEME} .
	docker run -v "${PWD}":/workdir -w /workdir -t compile-r7rs-test-${SCHEME} sh -c "make && make install && make SCHEME=${SCHEME} test-r6rs"

test-r7rs:
	rm -rf ${R7RSTMP}
	mkdir -p ${R7RSTMP}
	mkdir -p ${R7RSTMP}/libs
	mkdir -p ${R7RSTMP}/libs/foo
	mkdir -p ${R7RSTMP}/libs/hello
	echo "(import (scheme base) (foo bar) (hello world) (other hellolib)) (baz) (hello-world) (over-9000)" > ${R7RSTMP}/main.scm
	echo "(define baz (lambda () (display \"Test successfull\") (newline)))" > ${R7RSTMP}/libs/foo/bar.scm
	echo "(define-library (foo bar) (import (scheme base) (scheme write) (hello world)) (export baz) (include \"bar.scm\"))" > ${R7RSTMP}/libs/foo/bar.sld
	echo "(define hello-world (lambda () (+ 1 1)))" > ${R7RSTMP}/libs/hello/world.scm
	echo "(define-library (hello world) (import (scheme base) (scheme write)) (export hello-world) (include \"world.scm\"))" > ${R7RSTMP}/libs/hello/world.sld
	mkdir -p ${R7RSTMP}/libs/other
	echo "(define over-9000 (lambda () (+ 1 1)))" > ${R7RSTMP}/libs/other/hellolib.scm
	echo "(define-library (other hellolib) (import (scheme base) (scheme write)) (export over-9000) (include \"hellolib.scm\"))" > ${R7RSTMP}/libs/other/hellolib.sld
	cd ${R7RSTMP} && COMPILE_R7RS=${SCHEME} compile-r7rs -I ./libs -o main main.scm
	-cd ${R7RSTMP} && ./main > compile-r7rs-test-result.txt 2>&1
	@grep "Test successfull" ${R7RSTMP}/compile-r7rs-test-result.txt || (echo "Test failed, output: " && cat ${R7RSTMP}/compile-r7rs-test-result.txt && exit 1)

test-r7rs-docker: build-local-docker
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=compile-r7rs-test-${SCHEME} .
	docker run -v "${PWD}":/workdir -w /workdir -t compile-r7rs-test-${SCHEME} sh -c "make && make install && make SCHEME=${SCHEME} test-r7rs"

clean:
	rm -rf test-r7rs
	rm -rf compile-r7rs
	find . -name "*.so" -delete
	find . -name "*.o*" -delete
	find . -name "*.a*" -delete
	find . -name "*.rkt" -delete
	find . -name "*.link" -delete
	find . -name "*.meta" -delete
	find . -name "*.import.*" -delete
	rm -rf dist
	rm -rf deps


