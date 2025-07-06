PREFIX=/usr/local

build:
	csc -R r7rs -X r7rs -static -c -J -unit foreign.c -o foreign.c.o snow/foreign/c.sld
	ar rcs foreign.c.a foreign.c.o
	csc -R r7rs -X r7rs -static -c -J -unit srfi-170 -o srfi-170.o snow/srfi/170.sld
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

# Does uninstall because without that the changes do not seem to update
install: uninstall
	mkdir -p ${PREFIX}/lib/compile-r7rs/snow
	cp -r snow/* ${PREFIX}/lib/compile-r7rs/snow
	cp -r libs ${PREFIX}/lib/compile-r7rs/snow/libs
	cp compile-r7rs.scm ${PREFIX}/lib/compile-r7rs/main.scm
	install compile-r7rs ${PREFIX}/bin/compile-r7rs

snow:
	mkdir -p snow
	cp -r ../foreign-c/foreign snow/
	cp -r ../foreign-c-srfi-170/srfi snow/

clean-snow:
	rm -rf snow

install-compile-r7rs-docker:
	install compile-r7rs-docker.sh ${PREFIX}/bin/compile-r7rs-docker

uninstall:
	rm -rf ${PREFIX}/lib/compile-r7rs/snow
	rm -rf ${PREFIX}/bin/compile-r7rs

dist:
	mkdir -p dist

# Uses wine and innosetup
installer-exe: dist
	cp README.md README.txt
	wine "${HOME}/.wine/drive_c/Program Files (x86)/Inno Setup 6./Compil32.exe" /cc installer.iss

test-r6rs:
	rm -rf /tmp/compile-r7rs-test-result.txt
	mkdir -p test
	mkdir -p test/snow
	mkdir -p test/snow/foo
	printf "#!r6rs\n(library (foo bar) (export baz) (import (rnrs)) (define baz (lambda () (display \"Test successfull\") (newline))))" > test/snow/foo/bar.sls
	printf "#!r6rs\n(import (rnrs) (foo bar)) (baz)" > test/main.sps
	cd test && COMPILE_R7RS=${COMPILE_R7RS} compile-r7rs -I ./snow -o main main.sps
	-cd test && ./main > /tmp/compile-r7rs-test-result.txt 2>&1
	@grep "Test successfull" /tmp/compile-r7rs-test-result.txt || (echo "Test failed, output: " && cat /tmp/compile-r7rs-test-result.txt && exit 1)

test-r6rs-docker:
	docker build --build-arg COMPILE_R7RS=${COMPILE_R7RS} --tag=compile-r7rs-test-${COMPILE_R7RS} .
	docker run -v "${PWD}":/workdir -w /workdir -t compile-r7rs-test-${COMPILE_R7RS} sh -c "make && make install && make clean-test COMPILE_R7RS=${COMPILE_R7RS} test-r6rs"

test-r7rs:
	rm -rf /tmp/compile-r7rs-test-result.txt
	mkdir -p test
	mkdir -p test/snow
	mkdir -p test/snow/foo
	echo "(import (scheme base) (foo bar)) (baz)" > test/main.scm
	echo "(define baz (lambda () (display \"Test successfull\") (newline)))" > test/snow/foo/bar.scm
	echo "(define-library (foo bar) (import (scheme base) (scheme write)) (export baz) (include \"bar.scm\"))" > test/snow/foo/bar.sld
	cd test && COMPILE_R7RS=${COMPILE_R7RS} compile-r7rs -I ./snow -o main main.scm
	-cd test && ./main > /tmp/compile-r7rs-test-result.txt 2>&1
	@grep "Test successfull" /tmp/compile-r7rs-test-result.txt || (echo "Test failed, output: " && cat /tmp/compile-r7rs-test-result.txt && exit 1)

test-r7rs-docker:
	docker build --build-arg COMPILE_R7RS=${COMPILE_R7RS} --tag=compile-r7rs-test-${COMPILE_R7RS} .
	docker run -v "${PWD}":/workdir -w /workdir -t compile-r7rs-test-${COMPILE_R7RS} sh -c "make && make install && make clean-test COMPILE_R7RS=${COMPILE_R7RS} test-r7rs"

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
	rm -rf README.txt
	rm -rf dist
	rm -rf test


