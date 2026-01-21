.SILENT: test-r6rs test-r6rs-docker test-r7rs test-r7rs-docker
.PHONY: README.md
PREFIX=/usr/local
SCHEME=chibi
VERSION=1.3.0
R6RSTMP=.tmp/${SCHEME}-r6rs
R7RSTMP=.tmp/${SCHEME}-r7rs
DOCKERTAG=compile-scheme-test-${SCHEME}
DOCKERIMG=${SCHEME}:head
ifeq "${SCHEME}" "chicken"
DOCKERIMG="chicken:5"
endif
ifeq "${SCHEME}" "vicare"
DOCKERIMG="vicare"
endif
STATIC_LIBS=libs.util.a libs.library-util.a libs.implementations.a libs.srfi-64-util.a

all: build

build: build-chicken

README.md: doc/compile-scheme.1
	echo "" > README.md
	cat README_ADDITIONAL.md >> README.md
	echo "# Manual" >> README.md
	echo "<pre>" >> README.md
	echo "$$(MANWIDTH=80 man -l doc/compile-scheme.1)" >> README.md
	echo "</pre>" >> README.md

build-chibi:
	echo "#!/bin/sh" > compile-scheme
	echo "chibi-scheme -A ${PREFIX}/lib/compile-scheme ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

build-chicken:
	csc -R r7rs -X r7rs -static -c -J -unit libs.util -o libs.util.o libs/util.sld
	ar rcs libs.util.a libs.util.o
	csc -R r7rs -X r7rs -static -c -J -unit libs.library-util -o libs.library-util.o libs/library-util.sld
	ar rcs libs.library-util.a libs.library-util.o
	csc -R r7rs -X r7rs -static -c -J -unit libs.implementations -o libs.implementations.o libs/implementations.sld
	ar rcs libs.implementation.a libs.implementations.o
	csc -R r7rs -X r7rs -static \
		-o compile-scheme \
		-uses libs.util \
		-uses libs.library-util \
		-uses libs.implementations \
		-uses foreign.c \
		-uses srfi-170 \
		compile-scheme.scm

deb: build-chicken
	mkdir -p deb/usr/local/bin
	cp compile-scheme deb/usr/local/bin/
	mkdir -p deb/DEBIAN
	printf "Package: compile-scheme\nArchitecture: amd64\nVersion: ${VERSION}\nSection: misc\nMaintainer: Retropikzel <retropikzel@iki.fi>\nDescription: SRFI 138: Compiling Scheme programs to executables - Implementation\n" \
		> deb/DEBIAN/control
	dpkg-deb -b deb
	cp deb.deb compile-scheme-latest.deb
	mv deb.deb compile-scheme-${VERSION}.deb

build-gauche:
	echo "#!/bin/sh" > compile-scheme
	echo "gosh -r7 -I ${PREFIX}/lib/compile-scheme -I ${PREFIX}/lib/compile-scheme/libs ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

build-gauche.bat:
	echo "@echo off" > compile-scheme.bat
	echo "; & @echo off & gosh.exe -r7 \"%~f0\" %* & exit /b" > compile-scheme.bat
	echo "gosh -r7 -I ${PREFIX}/lib/compile-scheme -I ${PREFIX}/lib/compile-scheme/libs ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

build-guile:
	echo "#!/bin/sh" > compile-scheme
	echo "guile --r7rs --auto-compile -I -q -L ${PREFIX}/lib/compile-scheme ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

# FIXME
#build-kawa:
	#echo "#!/bin/sh" > compile-scheme
	#echo "kawa -J--add-exports=java.base/jdk.internal.foreign.abi=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign.layout=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign=ALL-UNNAMED -J--enable-native-access=ALL-UNNAMED -Dkawa.import.path=/usr/local/share/kawa/lib/*.sld:${PREFIX}/lib/compile-scheme/*.sld --r7rs ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\" 2> /dev/null" >> compile-scheme
	#chmod +x compile-scheme

# FIXME
#build-racket:
	#echo "#!/bin/sh" > compile-scheme
	#echo "racket -I r7rs -S ${PREFIX}/lib/compile-scheme --script ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme

build-sagittarius:
	echo "#!/bin/sh" > compile-scheme
	echo "sash -A ${PREFIX}/lib/compile-scheme ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

build-stklos:
	echo "#!/bin/sh" > compile-scheme
	echo "stklos -I ${PREFIX}/lib/compile-scheme ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

install:
	mkdir -p ${PREFIX}/bin
	mkdir -p ${PREFIX}/lib/compile-scheme
	cp -r libs ${PREFIX}/lib/compile-scheme/
	cp compile-scheme.scm ${PREFIX}/lib/compile-scheme/compile-scheme.scm
	install compile-scheme ${PREFIX}/bin/compile-scheme
	mkdir -p ${PREFIX}/share/man/man1
	cp doc/compile-scheme.1 ${PREFIX}/share/man/man1/
	-if [ -d /etc/bash_completion.d ]; then cp bash_completion.sh /etc/bash_completion.d/compile-scheme; fi

uninstall:
	rm -rf ${PREFIX}/lib/compile-scheme
	rm -rf ${PREFIX}/bin/compile-scheme

test-r6rs:
	rm -rf ${R6RSTMP}
	mkdir -p ${R6RSTMP}
	cp -r r6rs-testfiles/* ${R6RSTMP}/
	cd ${R6RSTMP} && COMPILE_R7RS=${SCHEME} compile-scheme -I ./libs -o main --debug main.sps
	cd ${R6RSTMP} && ./main 1 2 3 > test-result.txt
	@grep "Test successfull (\"1\" \"2\" \"3\")" ${R6RSTMP}/test-result.txt || (echo "Test failed, output: " && cat ${R6RSTMP}/test-result.txt && exit 1)

test-r6rs-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKERTAG} --quiet . > /dev/null
	docker run -v "${PWD}":/workdir -w /workdir -t ${DOCKERTAG} sh -c "make SCHEME=${SCHEME} test-r6rs"

test-r7rs:
	rm -rf ${R7RSTMP}
	mkdir -p ${R7RSTMP}
	cp -r r7rs-testfiles/* ${R7RSTMP}/
	cd ${R7RSTMP} && COMPILE_R7RS=${SCHEME} compile-scheme -I ./libs -o main --debug main.scm
	-cd ${R7RSTMP} && ./main 1 2 3 > test-result.txt 2>&1
	@grep "Test successfull (\"1\" \"2\" \"3\")" ${R7RSTMP}/test-result.txt || (echo "Test failed, output: " && cat ${R7RSTMP}/test-result.txt && exit 1)

test-r7rs-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKERTAG} --quiet . > /dev/null
	docker run -v "${PWD}":/workdir -w /workdir -t ${DOCKERTAG} sh -c "make SCHEME=${SCHEME} test-r7rs"

test-r7rs-wine:
	rm -rf ${R7RSTMP}
	mkdir -p ${R7RSTMP}
	cp -r r7rs-testfiles/* ${R7RSTMP}/
	cd ${R7RSTMP} && COMPILE_R7RS=${SCHEME} COMPILE_R7RS_TARGET=windows compile-scheme -I ./libs -o main.bat main.scm
	-cd ${R7RSTMP} && wine main.bat 1 2 3 > test-result.txt 2>&1
	@grep "Test successfull (\"1\" \"2\" \"3\")" ${R7RSTMP}/test-result.txt || (echo "Test failed, output: " && cat ${R7RSTMP}/test-result.txt && exit 1)

test-r7rs-wine-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKERTAG} --quiet . > /dev/null
	docker run -v "${PWD}":/workdir -w /workdir -t ${DOCKERTAG} sh -c "make SCHEME=${SCHEME} test-r7rs-wine"

clean:
	git clean -X -f
