PREFIX=/usr/local
SCHEME=chibi
VERSION=1.0.0
R6RSTMP=tmp/${SCHEME}-r6rs
R7RSTMP=tmp/${SCHEME}-r7rs
DOCKERTAG=compile-scheme-test-${SCHEME}
DOCKERIMG=${SCHEME}:head
ifeq "${SCHEME}" "chicken"
DOCKERIMG="chicken:5"
endif
ifeq "${SCHEME}" "vicare"
DOCKERIMG="vicare"
endif
STATIC_LIBS=libs.util.a libs.library-util.a libs.data.a libs.srfi-64-util.a

all: build-chibi

README.md: doc/compile-scheme.1
	printf "<pre>\n$$(MANWIDTH=80 man -l doc/compile-scheme.1)\n</pre>" > README.md

replace-version:
	sed 's/DEVELOPMENT_VERSION/${VERSION}/' compile-scheme.scm

build-chibi: replace-version
	echo "#!/bin/sh" > compile-scheme
	echo "chibi-scheme -A ${PREFIX}/lib/compile-scheme ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

build-chicken: replace-version
	csc -R r7rs -X r7rs -static -c -J -unit libs.util -o libs.util.o libs/util.sld
	ar rcs libs.util.a libs.util.o
	csc -R r7rs -X r7rs -static -c -J -unit libs.library-util -o libs.library-util.o libs/library-util.sld
	ar rcs libs.library-util.a libs.library-util.o
	csc -R r7rs -X r7rs -static -c -J -unit libs.data -o libs.data.o libs/data.sld
	ar rcs libs.data.a libs.data.o
	csc -R r7rs -X r7rs -static \
		-o compile-scheme \
		-uses libs.util \
		-uses libs.library-util \
		-uses libs.data \
		-uses foreign.c \
		-uses srfi-170 \
		compile-scheme.scm

deb: build-chicken
	mkdir -p deb/bin
	cp compile-scheme deb/bin/
	mkdir -p deb/DEBIAN
	printf "Package: compile-scheme\nArchitecture: amd64\nVersion: ${VERSION}\nSection: misc\nMaintainer: Retropikzel <retropikzel@iki.fi>\nDescription: SRFI 138: Compiling Scheme programs to executables - Implementation" \
		> deb/DEBIAN/control
	dpkg-deb -b deb

# FIXME
#build-gauche: replace-version
	#echo "#!/bin/sh" > compile-scheme
	#echo "gosh -r -I ${PREFIX}/lib/compile-scheme -I ${PREFIX}/lib/compile-scheme/libs ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compilescheme-
	#chmod +x compile-scheme

build-guile: replace-version
	echo "#!/bin/sh" > compile-scheme
	echo "guile --r7rs --auto-compile -I -q -L ${PREFIX}/lib/compile-scheme ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

# FIXME
#build-kawa: replace-version
	#echo "#!/bin/sh" > compile-scheme
	#echo "kawa -J--add-exports=java.base/jdk.internal.foreign.abi=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign.layout=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign=ALL-UNNAMED -J--enable-native-access=ALL-UNNAMED -Dkawa.import.path=/usr/local/share/kawa/lib/*.sld:${PREFIX}/lib/compile-scheme/*.sld --r7rs ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\" 2> /dev/null" >> compile-scheme
	#chmod +x compile-scheme

# FIXME
#build-racket: replace-version
	#echo "#!/bin/sh" > compile-scheme
	#echo "racket -I r7rs -S ${PREFIX}/lib/compile-scheme --script ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme

build-sagittarius: replace-version
	echo "#!/bin/sh" > compile-scheme
	echo "sash -A ${PREFIX}/lib/compile-scheme ${PREFIX}/lib/compile-scheme/compile-scheme.scm \"\$$@\"" >> compile-scheme
	chmod +x compile-scheme

build-stklos: replace-version
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
	if [ -d /etc/bash_completion.d ]; then cp bash_completion.sh /etc/bash_completion.d/compile-scheme; fi

uninstall:
	rm -rf ${PREFIX}/lib/compile-scheme
	rm -rf ${PREFIX}/bin/compile-scheme

test-r6rs:
	rm -rf ${R6RSTMP}
	mkdir -p ${R6RSTMP}
	cp -r r6rs-testfiles/* ${R6RSTMP}/
	cd ${R6RSTMP} && COMPILE_R7RS=${SCHEME} compile-scheme -I ./libs -o main main.sps
	cd ${R6RSTMP} && ./main 1 2 3 > test-result.txt
	@grep "Test successfull (\"1\" \"2\" \"3\")" ${R6RSTMP}/test-result.txt || (echo "Test failed, output: " && cat ${R6RSTMP}/test-result.txt && exit 1)

test-r6rs-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKERTAG} .
	docker run -v "${PWD}":/workdir -w /workdir -t ${DOCKERTAG} sh -c "make SCHEME=${SCHEME} test-r6rs"

test-r6rs-php:
	rm -rf ${R6RSTMP}
	mkdir -p ${R6RSTMP}
	cp -r r6rs-php-testfiles/* ${R6RSTMP}/
	cd ${R6RSTMP} && COMPILE_R7RS=${SCHEME} compile-scheme -t php -o main.php main.sps
	-cd ${R6RSTMP} && php main.php > test-result.txt 2>&1
	@grep "Test successfull" ${R6RSTMP}/test-result.txt || (echo "Test failed, output: " && cat ${R6RSTMP}/test-result.txt && exit 1)

test-r6rs-php-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKERTAG} .
	docker run -v "${PWD}":/workdir -w /workdir -t ${DOCKERTAG} sh -c "make SCHEME=${SCHEME} test-r6rs-php"

test-r7rs:
	rm -rf ${R7RSTMP}
	mkdir -p ${R7RSTMP}
	cp -r r7rs-testfiles/* ${R7RSTMP}/
	cd ${R7RSTMP} && COMPILE_R7RS=${SCHEME} compile-scheme -I ./libs main.scm
	-cd ${R7RSTMP} && ./main 1 2 3 > test-result.txt 2>&1
	@grep "Test successfull (\"1\" \"2\" \"3\")" ${R7RSTMP}/test-result.txt || (echo "Test failed, output: " && cat ${R7RSTMP}/test-result.txt && exit 1)

test-r7rs-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKERTAG} .
	docker run -v "${PWD}":/workdir -w /workdir -t ${DOCKERTAG} sh -c "make SCHEME=${SCHEME} test-r7rs"

test-r7rs-wine:
	rm -rf ${R7RSTMP}
	mkdir -p ${R7RSTMP}
	cp -r r7rs-testfiles/* ${R7RSTMP}/
	cd ${R7RSTMP} && COMPILE_R7RS=${SCHEME} COMPILE_R7RS_TARGET=windows compile-scheme -I ./libs -o main.bat main.scm
	-cd ${R7RSTMP} && wine main.bat 1 2 3 > test-result.txt 2>&1
	@grep "Test successfull (\"1\" \"2\" \"3\")" ${R7RSTMP}/test-result.txt || (echo "Test failed, output: " && cat ${R7RSTMP}/test-result.txt && exit 1)

test-r7rs-wine-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKERTAG} .
	docker run -v "${PWD}":/workdir -w /workdir -t ${DOCKERTAG} sh -c "make SCHEME=${SCHEME} test-r7rs-wine"

test-r7rs-php:
	rm -rf ${R7RSTMP}
	mkdir -p ${R7RSTMP}
	cp -r r7rs-php-testfiles/* ${R7RSTMP}/
	cd ${R7RSTMP} && COMPILE_R7RS=${SCHEME} COMPILE_R7RS_TARGET=php compile-scheme -o main.php main.scm
	-cd ${R7RSTMP} && php main.php > test-result.txt 2>&1
	@grep "Test successfull" ${R7RSTMP}/test-result.txt || (echo "Test failed, output: " && cat ${R7RSTMP}/test-result.txt && exit 1)

test-r7rs-php-docker:
	docker build -f Dockerfile.test --build-arg IMAGE=${DOCKERIMG} --build-arg SCHEME=${SCHEME} --tag=${DOCKERTAG} .
	docker run -v "${PWD}":/workdir -w /workdir -t ${DOCKERTAG} sh -c "make SCHEME=${SCHEME} test-r7rs-php"

clean:
	git clean -X -f
