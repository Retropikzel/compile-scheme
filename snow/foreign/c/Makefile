CC=gcc

chibi: foreign/c/primitives/chibi/foreign-c.stub
	chibi-ffi foreign/c/primitives/chibi/foreign-c.stub
	${CC} \
		-g3 \
		-o foreign/c/primitives/chibi/foreign-c.so \
		foreign/c/primitives/chibi/foreign-c.c \
		-fPIC \
		-lffi \
		-shared

chicken:
	@echo "Nothing to build for Chicken"

cyclone:
	@echo "Nothing to build for Cyclone"

gambit:
	@echo "Nothing to build for Gambit"

gauche: primitives/gauche/foreign-c-primitives-gauche.c primitives/gauche/gauchelib.scm
	gauche-package compile \
		--srcdir=primitives/gauche \
		--cc=${CC} \
		--cflags="-I./primitives/include" \
		--libs=-lffi \
		foreign-c-primitives-gauche foreign-c-primitives-gauche.c gauchelib.scm
	mkdir -p lib
	mv foreign-c-primitives-gauche.so lib/gauche.so
	mv foreign-c-primitives-gauche.o lib/gauche.o


gerbil:
	@echo "Nothing to build for Gerbil"

guile:
	@echo "Nothing to build for Guile"

kawa:
	@echo "Nothing to build for Kawa"

larceny:
	@echo "Nothing to build for Larceny"

mosh:
	@echo "Nothing to build for Mosh"

racket:
	@echo "Nothing to build for Racket"

sagittarius:
	@echo "Nothing to build for Sagittarius"

skint:
	@echo "Nothing to build for Skint"

stklos:
	@echo "Nothing to build for Stklos"

tr7:
	@echo "Nothing to build for tr7"

ypsilon:
	@echo "Nothing to build for Ypsilon"

clean:
	@rm -rf primitives/chibi/foreign-c.c
	@rm -rf lib
