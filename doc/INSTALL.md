## Build and install

You will need to have snow-chibi installed, it comes with Chibi Scheme.

You can run compile-r7rs on Chibi, Chicken, Gauche, Guile, Kawa, Sagittarius or
STklos.

    snow-chibi --impls=SCHEME "(foreign c)"
    snow-chibi --impls=SCHEME "(srfi 170)"
    make build-SCHEME
    make install

