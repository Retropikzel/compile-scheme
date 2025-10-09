#!r6rs
(import (rnrs)
        (rnrs programs)
        (foo bar))
(baz)
(write (list-tail (command-line) 1))