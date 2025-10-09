#!r6rs
(library (foo bar) (export baz) (import (rnrs)) (define baz (lambda () (display "Test successfull "))))