(import (scheme base)
        (scheme write)
        (scheme process-context)
        (foo bar)
        (hello world)
        (other hellolib))
(baz)
(hello-world)

(define l (list "1" "2" "3"))
(cond-expand
  ;; Meevax gives too much args
  ;; For this test for now this is okay
  (meevax (when (> (length (command-line)) 3) (write l)))
  ;; mit-scheme gives too much args
  ;; For this test for now this is okay
  (mit (when (> (length (command-line)) 3) (write l)))
  ;; tr7 gives too much args
  ;; For this test for now this is okay
  (tr7 (when (> (length (command-line)) 3) (write l)))
  (else (write (list-tail (command-line) 1))))

(over-9000)
