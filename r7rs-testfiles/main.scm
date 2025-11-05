(import (scheme base)
        (scheme write)
        (scheme process-context)
        (foo bar)
        (hello world)
        (other hellolib))
(baz)
(hello-world)

(define l (list "1" "2" "3"))

;; Implementations are allowed to give diffrent amout of args
(cond-expand
  (meevax (when (> (length (command-line)) 3) (write l)))
  (mit (when (> (length (command-line)) 3) (write l)))
  (tr7 (when (> (length (command-line)) 3) (write l)))
  (else (write (list-tail (command-line) 1))))

(over-9000)
