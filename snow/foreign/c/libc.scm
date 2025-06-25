(cond-expand
  (windows (define-c-library libc
                             '("stdlib.h" "stdio.h" "string.h")
                             "ucrtbase"
                             '()))
  (else
    (define c-library "c")
    (when (get-environment-variable "BE_HOST_CPU")
      (set! c-library "root"))
    (define-c-library libc
                      '("stdlib.h" "stdio.h" "string.h")
                      "c"
                      '((additional-versions ("0" "6"))))))
