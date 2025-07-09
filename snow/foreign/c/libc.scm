(cond-expand
  (windows
    (define libc-name "ucrtbase"))
  (else
    (define libc-name
      (cond ((get-environment-variable "BE_HOST_CPU") "root") ; Haiku
            (else "c")))))
