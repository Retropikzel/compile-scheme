(define-library
  (libs srfi-64-util)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme file)
          (libs util))
  (export srfi-64-output-read
          srfi-64-log-results)
  (include "srfi-64-util.scm"))
