(define-library
  (libs util)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme char)
          (scheme process-context)
          (foreign c))
  (export echo
          cat
          r6rs-schemes
          r7rs-schemes
          all-schemes
          string-replace
          string-ends-with?
          string-starts-with?
          string-cut-from-end
          string-find
          string-reverse
          string-split
          path->filename
          change-file-suffix
          string-join
          util-getenv
          dirname
          search-library-file
          slurp
          file->list
          trim
          trim-end
          trim-both)
  (include "util.scm"))
