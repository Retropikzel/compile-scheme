(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (scheme process-context)
        (foreign c)
        (libs util)
        (libs data)
        (libs library-util)
        (libs srfi-64-util)
        (srfi 170)
        (retropikzel system))

(define timeout
  (if (get-environment-variable "TEST_R7RS_TIMEOUT")
    (get-environment-variable "TEST_R7RS_TIMEOUT")
    "6000"))

(define input-file
  (let ((input-file #f))
    (for-each
      (lambda (item)
        (when (or (string-ends-with? item ".scm")
                  (string-ends-with? item ".sps"))
          (set! input-file item)))
      (list-tail (command-line) 1))
    input-file))

(define output-file
  (if (member "-o" (command-line))
    (cadr (member "-o" (command-line)))
    (if input-file
      "a.out"
      #f)))

(define stop-on-error?
  (if (member "--stop-on-error" (command-line)) #t #f))

(define stop-on-fail?
  (if (member "--stop-on-fail" (command-line)) #t #f))

(define use-docker-head?
  (if (member "--use-docker-head" (command-line)) #t #f))

(define schemes
  (let ((compile-r7rs (get-environment-variable "COMPILE_R7RS")))
    (cond
      ((not compile-r7rs)
       #f)
      ((not (string? compile-r7rs))
            (error "COMPILE_R7RS is not a string" compile-r7rs))
      ((string=? compile-r7rs "all-r6rs")
       (map symbol->string r6rs-schemes))
      ((string=? compile-r7rs "all-r7rs")
       (map symbol->string r7rs-schemes))
      (else
        (list compile-r7rs)))))
(when (not schemes) (error "Environment variable COMPILE_R7RS not set."))
(when (and (< (length schemes) 2)
           (not (assoc (string->symbol (car schemes)) data)))
  (error "Unsupported implementation" schemes))
(define input-file
  (let ((input-file #f))
    (for-each
      (lambda (item)
        (when (or (string-ends-with? item ".scm")
                  (string-ends-with? item ".sps"))
          (set! input-file item)))
      (list-tail (command-line) 1))
    input-file))
(define filename (string-cut-from-end input-file 3))
(define r6rs?
  (if (and input-file
           (or (string-ends-with? input-file ".sps")
               (string-ends-with? input-file ".sls")))
    #t
    #f))

(define original-arguments
  (apply string-append
         (map
           (lambda (item)
             (string-append item " "))
           (list-tail (command-line) 1))))

(define snow-pkgs
  (let ((pkgs (open-output-string)))
    (for-each
      (lambda (pkg)
        (for-each
          (lambda (i) (display i pkgs))
          `(#\" ,pkg #\" " ")))
      (read
        (open-input-string
          (string-append "((srfi 64) " (util-getenv "SNOW_PKGS") ")"))))
    (get-output-string pkgs)))

(define akku-pkgs
  (let ((pkgs (open-output-string)))
    (for-each
      (lambda (pkg)
        (for-each
          (lambda (i) (display i pkgs))
          `(#\" ,pkg #\" " ")))
      (read
        (open-input-string
          (string-append "((srfi 64) " (util-getenv "AKKU_PKGS") ")"))))
    (get-output-string pkgs)))

(define apt-pkgs (util-getenv "APT_PKGS"))
(define lines ":----------------")
(define cell-width 17)
(define (make-cell text)
  (letrec* ((looper (lambda (result)
                      (if (> (string-length result) cell-width)
                        result
                        (looper (string-append result " "))))))
    (string-append "| " (looper text))))
(define (make-row items)
  (string-append (apply string-append (map make-cell items)) "|"))
(define (string-copy-until text begin-index until-char)
  (letrec* ((end (string->list (string-copy text begin-index)))
            (looper (lambda (c rest result)
                      (if (or (null? rest) (char=? c until-char))
                        result
                        (looper (car rest) (cdr rest) (append result (list c)))))))
    (if (null? end)
      ""
      (list->string (looper (car end) (cdr end) (list))))))

(define (get-test-name run-out)
  (letrec* ((prefix "%%%% Starting test ")
            (prefix-length (string-length prefix))
            (looper (lambda (line)
                      (if (and (not (eof-object? line))
                               (string? line)
                               (> (string-length line) prefix-length)
                               (string=? (string-copy line 0 prefix-length)
                                         prefix))
                        (string-copy-until line prefix-length #\()
                        (when (not (eof-object? line))
                          (looper (read-line)))))))
    (with-input-from-file
      run-out
      (lambda ()
        (trim-both (looper (read-line)))))))

(define (write-dockerfile scheme snow-pkgs akku-pkgs apt-pkgs)
  (let ((dockerfile-path (string-append ".test-r7rs/" scheme "/Dockerfile")))
    (when (file-exists? dockerfile-path) (delete-file dockerfile-path))
    (with-output-to-file
      dockerfile-path
      (lambda ()
        (for-each
          echo
          `("FROM debian:trixie AS build"
            "RUN apt-get update && apt-get install -y git gcc wget make guile-3.0-dev libcurl4-openssl-dev"
            "WORKDIR /cache"
            "RUN git clone https://github.com/ashinn/chibi-scheme.git --depth=1"
            "RUN wget https://gitlab.com/-/project/6808260/uploads/819fd1f988c6af5e7df0dfa70aa3d3fe/akku-1.1.0.tar.gz && tar -xf akku-1.1.0.tar.gz"
            "RUN mv akku-1.1.0 akku"

            "WORKDIR /cache/chibi-scheme"
            "RUN make"

            "WORKDIR /cache/akku"
            "RUN ./configure && make"

            ,(string-append "FROM schemers/"
                            scheme
                            (cond ((and (string=? scheme "chicken")
                                        use-docker-head?)
                                   ":5")
                                  (use-docker-head? ":head")
                                  (else "")))
            ,(string-append
               "RUN apt-get update && apt-get install -y make guile-3.0 libcurl4-openssl-dev " apt-pkgs)
            "RUN mkdir -p ${HOME}/.snow && echo \"()\" > ${HOME}/.snow/config.scm"

            "COPY --from=build /cache /cache"

            "COPY --from=retropikzel1/compile-r7rs /opt/compile-r7rs /opt/compile-r7rs"

            "ENV PATH=/opt/compile-r7rs/bin:${PATH}"
            ,(string-append "ENV COMPILE_R7RS=" scheme)

            "WORKDIR /cache/chibi-scheme"
            "RUN make install"
            "WORKDIR /cache/akku"
            "RUN make install"

            "WORKDIR /akku"

            "RUN akku update"
            ,(string-append "RUN snow-chibi install --always-yes --impls=" scheme " " snow-pkgs)
            ,(string-append "RUN akku install " akku-pkgs)

            "WORKDIR /workdir"))))
    dockerfile-path))

(define (docker-run-cmd tag cmd)
  (string-append "docker run -it -v \"${PWD}:/workdir\" --workdir /workdir "
                 tag " sh -c \"timeout " timeout " " cmd "\""))

(for-each
  (lambda (path) (when (not (file-exists? path)) (create-directory path)))
  `(".test-r7rs" ".test-r7rs/tmp"))

(define timestamp-path ".test-r7rs/timestamp")
(system (string-append "date --iso-8601=minutes --utc > " timestamp-path))
(define timestamp
  (if (file-exists? timestamp-path)
    (with-input-from-file timestamp-path (lambda () (read-line)))
    ""))

(for-each
  echo
  `(,(string-append "# Test report - " output-file)
     ""
     ,(string-append "Timestamp(UTC): " timestamp)
     ""
     "Output files are under .test-r7rs/output"
     "Log files are under .test-r7rs/logs"
     "Any other output is under .test-r7rs/tmp for debugging"
     ,(string-append "Timeout: " timeout)
     ""
     ;"Exit code 124 means timed out."
     ""
     "First run may take a while as docker containers are being built"
     ""
     ,(make-row '("Implementation"
                  "Passes"
                  "Unexpected passes"
                  "Failures"
                  "Expected failures"
                  "Skipped tests"
                  "Build exit code"
                  "Run exit code"))
     ,(make-row (list lines lines lines lines lines lines lines lines))))

(for-each
  (lambda (scheme)
    (display (make-cell scheme))
    (flush-output-port)
    (let*
      ((scheme-dir (let ((path (string-append ".test-r7rs/" scheme)))
                     (when (not (file-exists? path)) (create-directory path))
                     path))
       (scheme-log-dir (let ((path (string-append scheme-dir "/logs")))
                         (when (not (file-exists? path)) (create-directory path))
                         path))
       (dockerfile-path (write-dockerfile scheme snow-pkgs akku-pkgs apt-pkgs))
       (docker-tag
         (string-append "test-r7rs-" scheme "-run"))
       (docker-build-out
         (string-append ".test-r7rs/tmp/" scheme "-last-docker-build"))
       (docker-build-cmd
         (string-append "docker build"
                        " -f " dockerfile-path
                        " --tag=" docker-tag
                        " > " docker-build-out " 2>&1"))
       (build-out
         (string-append ".test-r7rs/tmp/" scheme "-last-build"))
       (build-cmd
         (docker-run-cmd docker-tag
                         (string-append
                           "compile-r7rs -I /akku/.akku/lib "
                           original-arguments
                           (string-append " > " build-out " 2>&1"))))
       (run-out
         (string-append ".test-r7rs/tmp/" scheme "-last-run"))
       (run-cmd
         (docker-run-cmd docker-tag
                         (string-append
                           "./" output-file
                           (string-append " > " run-out " 2>&1")))))
      (when (file-exists? build-out) (delete-file build-out))
      (when (file-exists? run-out) (delete-file run-out))
      (when (not (= (system docker-build-cmd) 0))
        (display "Docker container build failed")
        (newline)
        (display "Command: ")
        (display docker-build-cmd)
        (newline)
        (display "Output: ")
        (newline)
        (cat docker-build-out)
        (newline)
        (exit 1))
      (let* ((build-exit-code (number->string (system build-cmd)))
             (run-exit-code (number->string (system run-cmd)))
             (testname (if (and (string? run-exit-code)
                                (not (string=? run-exit-code "0")))
                         ""
                         (get-test-name run-out)))
             (logfile (string-append testname ".log"))
             (scheme-docker-build-out (string-append scheme-log-dir "/" output-file "-docker.log"))
             (scheme-build-out (string-append scheme-log-dir "/" output-file "-build.log"))
             (scheme-run-out (string-append scheme-log-dir "/" output-file "-run.log"))
             (scheme-results-out (string-append scheme-log-dir "/" output-file "-results.log"))
             (short-test-results (srfi-64-output-read (if (file-exists? run-out) (slurp run-out) "")))
             (passes (cdr (assoc 'expected-passes short-test-results)))
             (failures (cdr (assoc 'failures short-test-results)))
             (unexpected-passes (cdr (assoc 'unexpected-passes short-test-results)))
             (expected-failures (cdr (assoc 'expected-failures short-test-results)))
             (skipped (cdr (assoc 'skipped short-test-results)))
             (test-results (srfi-64-log-results logfile)))

          (system (string-append "mv " docker-build-out " " scheme-docker-build-out " > /dev/null 2>&1"))
          (system (string-append "mv " build-out " " scheme-build-out " > /dev/null 2>&1"))
          (system (string-append "mv " run-out " " scheme-run-out " > /dev/null 2>&1"))
        (when (not (string=? testname ""))
          (system (string-append "mv " logfile " " scheme-results-out " > /dev/null 2>&1")))

        (echo
          (make-row
            (list passes
                  unexpected-passes
                  failures
                  expected-failures
                  skipped
                  build-exit-code
                  run-exit-code)))

        (when stop-on-error?
          (when (not (string=? build-exit-code "0"))
            (display "Error on build:")
            (newline)
            (display scheme-build-out)
            (display ": ")
            (newline)
            (cat scheme-build-out)
            (exit 1))
          (when (not (string=? run-exit-code "0"))
            (display "Error on run:")
            (newline)
            (display scheme-run-out)
            (display ": ")
            (newline)
            (cat scheme-run-out)
            (exit 1)))
        (when stop-on-fail?
          (when (and (string->number failures) (> (string->number failures) 0))
            (let ((pretty-print (lambda (pair)
                                  (display (car pair))
                                  (display ": ")
                                  (display (cdr pair))
                                  (newline))))
              (display "Test failures:")
              (newline)
              (for-each
                (lambda (result)
                  (when (string=? (cdr (assoc 'result-kind result)) "fail")
                    (pretty-print (assq 'test-name result))
                    (for-each
                      (lambda (item)
                        (when (not (equal? (car item) 'test-name))
                          (display "  ")
                          (pretty-print item)))
                      (cdr result))
                    (newline)))
                test-results)
              (exit 1)))))))
  schemes)

