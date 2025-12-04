(define-library
  (libs util)
  (cond-expand
    (gauche
      (import (scheme base)
              (scheme write)
              (scheme file)
              (scheme char)
              (scheme process-context)
              (only (gauche base) sys-system)))
    (else
      (import (scheme base)
              (scheme write)
              (scheme file)
              (scheme char)
              (scheme process-context)
              (foreign c))))
  (export system
          echo
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
          slurp-bytes
          file->list
          trim
          trim-end
          trim-both)
  (begin
    (cond-expand
      (gauche
        (define system sys-system))
      (else
        (define-c-library c-stdlib
                          '("stdlib.h")
                          libc-name
                          '((additional-versions ("6"))))

        (define-c-procedure c-system c-stdlib 'system 'int '(pointer))
        (define (system cmd)
          (c-system (string->c-utf8 cmd)))))

    (define (echo text) (display text) (newline))
    (define (cat path) (for-each (lambda (line) (echo line)) (file->list path)))
    (define r6rs-schemes '(capyscheme
                            chezscheme
                            guile
                            ikarus
                            ironscheme
                            larceny
                            loko
                            mosh
                            racket
                            sagittarius
                            ypsilon))

    (define r7rs-schemes '(capyscheme
                            chibi
                            chicken
                            cyclone
                            ;gambit
                            foment
                            gauche
                            guile
                            kawa
                            larceny
                            loko
                            meevax
                            mit-scheme
                            mosh
                            racket
                            sagittarius
                            skint
                            stklos
                            tr7
                            ypsilon))
    (define all-schemes '(capyscheme
                           chezscheme
                           chibi
                           chicken
                           cyclone
                           ;gambit
                           foment
                           gauche
                           guile
                           ikarus
                           ironscheme
                           kawa
                           larceny
                           loko
                           meevax
                           mit-scheme
                           mosh
                           racket
                           sagittarius
                           skint
                           stklos
                           tr7
                           ypsilon))

    (define util-getenv
      (lambda (name)
        (if (get-environment-variable name)
          (get-environment-variable name)
          "")))

    (define dirname
      (lambda (path)
        (letrec ((looper (lambda (dirpath)
                           (cond ((= (string-length dirpath) 0) dirpath)
                                 ((char=? (string-ref dirpath 0) #\/) (string-copy dirpath 1))
                                 (else (looper (string-copy dirpath 1)))))))
          (string-reverse (looper (string-reverse path))))))

    (define string-replace
      (lambda (string-content replace with)
        (string-map (lambda (c)
                      (if (char=? c replace)
                        with c))
                    string-content)))

    (define string-replace-one
      (lambda (string-content replace with)
        (let ((replaced? #f))
          (string-map (lambda (c)
                        (if (and (not replaced?)
                                 (char=? c replace))
                          with c))
                      string-content))))

    (define string-replace-one-from-end
      (lambda (string-content replace with)
        (let ((replaced? #f))
          (list->string (reverse (map (lambda (c)
                                        (if (and (not replaced?)
                                                 (char=? c replace))
                                          with c))
                                      (reverse (string->list string-content))))))))

    (define string-ends-with?
      (lambda (string-content end)
        (if (and (>= (string-length string-content) (string-length end))
                 (string=? (string-copy string-content
                                        (- (string-length string-content)
                                           (string-length end)))
                           end))
          #t
          #f)))

    (define string-starts-with?
      (lambda (string-content start)
        (if (and (>= (string-length string-content) (string-length start))
                 (string=? (string-copy string-content
                                        0
                                        (string-length start))
                           start))
          #t
          #f)))

    (define string-cut-from-end
      (lambda (string-content cut-length)
        (string-copy string-content
                     0
                     (- (string-length string-content) cut-length))))


    (define string-find
      (lambda (string-content character)
        (letrec* ((string-list (string->list string-content))
                  (looper (lambda (c rest index)
                            (cond ((null? rest) #f)
                                  ((char=? c character) index)
                                  (else (looper (car rest)
                                                (cdr rest)
                                                (+ index 1)))))))
          (looper (car string-list)
                  (cdr string-list)
                  0))))

    (define string-reverse
      (lambda (string-content)
        (list->string (reverse (string->list string-content)))))

    (define (string-split text c)
      (letrec* ((looper (lambda (previous rest result)
                          (if (null? rest)
                            (append result (list previous))
                            (if (char=? (car rest) c)
                              (looper (list)
                                      (cdr rest)
                                      (append result (list previous)))
                              (looper (append previous (list (car rest)))
                                      (cdr rest)
                                      result)))))
                (chars (string->list text)))
        (map list->string (looper (list) chars (list)))))

    (define path->filename
      (lambda (path)
        (let ((last-slash-index (string-find (string-reverse path) #\/)))
          (cond ((not last-slash-index) path)
                (else (string-copy path (- (string-length path)
                                           last-slash-index)))))))

    (define change-file-suffix
      (lambda (path new-suffix)
        (let ((last-dot-index (string-find (string-reverse path) #\.)))
          (cond ((not last-dot-index) path)
                (else (string-append (string-copy path 0
                                                  (- (string-length path)
                                                     last-dot-index
                                                     1))
                                     new-suffix))))))

    (define string-join
      (lambda (string-list between)
        (apply string-append
               (let ((index 0)
                     (size (length string-list)))
                 (map
                   (lambda (item)
                     (cond ((= index 0) item)
                           ((= index size) item)
                           (else (string-append item between))))
                   string-list)))))

    (define search-library-file
      (lambda (directories path)
        (let ((result path))
          (for-each
            (lambda (directory)
              (let ((full-path (string-append directory "/" path)))
                (when (file-exists? full-path)
                  (set! result full-path))))
            directories)
          result)))

    (define (slurp path)
      (letrec* ((looper (lambda (result line)
                          (if (eof-object? line)
                            result
                            (looper (append result (list line)) (read-line))))))
        (with-input-from-file
          path
          (lambda ()
            (apply string-append
                   (map (lambda (line)
                          (string-append line (string #\newline)))
                        (looper (list) (read-line))))))))

    (define (slurp-bytes path)
      (letrec* ((looper (lambda (result bytes)
                          (if (eof-object? bytes)
                            result
                            (looper (bytevector-append result bytes)
                                    (read-bytevector 4000))))))
        (with-input-from-file
          path
          (lambda ()
            (looper (bytevector) (read-bytevector 4000))))))

    (define (file->list path)
      (letrec* ((looper (lambda (result line)
                          (if (eof-object? line)
                            result
                            (looper (append result (list line)) (read-line))))))
        (with-input-from-file
          path
          (lambda ()
            (looper (list) (read-line))))))

    (define (trim text)
      (cond ((not (string? text)) "")
            ((string=? text "") "")
            (else
              (letrec* ((looper (lambda (text)
                                  (if (or (null? text)
                                          (not (char-whitespace? (car text))))
                                    (list->string text)
                                    (looper (cdr text))))))
                (looper  (string->list text))))))

    (define (trim-end text)
      (string-reverse (trim (string-reverse text))))

    (define (trim-both text)
      (let ((trimmed (trim text)))
        (string-reverse (trim (string-reverse trimmed)))))))
