(define-library
  (libs util)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)
          (foreign c))
  (export string-replace
          string-ends-with?
          string-starts-with?
          string-cut-from-end
          string-find
          string-reverse
          path->filename
          change-file-suffix
          string-join
          util-getenv
          dirname
          search-library-file)
  (begin

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
          result)))))
