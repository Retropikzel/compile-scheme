
(define (get-number text)
  (let
    ((result
       (trim
         (string-reverse
           (string-copy (string-reverse text) 0 4)))))
    (if (not result)
      ""
      result)))

(define (srfi-64-output-read text)
  (let ((result (list)))
    (for-each
      (lambda (line)
        (cond
          ((not (string? line)) #f)
          ((string-starts-with? line "# of expected passes")
           (set! result (append result
                                (list (cons 'expected-passes
                                            (get-number line))))))
          ((string-starts-with? line "# of unexpected passes")
           (set! result (append result
                                (list (cons 'unexpected-passes
                                            (get-number line))))))
          ((string-starts-with? line "# of expected failures")
           (set! result (append result
                                (list (cons 'expected-failures
                                            (get-number line))))))
          ((string-starts-with? line "# of failures")
           (set! result (append result
                                (list (cons 'failures
                                            (get-number line))))))
          ((string-starts-with? line "# of skipped")
           (set! result (append result
                                (list (cons 'skipped
                                            (get-number line))))))))
      (string-split text #\newline))
    (when (not (assoc 'expected-passes result))
      (set! result (append result (list (cons 'expected-passes "")))))
    (when (not (assoc 'unexpected-passes result))
      (set! result (append result (list (cons 'unexpected-passes "")))))
    (when (not (assoc 'expected-failures result))
      (set! result (append result (list (cons 'expected-failures "")))))
    (when (not (assoc 'failures result))
      (set! result (append result (list (cons 'failures "")))))
    (when (not (assoc 'skipped result))
      (set! result (append result (list (cons 'skipped "")))))
    result))

(define (line->data line)
  (let ((pair (apply cons (map trim-both (string-split line #\:)))))
    (cons (string->symbol (car pair)) (cdr pair))))

(define (read-test-data)
  (letrec
    ((looper
       (lambda (results line count)
         (if (>= count 7)
           results
           (looper (append results
                           (if (string-starts-with? line "Test end")
                             (list)
                             (list (line->data line))))
                   (read-line)
                   (+ count 1))))))
    (looper (list) (read-line) 0)))

(define (srfi-64-log-results path)
  (letrec
    ((looper
       (lambda (results group line)
         (cond
           ((eof-object? line) results)
           ((string-starts-with? line "Group begin:")
            (looper results `(group . ,(cdr (line->data line))) (read-line)))
           ((string-starts-with? line "Test begin:")
            (looper (append results (list (append (list group) (read-test-data))))
                    group
                    (read-line)))
           (else (looper results group (read-line)))))))
    (with-input-from-file
      path
      (lambda () (looper (list) '(group . "") (read-line))))))
