(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (scheme process-context)
        (foreign c)
        (libs util)
        (libs data)
        (libs library-util)
        (srfi 170))

(when (or (member "--list-r6rs" (command-line))
        (member "--list-r6rs-schemes" (command-line)))
  (for-each (lambda (scheme) (display scheme) (display " ")) r6rs-schemes)
  (newline)
  (exit 0))

(when (or (member "--list-r7rs" (command-line))
          (member "--list-r7rs-schemes" (command-line)))
  (for-each (lambda (scheme) (display scheme) (display " ")) r7rs-schemes)
  (newline)
  (exit 0))

(when (or (member "--list" (command-line))
          (member "--list-schemes" (command-line)))
  (for-each (lambda (scheme) (display scheme) (display " ")) all-schemes)
  (newline)
  (exit 0))

(define scheme (if (get-environment-variable "COMPILE_R7RS")
                  (string->symbol (get-environment-variable "COMPILE_R7RS"))
                  #f))
(when (not scheme)
  (display "Environment variable COMPILE_R7RS not set." (current-error-port))
  (newline (current-error-port))
  (exit 1))
(when (not (assoc scheme data)) (error "Unsupported implementation" scheme))
(define compilation-target
  (if (get-environment-variable "COMPILE_R7RS_TARGET_OS")
    (string->symbol (get-environment-variable "COMPILE_R7RS_TARGET_OS"))
    (cond-expand (windows 'windows)
                 (else 'unix))))

(define input-file
  (let ((input-file #f))
    (for-each
      (lambda (item)
        (when (or (string-ends-with? item ".scm")
                  (string-ends-with? item ".sps"))
          (set! input-file item)))
      (list-tail (command-line) 1))
    input-file))

(define single-library-input-file
  (let ((input-file #f))
    (for-each
      (lambda (item)
        (when (or (string-ends-with? item ".sld")
                  (string-ends-with? item ".sls"))
          (set! input-file item)))
      (list-tail (command-line) 1))
    input-file))

(define r6rs?
  (if (and input-file
           (or (string-ends-with? input-file ".sps")
               (string-ends-with? input-file ".sls")))
    #t
    #f))

(define output-file
  (if (member "-o" (command-line))
    (cadr (member "-o" (command-line)))
    (if input-file
      "a.out"
      #f)))

(define prepend-directories
  (letrec ((looper (lambda (rest result)
                     (if (null? rest)
                       result
                       (if (string=? (car rest) "-I")
                         (looper (cdr (cdr rest))
                                 (append (list (cadr rest)) result))
                         (looper (cdr rest)
                                 result))))))
    (looper (command-line) (list))))

(define append-directories
  (letrec ((looper (lambda (rest result)
                     (if (null? rest)
                       result
                       (if (string=? (car rest) "-A")
                         (looper (cdr (cdr rest))
                                 (append (list (cadr rest)) result))
                         (looper (cdr rest)
                                 result))))))
    (looper (command-line) (list))))

(when (member "--library-dependencies" (command-line))
  (write (library-dependencies scheme
                               (append prepend-directories append-directories)
                               (if input-file
                                 input-file
                                 single-library-input-file)
                               (list)
                               (list)))
  (newline)
  (exit 0))

(define-c-library c-stdlib
                  '("stdlib.h")
                  libc-name
                  '((additional-versions ("6"))))

(define-c-procedure c-system c-stdlib 'system 'int '(pointer))

#;(define search-library-files
  (lambda (directory)
    (let ((result (list)))
      (for-each
        (lambda (file)
          (let* ((path (string-append directory "/" file))
                 (info (file-info path #f)))
            (when (and (not r6rs?)
                       (string-ends-with? path ".sld"))
              (set! result (append result (list path))))
            (when (and r6rs?
                       (string-ends-with? path ".sls"))
              (set! result (append result (list path))))
            (if (file-info-directory? info)
              (set! result (append result (search-library-files path))))))
        (directory-files directory))
      result)))

#;(define library-files
  (cond (single-library-input-file (list single-library-input-file))
        (else
          (apply append
                 (map
                   (lambda (directory)
                     (if (file-exists? directory)
                       (search-library-files directory)
                       (list)))
                   (append prepend-directories append-directories))))))

(define library-files (library-dependencies scheme
                               (append prepend-directories append-directories)
                               (if input-file
                                 input-file
                                 single-library-input-file)
                               (list)
                               (list)))

(define scheme-type (cdr (assoc 'type (cdr (assoc scheme data)))))

(define scheme-command
  (apply (cdr (assoc 'command (cdr (assoc scheme data))))
         (list
           (cond ((symbol=? compilation-target 'windows) "")
                 (else "exec"))
           ;; How to get the script file
           (cond ((symbol=? compilation-target 'windows) "%0%")
                 (else "$(cd -- \"$(dirname \"$0\")\" >/dev/null 2>&1 && pwd -P)/\"$0\""))
           (cond ((symbol=? compilation-target 'windows) "")
                 (else "\"$@\""))
           (if input-file input-file "")
           (if output-file output-file "")
           prepend-directories
           append-directories
           library-files
           r6rs?
           compilation-target)))

(define scheme-library-command
  (lambda (library-file)
    (apply (cdr (assoc 'library-command (cdr (assoc scheme data))))
      (list library-file prepend-directories append-directories r6rs?))))


(define list-of-features
  (letrec ((looper (lambda (rest result)
                     (if (null? rest)
                       result
                       (if (string=? (car rest) "-D")
                         (looper (cdr (cdr rest))
                                 (append (list (cadr rest)) result))
                         (looper (cdr rest)
                                 result))))))
    (looper (command-line) (list))))

(when (not (null? library-files))
  (when (assoc 'library-command (cdr (assoc scheme data)))
         (for-each
           (lambda (file)
             (let* ((library-command (scheme-library-command file)))
               (for-each
                 (lambda (command)
                   (let ((exit-code (c-system (string->c-utf8 command))))
                     (when (not (= exit-code 0))
                       (exit exit-code))))
                 library-command)))
           library-files)))

(when (and (equal? scheme-type 'interpreter) input-file)
  (when (and output-file (file-exists? output-file))
    (delete-file output-file))
  (let ((scheme-program (slurp input-file)))
    (with-output-to-file
      (if (symbol=? compilation-target 'windows)
        (string-append output-file ".bat")
        output-file)
      (lambda ()
      (if (symbol=? compilation-target 'windows)
        ""
        (for-each
          display
          `(#\newline
            "#|"
            #\newline
            ,scheme-command
            #\newline
            "|#"
            #\newline
            ,scheme-program)))))
    (cond ((symbol=? compilation-target 'unix)
           (c-system (string->c-utf8 (string-append "chmod +x " output-file)))))))

(when (and (equal? scheme-type 'compiler) input-file)
  (when (and output-file (file-exists? output-file))
    (delete-file output-file))
  (for-each
    (lambda (command)
      (let ((exit-code (c-system (string->c-utf8 command))))
        (when (not (= exit-code 0))
          (exit exit-code))))
    scheme-command))

