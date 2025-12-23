(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme write)
        (scheme process-context)
        (libs util)
        (libs implementations)
        (libs library-util)
        (srfi 170))

(define debug?
  (if (or (member "--debug" (command-line))
          (get-environment-variable "SCHEME_COMPILE_DEBUG"))
    #t
    #f))

(when (member "--help" (command-line))
  (display "For help see: man compile-scheme")
  (newline)
  (exit 0))

(when (member "--version" (command-line))
  (display "1.1.1")
  (newline)
  (exit 0))

(when (or (member "--list-r6rs" (command-line))
        (member "--list-r6rs-schemes" (command-line)))
  (for-each (lambda (scheme) (display scheme) (display " ")) r6rs-schemes)
  (newline)
  (exit 0))

(when (member "--list-r6rs-except" (command-line))
  (for-each
    (lambda (scheme)
      (when (not (member (symbol->string scheme)
                         (cdr (member "--list-r6rs-except" (command-line)))))
        (display scheme)
        (display " ")))
    r6rs-schemes)
  (newline)
  (exit 0))

(when (or (member "--list-r7rs" (command-line))
          (member "--list-r7rs-schemes" (command-line)))
  (for-each (lambda (scheme) (display scheme) (display " ")) r7rs-schemes)
  (newline)
  (exit 0))

(when (member "--list-r7rs-except" (command-line))
  (for-each
    (lambda (scheme)
      (when (not (member (symbol->string scheme)
                         (cdr (member "--list-r7rs-except" (command-line)))))
        (display scheme)
        (display " ")))
    r7rs-schemes)
  (newline)
  (exit 0))

(when (or (member "--list-all" (command-line))
          (member "--list-schemes" (command-line)))
  (for-each (lambda (scheme) (display scheme) (display " ")) all-schemes)
  (newline)
  (exit 0))

(define scheme
  (cond
    ((get-environment-variable "COMPILE_R7RS")
     (string->symbol (get-environment-variable "COMPILE_R7RS")))
    ((get-environment-variable "COMPILE_SCHEME")
     (string->symbol (get-environment-variable "COMPILE_SCHEME")))
    (else #f)))

(when (not scheme)
  (display "Either environment variable COMPILE_R7RS or COMPILE_SCHEME is not set." (current-error-port))
  (newline (current-error-port))
  (exit 1))

(when (not (assoc scheme data)) (error "Unsupported implementation" scheme))

(define input-file
  (let ((input-file #f))
    (for-each
      (lambda (item)
        (if (or (string-ends-with? item ".scm")
                (string-ends-with? item ".sps"))
          (set! input-file item)))
      (list-tail (command-line) 1))
    (when (not input-file)
      (error "The intput-file must be either .scm (R7RS) or .sps (R6RS)"))
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

(define scheme-type (cdr (assoc 'type (cdr (assoc scheme data)))))

(define compilation-target
  (let ((outfile (if (member "-o" (command-line))
                   (cadr (member "-o" (command-line)))
                   (if input-file
                     "a.out"
                     #f))))
    (if (and (symbol=? scheme-type 'compiler))
      (string-append outfile ".bin")
      outfile)))

(define compilation-target
  (cond
    ((member "--target" (command-line))
     (string->symbol (cadr (member "--target" (command-line)))))
    (else
      (cond-expand (windows 'windows)
                   (else 'unix)))))

(when debug?
  (display "compilation-target: ")
  (write compilation-target)
  (newline))

(define output-file
  (let ((outfile
          (cond
            ((member "-o" (command-line))
             (cadr (member "-o" (command-line))))
            (input-file (string-cut-from-end input-file 4)))))
    (if (symbol=? compilation-target 'windows)
      (string-append outfile ".bat")
      outfile)))

(when debug?
  (display "output-file: ")
  (write output-file)
  (newline))

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

(define library-files
  (library-dependencies scheme
                        (append prepend-directories append-directories)
                        (if input-file
                          input-file
                          single-library-input-file)
                        (list)
                        (list)))

(define scheme-command
  (apply (cdr (assoc 'command (cdr (assoc scheme data))))
         (list
           (cond
             ((symbol=? compilation-target 'windows) "; & @echo off &")
             (else "exec"))
           (cond
             ((symbol=? compilation-target 'windows) "\"%~f0\"")
             (else "\"$0\""))
           (cond
             ((symbol=? compilation-target 'windows) "%* & exit /b")
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
          (when debug?
            (display "[DEBUG] library-command: ")
            (write library-command)
            (newline))
          (for-each
            (lambda (command)
              (let ((exit-code (system command)))
                (when (not (= exit-code 0))
                  (exit exit-code))))
            library-command)))
      library-files)))

(when debug?
  (display "[DEBUG] scheme-command: ")
  (write scheme-command)
  (newline))

(when (and (equal? scheme-type 'interpreter) input-file)
  (when (and output-file (file-exists? output-file))
    (delete-file output-file))
  (let ((scheme-program (slurp input-file)))
    (with-output-to-file
      output-file
      (lambda ()
        (cond
          ((symbol=? compilation-target 'windows)
           (for-each
              display
              `(,scheme-command ,scheme-program))
           #;(for-each
             display
             `(";dir; start /WAIT " ,scheme-command " && exit"
               #\newline
               ,scheme-program
               ))
           )
          (else
            (for-each
              display
              `(#\newline
                "#|"
                #\newline
                ,scheme-command
                #\newline
                "|#"
                #\newline
                ,scheme-program))))))
    (cond ((symbol=? compilation-target 'unix)
           (system (string-append "chmod +x " output-file))))))

(when (and (symbol=? scheme-type 'compiler) input-file)
  (when (and output-file (file-exists? output-file))
    (delete-file output-file))
  (for-each
    (lambda (command)
      (let ((exit-code (system command)))
        (when (not (= exit-code 0))
          (exit exit-code))))
    scheme-command))
