(import (scheme base)
            (scheme file)
            (scheme read)
            (scheme write)
            (scheme process-context)
            (libs util)
            (libs data)
            (libs library-util)
            (srfi 170))

(define debug? (if (member "--debug" (command-line)) #t #f))

(when (member "--help" (command-line))
  (display "For help see: man compile-scheme")
  (newline)
  (exit 0))

(when (member "--version" (command-line))
  (display "1.0.0")
  (newline)
  (exit 0))

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
    (if (and (symbol=? scheme-type 'compiler)
             ;(symbol=? compilation-target 'php)
             )
      (string-append outfile ".bin")
      outfile)))

(define compilation-target
  (cond
    ((member "-t" (command-line))
     (cadr (member "-t" (command-line))))
    (else
      (cond-expand (windows 'windows)
                   (else 'unix)))))

(define output-file
  (let ((outfile
          (cond
            ((member "-o" (command-line))
             (cadr (member "-o" (command-line))))
            (input-file (string-cut-from-end input-file 4)))))
    (if (and (symbol=? scheme-type 'compiler)
             (symbol=? compilation-target 'php))
      (string-append outfile ".bin")
      outfile)))

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

(define scheme-command
  (apply (cdr (assoc 'command (cdr (assoc scheme data))))
         (list
           (cond
             ((symbol=? compilation-target 'windows) "")
             ((symbol=? compilation-target 'php) "")
             (else "exec"))
           (cond
             ((symbol=? compilation-target 'windows) "%0%")
             ((symbol=? compilation-target 'php) "$binname")
             (else "\"$0\""))
           (cond
             ((symbol=? compilation-target 'windows) "")
             ((symbol=? compilation-target 'php) "")
             (else "\"$@\""))
           (if input-file input-file "")
           (if output-file output-file "")
           prepend-directories
           append-directories
           library-files
           r6rs?
           compilation-target)))

(when debug?
  (display "[debug] scheme-command: ")
  (write scheme-command)
  (newline))

(define scheme-library-command
  (lambda (library-file)
    (apply (cdr (assoc 'library-command (cdr (assoc scheme data))))
      (list library-file prepend-directories append-directories r6rs?))))

(when debug?
  (display "[debug] scheme-library-command: ")
  (write scheme-library-command)
  (newline))


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
                   (let ((exit-code (system command)))
                     (when (not (= exit-code 0))
                       (exit exit-code))))
                 library-command)))
           library-files)))

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
             `(";dir; start /WAIT " ,scheme-command " && exit"
               #\newline
               ,scheme-program
               )))
          ((symbol=? compilation-target 'php)
           (for-each
             display
             `("<?php"
               " $descriptorspec = array(0 => fopen('php://stdin', 'r'), 1 => array('pipe', 'w'), 2 => fopen('php://stderr', 'w'));"
               " $cwd = '.';"
               " $filepath = $_SERVER['SCRIPT_FILENAME'];"
               " $filename = $_SERVER['SCRIPT_NAME'];"
               " $binname = '/tmp/test.bin';"
               " system(\"tail -n+3 $filepath > $binname\");"
               " $scheme_command = \"" ,scheme-command "\";"
               " $process = proc_open($scheme_command, $descriptorspec, $pipes, $cwd, $_ENV);"
               " echo stream_get_contents($pipes[1]);"
               " die();"
               " ?>"
               #\newline
               #\newline
               ,scheme-program)))
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
    scheme-command)
  (cond
    ((symbol=? compilation-target 'php)
     (let* ((php-file (string-cut-from-end output-file 4))
            (port (open-binary-output-file php-file))
            (bin (slurp-bytes output-file)))
       (for-each
         (lambda (item) (write-bytevector (string->utf8 item) port))
         `("<?php"
           " $descriptorspec = array(0 => fopen('php://stdin', 'r'), 1 => array('pipe', 'w'), 2 => fopen('php://stderr', 'w'));"
           " $cwd = '.';"
           " $filepath = $_SERVER['SCRIPT_FILENAME'];"
           " $binname = '/tmp/test.bin';"
           " system(\"tail -n+3 $filepath > $binname\");"
           " $process = proc_open($binname, $descriptorspec, $pipes, $cwd, $_ENV);"
           " echo stream_get_contents($pipes[1]);"
           " die();"
           " ?>"
           ,(string #\newline)
           ,(string #\newline)))
       (write-bytevector bin port)
       (close-output-port port)))
    (else #t)))

