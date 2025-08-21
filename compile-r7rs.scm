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

(define r6rs-schemes '(chezscheme
                       guile
                       ikarus
                       ironscheme
                       larceny
                       loko
                       mosh
                       racket
                       sagittarius
                       ypsilon))
(define r7rs-schemes '(chibi
                       chicken
                       cyclone
                       gambit
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

(define all-schemes (append r6rs-schemes r7rs-schemes))

(when (member "--list-r6rs-schemes" (command-line))
  (for-each
    (lambda (scheme)
      (display scheme)
      (newline))
    r6rs-schemes)
  (exit 0))

(when (member "--list-r7rs-schemes" (command-line))
  (for-each
    (lambda (scheme)
      (display scheme)
      (newline))
    r7rs-schemes)
  (exit 0))

(when (member "--list-schemes" (command-line))
  (for-each
    (lambda (scheme)
      (display scheme)
      (newline))
    all-schemes)
  (exit 0))

(define scheme (if (get-environment-variable "COMPILE_R7RS")
                  (string->symbol (get-environment-variable "COMPILE_R7RS"))
                  #f))
(when (not scheme) (error "Environment variable COMPILE_R7RS not set."))
(when (not (assoc scheme data))
  (error "Unsupported implementation" scheme))
(define compilation-target (if (get-environment-variable "TARGET")
                             (get-environment-variable "TARGET")
                             (cond-expand (windows "windows")
                                          (else "unix"))))

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
  (string-append (apply (cdr (assoc 'command (cdr (assoc scheme data))))
                        (list (if input-file input-file "")
                              (if output-file output-file "")
                              prepend-directories
                              append-directories
                              library-files
                              r6rs?))
                 (string #\newline)))

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

(display "Scheme            ")
(display scheme)
(newline)
(display "Type              ")
(display scheme-type)
(newline)
(newline)

; Compile libraries
(when (not (null? library-files))
  (if single-library-input-file
    (display "Given library file: ")
    (display "Found library files: "))
  (display library-files)
  (newline)
  (cond ((assoc 'library-command (cdr (assoc scheme data)))
         (for-each
           (lambda (file)
             (let* ((library-command (scheme-library-command file)))
               (display "Compiling library ")
               (display file)
               (newline)
               (display "With command      ")
               (display library-command)
               (newline)
               (display "Exit code         ")
               (let ((output (c-system (string->c-utf8 library-command))))
                 (when (not (= output 0))
                   (error "Problem compiling libraries, exiting" output))
                 (display output))
               (newline)
               (newline)))
           library-files))
        (else
          (display "Implementation has no library build command, skipping library compilation.")
          (newline))))

; Create executable file
(when (and (equal? scheme-type 'interpreter) input-file)
  (when (and output-file (file-exists? output-file))
    (delete-file output-file))
    (display "Creating startup script    ")
    (display output-file)
    (newline)
    (display "Containing command         ")
    (display scheme-command)
    (newline)
    (with-output-to-file
    (if (string=? compilation-target "windows")
      (string-append output-file ".bat")
      output-file)
    (lambda ()
      (cond ((string=? compilation-target "unix")
             (display "#!/bin/sh")
             (newline))
            ((string=? compilation-target "windows")
             (display "@echo off")
             (newline)
             (display "start")))
      (display scheme-command)))
  (cond ((string=? compilation-target "unix")
         (c-system (string->c-utf8 (string-append "chmod +x " output-file))))))

(when (and (equal? scheme-type 'compiler) input-file)
  (when (and output-file (file-exists? output-file))
    (delete-file output-file))
  (display "Compiling file    ")
  (display input-file)
  (newline)
  (display "With command      ")
  (display scheme-command)
  (newline)
  (display "Exit code         ")
  (display (c-system (string->c-utf8 scheme-command)))
  (newline))

