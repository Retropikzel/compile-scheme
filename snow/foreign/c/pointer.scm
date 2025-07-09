(define-c-library libc
                  '("stdlib.h" "stdio.h" "string.h")
                  libc-name
                  '((additional-versions ("0" "6"))))

(define-c-procedure c-calloc libc 'calloc 'pointer '(int int))
(cond-expand
  (gambit
    (define c-memset-address->pointer
     (c-lambda (unsigned-int64 unsigned-int8 int)
               (pointer void)
     "___return(memset((void*)___arg1, ___arg2, ___arg3));")))
  (chicken
    (define c-memset-address->pointer
             (lambda (address value offset)
               (address->pointer address))))
  (else
    (define-c-procedure c-memset-address->pointer libc 'memset 'pointer '(uint64 uint8 int))))

(cond-expand
  (gambit
    (define c-memset-pointer->address
     (c-lambda ((pointer void) unsigned-int8 int)
               unsigned-int64
     "___return((uint64_t)memset(___arg1, ___arg2, ___arg3));")))
  (chicken (define c-memset-pointer->address
             (lambda (pointer value offset)
               (pointer->address pointer))))
  (else (define-c-procedure c-memset-pointer->address libc 'memset 'uint64 '(pointer uint8 int))))
;(define-c-procedure c-memset-address libc 'memset 'pointer '(uint64 uint8 int))
;(define-c-procedure c-printf libc 'printf 'int '(pointer pointer))
(define-c-procedure c-malloc libc 'malloc 'pointer '(int))
(define-c-procedure c-strlen libc 'strlen 'int '(pointer))

(define make-c-bytevector
  (lambda (k . byte)
    (if (null? byte)
      (c-malloc k)
      (bytevector->c-bytevector (make-bytevector k (car byte))))))

(define c-bytevector
  (lambda bytes
    (bytevector->c-bytevector (apply bytevector bytes))))

(cond-expand
  (else (define-c-procedure c-free libc 'free 'void '(pointer))))

(define bytevector->c-bytevector
  (lambda (bytes)
    (letrec* ((bytes-length (bytevector-length bytes))
              (pointer (make-c-bytevector bytes-length))
              (looper (lambda (index)
                        (when (< index bytes-length)
                          (c-bytevector-u8-set! pointer
                                                index
                                                (bytevector-u8-ref bytes index))
                          (looper (+ index 1))))))
      (looper 0)
      pointer)))

(define c-bytevector->bytevector
  (lambda (pointer size)
    (letrec* ((bytes (make-bytevector size))
              (looper (lambda (index)
                        (let ((byte (c-bytevector-u8-ref pointer index)))
                          (if (= index size)
                            bytes
                            (begin
                              (bytevector-u8-set! bytes index byte)
                              (looper (+ index 1))))))))
      (looper 0))))

(define c-string-length
  (lambda (bytevector-var)
    (c-strlen bytevector-var)))

(define c-utf8->string
  (lambda (c-bytevector)
    (let ((size (c-strlen c-bytevector)))
      (utf8->string (c-bytevector->bytevector c-bytevector size)))))

(define string->c-utf8
  (lambda (string-var)
    (bytevector->c-bytevector (string->utf8 (string-append string-var (string #\null))))))

(cond-expand
  (chicken #t) ; FIXME
  (kawa #t) ; FIXME
  (else (define make-c-null
          (lambda ()
            (cond-expand (stklos (let ((pointer (make-c-bytevector 1)))
                                   (free-bytes pointer)
                                   pointer))
                         (else (c-memset-address->pointer 0 0 0)))))))

(cond-expand
  (chicken #t) ; FIXME
  (kawa #t) ; FIXME
  (else (define c-null?
          (lambda (pointer)
            (if (c-bytevector? pointer)
              (= (c-memset-pointer->address pointer 0 0) 0)
              #f)))))

#;(define c-bytevector->address
  (lambda (c-bytevector)
    (c-memset-pointer->address c-bytevector 0 0)))

#;(define address->c-bytevector
  (lambda (address)
    (c-memset-address->pointer address 0 0)))

#;(define c-bytevector-pointer-set!
  (lambda (c-bytevector k pointer)
    (c-bytevector-uint-set! c-bytevector
                            0
                            (c-bytevector->address pointer)
                            (native-endianness)
                            (c-type-size 'pointer))))

#;(define c-bytevector-pointer-ref
  (lambda (c-bytevector k)
    (address->c-bytevector (c-bytevector-uint-ref c-bytevector
                                                  0
                                                  (native-endianness)
                                                  (c-type-size 'pointer)))))

(cond-expand
  ;(kawa #t) ; Defined in kawa.scm
  (else (define-syntax call-with-address-of
          (syntax-rules ()
            ((_ input-pointer thunk)
             (let ((address-pointer (make-c-bytevector (c-type-size 'pointer))))
               (c-bytevector-pointer-set! address-pointer 0 input-pointer)
               (let ((result (apply thunk (list address-pointer))))
                 (set! input-pointer (c-bytevector-pointer-ref address-pointer 0))
                 (c-free address-pointer)
                 result)))))))
