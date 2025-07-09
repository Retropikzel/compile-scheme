(define size-of-type
  (lambda (type)
    (cond ((eq? type 'int8) 1)
          ((eq? type 'uint8) 1)
          ((eq? type 'int16) 2)
          ((eq? type 'uint16) 2)
          ((eq? type 'int32) 4)
          ((eq? type 'uint32) 4)
          ((eq? type 'int64) 8)
          ((eq? type 'uint64) 8)
          ((eq? type 'char) 1)
          ((eq? type 'unsigned-char) 1)
          ((eq? type 'short) size-of-short)
          ((eq? type 'unsigned-short) size-of-unsigned-short)
          ((eq? type 'int) size-of-int)
          ((eq? type 'unsigned-int) size-of-unsigned-int)
          ((eq? type 'long) size-of-long)
          ((eq? type 'unsigned-long) size-of-unsigned-long)
          ((eq? type 'float) size-of-float)
          ((eq? type 'double) size-of-double)
          ((eq? type 'pointer) size-of-pointer)
          ((eq? type 'callback) size-of-pointer)
          ((eq? type 'void) 0)
          (else #f))))

(define shared-object-load
  (lambda (path options)
    (open-shared-library path)))

(define c-bytevector?
  (lambda (object)
    (pointer? object)))

(define c-bytevector-u8-set! pointer-set-c-uint8!)
(define c-bytevector-u8-ref pointer-ref-c-uint8)
(define c-bytevector-pointer-set! pointer-set-c-pointer!)
(define c-bytevector-pointer-ref pointer-ref-c-pointer)

(define type->native-type
  (lambda (type)
    (cond ((equal? type 'int8) 'int8_t)
          ((equal? type 'uint8) 'uint8_t)
          ((equal? type 'int16) 'int16_t)
          ((equal? type 'uint16) 'uint16_t)
          ((equal? type 'int32) 'int32_t)
          ((equal? type 'uint32) 'uint32_t)
          ((equal? type 'int64) 'int64_t)
          ((equal? type 'uint64) 'uint64_t)
          ((equal? type 'char) 'char)
          ((equal? type 'unsigned-char) 'char)
          ((equal? type 'short) 'short)
          ((equal? type 'unsigned-short) 'unsigned-short)
          ((equal? type 'int) 'int)
          ((equal? type 'unsigned-int) 'unsigned-int)
          ((equal? type 'long) 'long)
          ((equal? type 'unsigned-long) 'unsigned-long)
          ((equal? type 'float) 'float)
          ((equal? type 'double) 'double)
          ((equal? type 'pointer) 'void*)
          ((equal? type 'void) 'void)
          ((equal? type 'callback) 'void*)
          (else (error "type->native-type -- No such type" type)))))

(define-syntax define-c-procedure
  (syntax-rules ()
    ((_ scheme-name shared-object c-name return-type argument-types)
     (define scheme-name
       (make-c-function shared-object
                        (type->native-type return-type)
                        c-name
                        (map type->native-type argument-types))))))

(define-syntax define-c-callback
  (syntax-rules ()
    ((_ scheme-name return-type argument-types procedure)
     (define scheme-name
       (make-c-callback (type->native-type return-type)
                        (map type->native-type argument-types)
                        procedure)))))
