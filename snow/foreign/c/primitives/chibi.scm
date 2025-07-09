(define size-of-type
  (lambda (type)
    (cond ((eq? type 'int8) (size-of-int8_t))
          ((eq? type 'uint8) (size-of-uint8_t))
          ((eq? type 'int16) (size-of-int16_t))
          ((eq? type 'uint16) (size-of-uint16_t))
          ((eq? type 'int32) (size-of-int32_t))
          ((eq? type 'uint32) (size-of-uint32_t))
          ((eq? type 'int64) (size-of-int64_t))
          ((eq? type 'uint64) (size-of-uint64_t))
          ((eq? type 'char) (size-of-char))
          ((eq? type 'unsigned-char) (size-of-char))
          ((eq? type 'short) (size-of-short))
          ((eq? type 'unsigned-short) (size-of-unsigned-short))
          ((eq? type 'int) (size-of-int))
          ((eq? type 'unsigned-int) (size-of-unsigned-int))
          ((eq? type 'long) (size-of-long))
          ((eq? type 'unsigned-long) (size-of-unsigned-long))
          ((eq? type 'float) (size-of-float))
          ((eq? type 'double) (size-of-double))
          ((eq? type 'pointer) (size-of-pointer))
          ((eq? type 'pointer-address) (size-of-pointer))
          ((eq? type 'callback) (size-of-pointer))
          ((eq? type 'void) 0)
          (else #f))))

(define shared-object-load
  (lambda (path options)
    (let ((shared-object (dlopen path RTLD-NOW))
          (maybe-error (dlerror)))
      shared-object)))

(define c-bytevector?
  (lambda (object)
    (or (equal? object #f) ; False can be null pointer
        (pointer? object))))

(define pffi-type->native-type
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
          ((equal? type 'pointer) '(maybe-null pointer void*))
          ((equal? type 'pointer-address) '(maybe-null pointer void*))
          ((equal? type 'void) 'void)
          ((equal? type 'callback) '(maybe-null pointer void*))
          (else (error "pffi-type->native-type -- No such pffi type" type)))))

;; define-c-procedure

(define make-c-function
  (lambda (shared-object c-name return-type argument-types)
    (dlerror) ;; Clean all previous errors
    (let ((c-function (dlsym shared-object c-name))
          (maybe-dlerror (dlerror)))
      (lambda arguments
        (display "NAME: ")
        (display c-name)
        (newline)
        (display "ARGS: ")
        (write arguments)
        (newline)
        (let* ((return-pointer
                 (internal-ffi-call (length argument-types)
                                    (type->libffi-type-number return-type)
                                    (map type->libffi-type-number argument-types)
                                    c-function
                                    (c-type-size return-type)
                                    arguments)))
          (c-bytevector-get return-pointer return-type 0))))))

(define-syntax define-c-procedure
  (syntax-rules ()
    ((_ scheme-name shared-object c-name return-type argument-types)
     (define scheme-name
       (make-c-function shared-object
                        (symbol->string c-name)
                        return-type
                        argument-types)))))

(define make-c-callback
  (lambda (return-type argument-types procedure)
    (scheme-procedure-to-pointer procedure)))

(define-syntax define-c-callback
  (syntax-rules ()
    ((_ scheme-name return-type argument-types procedure)
     (define scheme-name
       (make-c-callback return-type 'argument-types procedure)))))
