(define-library
  (foreign c)
  (cond-expand
    (chibi
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme process-context)
              (chibi ast)
              (scheme inexact)
              (chibi))
      (include-shared "c/primitives/chibi/foreign-c"))
    (chicken
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (chicken base)
              (chicken foreign)
              (chicken locative)
              (chicken syntax)
              (chicken memory)
              (chicken random)))
    #;(cyclone
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (cyclone foreign)
              (scheme cyclone primitives)))
    (gambit
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (only (gambit) c-declare c-lambda c-define define-macro)))
    (gauche
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (gauche base)
              (foreign c primitives gauche)))
    #;(gerbil
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)))
    (guile
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (system foreign)
              (system foreign-library)
              (only (guile) include-from-path)
              (only (rnrs bytevectors)
                    bytevector-uint-set!
                    bytevector-uint-ref)))
    (kawa
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)))
    (mit-scheme
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)))
    #;(larceny
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (rename (primitives r5rs:require) (r5rs:require require))
              (primitives std-ffi)
              (primitives foreign-procedure)
              (primitives foreign-file)
              (primitives foreign-stdlib)
              (primitives system-interface)))
    (mosh
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme inexact)
              (scheme process-context)
              (mosh ffi)))
    (racket
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (only (racket base) system-type)
              (ffi winapi)
              (compatibility mlist)
              (ffi unsafe)
              (ffi vector)))
    (sagittarius
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (except (sagittarius ffi) c-free c-malloc)
              (sagittarius)))
    #;(skint
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)))
    (stklos
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (only (stklos)
                    %make-callback
                    make-external-function
                    allocate-bytes
                    free-bytes
                    cpointer?
                    cpointer-null?
                    cpointer-data
                    cpointer-data-set!
                    cpointer-set!
                    cpointer-ref
                    void?))
      (export ; calculate-struct-size-and-offsets
              ;struct-make
              get-environment-variable
              file-exists?
              make-external-function
              foreign-c:string-split
              c-bytevector-pointer-set!
              c-bytevector-pointer-ref))
    #;(tr7
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              ;(scheme inexact)
              (scheme process-context)))
    (ypsilon
      (import (scheme base)
              (scheme write)
              (scheme char)
              (scheme file)
              (scheme inexact)
              (scheme process-context)
              (ypsilon c-ffi)
              (ypsilon c-types)
              (only (core) define-macro syntax-case))))
  (export ;;;; Primitives 1
          c-type-size
          define-c-library
          define-c-procedure
          c-bytevector?
          c-bytevector-u8-set!
          c-bytevector-u8-ref
          c-bytevector-pointer-set!
          c-bytevector-pointer-ref

          ;;;; Primitives 2
          define-c-callback

          ;;;; c-bytevector
          make-c-null
          c-null?
          c-free
          call-with-address-of

          bytevector->c-bytevector
          c-bytevector->bytevector

          ;;;; Utilities
          libc-name

          ;; TODO endianness
          native-endianness
          make-c-bytevector
          ;; TODO c-bytevector=?
          ;; TODO c-bytevector-fill!
          ;; TODO c-bytevector-copy!
          ;; TODO c-bytevector-copy
          c-bytevector-s8-set!
          c-bytevector-s8-ref
          ;; TODO c-bytevector->u8-list
          ;; TODO u8-list->c-bytevector

          c-bytevector-uchar-ref
          c-bytevector-char-ref
          c-bytevector-char-set!
          c-bytevector-uchar-set!

          c-bytevector-uint-ref
          c-bytevector-sint-ref
          c-bytevector-sint-set!
          c-bytevector-uint-set!
          ;; TODO bytevector->uint-list
          ;; TODO bytevector->sint-list
          ;; TODO uint-list->bytevector
          ;; TODO sint-list->bytevector

          c-bytevector-u16-ref
          c-bytevector-s16-ref
          c-bytevector-u16-native-ref
          c-bytevector-s16-native-ref
          c-bytevector-u16-set!
          c-bytevector-s16-set!
          c-bytevector-u16-native-set!
          c-bytevector-s16-native-set!

          c-bytevector-u32-ref
          c-bytevector-s32-ref
          c-bytevector-u32-native-ref
          c-bytevector-s32-native-ref
          c-bytevector-u32-set!
          c-bytevector-s32-set!
          c-bytevector-u32-native-set!
          c-bytevector-s32-native-set!

          c-bytevector-u64-ref
          c-bytevector-s64-ref
          c-bytevector-s64-native-ref
          c-bytevector-u64-native-ref
          c-bytevector-u64-set!
          c-bytevector-s64-set!
          c-bytevector-u64-native-set!
          c-bytevector-s64-native-set!

          c-bytevector-ieee-single-native-ref
          c-bytevector-ieee-single-ref

          c-bytevector-ieee-double-native-ref
          c-bytevector-ieee-double-ref

          c-bytevector-ieee-single-native-set!
          c-bytevector-ieee-single-set!

          c-bytevector-ieee-double-native-set!
          c-bytevector-ieee-double-set!

          string->c-utf8
          ;; TODO string->c-utf16
          ;; TODO string->c-utf32

          c-utf8->string
          ;; TODO c-utf16->string
          ;; TODO c-utf32->string


          ;c-string-length ;; TODO Documentation, Testing

          ;; c-struct
          ;pffi-define-struct;define-c-struct
          ;pffi-struct-pointer;c-struct-bytevector
          ;pffi-struct-offset-get;c-struct-offset
          ;pffi-struct-set!;c-struct-set!
          ;pffi-struct-get;c-struct-get

          ;; c-array
          ;define-c-array (?)
          ;pffi-array-allocate;make-c-array
          ;pffi-array-pointer;c-array-pointer
          ;pffi-array?;c-array?
          ;pffi-pointer->array;c-bytevector->array
          ;pffi-array-get;c-array-get
          ;pffi-array-set!;c-array-set!
          ;pffi-list->array;list->c-array
          ;pffi-array->list;c-array->list

          ;; c-variable
          ;define-c-variable (?)
          )
   (begin
     (define type->libffi-type-number
       (lambda (type)
         (cond ((equal? type 'int8) 1)
               ((equal? type 'uint8) 2)
               ((equal? type 'int16) 3)
               ((equal? type 'uint16) 4)
               ((equal? type 'int32) 5)
               ((equal? type 'uint32) 6)
               ((equal? type 'int64) 7)
               ((equal? type 'uint64) 8)
               ((equal? type 'char) 9)
               ((equal? type 'unsigned-char) 10)
               ((equal? type 'short) 11)
               ((equal? type 'unsigned-short) 12)
               ((equal? type 'int) 13)
               ((equal? type 'unsigned-int) 14)
               ((equal? type 'long) 15)
               ((equal? type 'unsigned-long) 16)
               ((equal? type 'float) 17)
               ((equal? type 'double) 18)
               ((equal? type 'void) 19)
               ((equal? type 'pointer) 20)
               ((equal? type 'pointer-address) 21)
               ((equal? type 'callback) 22)
               (else (error "Undefined type" type)))))
      (define c-bytevector-get
        (lambda (pointer type offset)
          (cond ((equal? type 'int8) (c-bytevector-s8-ref pointer offset))
                ((equal? type 'uint8) (c-bytevector-u8-ref pointer offset))
                ((equal? type 'int16) (c-bytevector-s16-ref pointer offset))
                ((equal? type 'uint16) (c-bytevector-u16-ref pointer offset))
                ((equal? type 'int32) (c-bytevector-s32-ref pointer offset))
                ((equal? type 'uint32) (c-bytevector-u32-ref pointer offset))
                ((equal? type 'int64) (c-bytevector-s64-ref pointer offset))
                ((equal? type 'uint64) (c-bytevector-u64-ref pointer offset))
                ((equal? type 'char) (integer->char (c-bytevector-s8-ref pointer offset)))
                ((equal? type 'unsigned-char) (integer->char (c-bytevector-u8-ref pointer offset)))
                ((equal? type 'short) (c-bytevector-sint-ref pointer offset (native-endianness) (c-type-size 'short)))
                ((equal? type 'unsigned-short) (c-bytevector-sint-ref pointer offset (native-endianness) (c-type-size 'unsigned-short)))
                ((equal? type 'int) (c-bytevector-sint-ref pointer offset (native-endianness) (c-type-size 'int)))
                ((equal? type 'unsigned-int) (c-bytevector-sint-ref pointer offset (native-endianness) (c-type-size 'unsigned-int)))
                ((equal? type 'long) (c-bytevector-sint-ref pointer offset (native-endianness) (c-type-size 'long)))
                ((equal? type 'unsigned-long) (c-bytevector-sint-ref pointer offset (native-endianness) (c-type-size 'unsigned-long)))
                ((equal? type 'float) (c-bytevector-ieee-single-native-ref pointer offset))
                ((equal? type 'double) (c-bytevector-ieee-double-native-ref pointer offset))
                ((equal? type 'pointer) (c-bytevector-pointer-ref pointer offset))
                ((not (equal? type 'void)) (error "No such foreign type" type))
                ;; Return unspecified on purpose if type is void
                ))))
  #;(cond-expand
    (chicken-6 (include-relative "c/internal.scm"))
    (else (include "c/internal.scm")))
  (cond-expand
    (chibi (include "c/primitives/chibi.scm"))
    (chicken-5 (export foreign-declare
                       foreign-safe-lambda
                       void)
               (include "c/primitives/chicken.scm"))
    (chicken-6 (include-relative "c/primitives/chicken.scm"))
    ;(cyclone (include "c/primitives/cyclone.scm"))
    (gambit (include "c/primitives/gambit.scm"))
    (gauche (include "c/primitives/gauche/define-c-procedure.scm"))
    ;(gerbil (include "c/primitives/gerbil.scm"))
    (guile (include "./c/primitives/guile.scm"))
    (kawa (include "c/primitives/kawa.scm"))
    (mit-scheme (include "c/primitives/mit-scheme.scm"))
    ;(larceny (include "c/primitives/larceny.scm"))
    (mosh (include "c/primitives/mosh.scm"))
    (racket (include "c/primitives/racket.scm"))
    (sagittarius (include "c/primitives/sagittarius.scm"))
    ;(skint (include "c/primitives/skint.scm"))
    (stklos (include "c/primitives/stklos.scm"))
    ;(tr7 (include "c/primitives/tr7.scm"))
    (ypsilon (export c-function c-callback)
             (include "c/primitives/ypsilon.scm")))
  (cond-expand
    (chicken-6 (include-relative "c/main.scm")
               (include-relative "c/libc.scm")
               (include-relative "c/c-bytevectors.scm")
               (include-relative "c/pointer.scm")
               ;(include-relative "c/array.scm")
               ;(include-relative "c/struct.scm")
               )
    (else (include "c/main.scm")
          (include "c/libc.scm")
          ;(include "c/struct.scm")
          (include "c/c-bytevectors.scm")
          (include "c/pointer.scm")
          ;(include "c/array.scm")
          )))
