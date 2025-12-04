(define-library
  (libs data)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)
          (srfi 170)
          (libs util))
  (export data)
  (begin
    (define pwd (cond-expand (windows "%CD%") (else "${PWD}")))
    (define data
      `((capyscheme
          (type . interpreter)
          (command . ,(lambda (exec-cmd
                                script-file
                                args
                                input-file
                                output-file
                                prepend-directories
                                append-directories
                                library-files
                                r6rs?
                                compilation-target)
                        (apply string-append
                               `(,exec-cmd
                                  " capy "
                                  ,(util-getenv "COMPILE_R7RS_CAPYSCHEME")
                                  " "
                                  ,@(map (lambda (item)
                                           (string-append "-L" " " item " "))
                                         (append prepend-directories
                                                 append-directories))
                                  " --script "
                                  ,script-file
                                  " "
                                  ,args)))))
        (chezscheme
          (type . interpreter)
          (command . ,(lambda (exec-cmd
                                script-file
                                args
                                input-file
                                output-file
                                prepend-directories
                                append-directories
                                library-files
                                r6rs?
                                compilation-target)
                        (let ((separator (cond-expand (windows ";") (else ":"))))
                          (apply string-append
                                 `(,exec-cmd
                                    " chezscheme "
                                    ,(util-getenv "COMPILE_R7RS_CHEZSCHEME")
                                    " "
                                    ,(if (and (null? prepend-directories)
                                              (null? append-directories))
                                       ""
                                       (apply string-append
                                              (list "--libdirs "
                                                    "'"
                                                    (apply string-append
                                                           (map (lambda (item)
                                                                  (string-append item separator))
                                                                (append prepend-directories append-directories)))
                                                    "'")))
                                    " --program "
                                    ,script-file
                                    " "
                                    ,args))))))
        (chibi
          (type . interpreter)
          (command . ,(lambda (exec-cmd
                                script-file
                                args
                                input-file
                                output-file
                                prepend-directories
                                append-directories
                                library-files
                                r6rs
                                compilation-target)
                        (apply string-append
                               `(,exec-cmd
                                  " chibi-scheme "
                                  ,(util-getenv "COMPILE_R7RS_CHIBI")
                                  ,@(map (lambda (item)
                                           (string-append " -I" " " item " "))
                                         prepend-directories)
                                  ,@(map (lambda (item)
                                           (string-append " -A" " " item " "))
                                         append-directories)
                                  ,script-file
                                  " "
                                  ,args)))))
        (chicken
          (type . compiler)
          (library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
                                (let ((unit (string-append (if (string-starts-with? library-file "srfi")
                                                             (string-replace (string-cut-from-end library-file 4) #\/ #\-)
                                                             (string-replace (string-cut-from-end library-file 4) #\/ #\.))))
                                      (out (string-append (if (string-starts-with? library-file "srfi")
                                                            (string-replace (string-cut-from-end library-file 4) #\/ #\-)
                                                            (string-replace (string-cut-from-end library-file 4) #\/ #\.))
                                                          ".o"))
                                      (static-out (string-append (if (string-starts-with? library-file "srfi")
                                                                   (string-replace (string-cut-from-end library-file 4) #\/ #\-)
                                                                   (string-replace (string-cut-from-end library-file 4) #\/ #\.))
                                                                 ".a")))
                                  `(,(string-append "csc -R r7rs -X r7rs "
                                                    (util-getenv "COMPILE_R7RS_CHICKEN")
                                                    " -static -c -J -o "
                                                    out
                                                    " "
                                                    (search-library-file (append prepend-directories append-directories) library-file)
                                                    " "
                                                    (apply string-append
                                                           (map (lambda (item)
                                                                  (string-append "-I " item " "))
                                                                (append append-directories
                                                                        prepend-directories)))
                                                    "-unit "
                                                    unit)
                                     ,(string-append "ar rcs " static-out " " out)))))
          (command . ,(lambda (exec-cmd
                                script-file
                                args
                                input-file
                                output-file
                                prepend-directories
                                append-directories
                                library-files
                                r6rs?
                                compilation-target)
                        `(,(string-append "csc -R r7rs -X r7rs "
                                          (util-getenv "COMPILE_R7RS_CHICKEN")
                                          " -static "
                                          (apply string-append
                                                 (map (lambda (item)
                                                        (string-append " -I " item " "))
                                                      (append append-directories prepend-directories)))
                                          (apply string-append
                                                 (map (lambda (library-file)
                                                        (string-append " -uses "
                                                                       (if (string-starts-with? library-file "srfi")
                                                                         (string-replace (string-cut-from-end library-file 4) #\/ #\-)
                                                                         (string-replace (string-cut-from-end library-file 4) #\/ #\.))
                                                                       " "))
                                                      library-files))
                                          " -output-file "
                                          output-file
                                          " "
                                          input-file)))))
        (cyclone
          (type . compiler)
          (library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
                                `(,(string-append "cyclone "
                                                  (util-getenv "COMPILE_R7RS_CYCLONE")
                                                  " "
                                                  (apply string-append
                                                         (map (lambda (item) (string-append "-I " item " ")) prepend-directories))
                                                  (apply string-append
                                                         (map (lambda (item) (string-append "-A " item " ")) append-directories))
                                                  (search-library-file (append prepend-directories
                                                                               append-directories)
                                                                       library-file)))))
          (command . ,(lambda (exec-cmd
                                script-file
                                args
                                input-file
                                output-file
                                prepend-directories
                                append-directories
                                library-files
                                r6rs?
                                compilation-target)
                        `(,(string-append "cyclone "
                                          (util-getenv "COMPILE_R7RS_CYCLONE")
                                          " "
                                          (apply string-append
                                                 (map (lambda (item) (string-append "-I " item " ")) prepend-directories))
                                          (apply string-append
                                                 (map (lambda (item) (string-append "-A " item " ")) append-directories))
                                          input-file)
                           ,(string-append (if (not (string=? (string-cut-from-end input-file 4) output-file))
                                             (string-append
                                               "mv "
                                               (string-cut-from-end input-file 4)
                                               " "
                                               output-file)
                                             "sleep 0"))))))
        (foment
          (type . interpreter)
          (command . ,(lambda (exec-cmd
                                script-file
                                args
                                input-file
                                output-file
                                prepend-directories
                                append-directories
                                library-files
                                r6rs?
                                compilation-target)
                        (apply string-append
                               `(,exec-cmd
                                  " foment "
                                  ,(util-getenv "COMPILE_R7RS_FOMENT")
                                  ,@(map (lambda (item)
                                           (string-append " -I" " " item " "))
                                         prepend-directories)
                                  ,@(map (lambda (item)
                                           (string-append " -A" " " item " "))
                                         append-directories)
                                  ,script-file
                                  " "
                                  ,args)))))
        (gambit
          (type . compiler)
          #;(library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
          `(,(string-append "gsc -:search="
                            (apply string-append
                                   (map (lambda (item)
                                          (string-append item "/, "))
                                        (append prepend-directories
                                                append-directories)))
                            (search-library-file (append append-directories
                                                         prepend-directories)
                                                 library-file)))))
      (command . ,(lambda (exec-cmd
                            script-file
                            args
                            input-file
                            output-file
                            prepend-directories
                            append-directories
                            library-files
                            r6rs?
                            compilation-target)
                    (let ((library-files-paths
                            (map (lambda (item)
                                   (search-library-file (append prepend-directories
                                                                append-directories)
                                                        item))
                                 library-files))
                          (link-file
                            (string-append
                              (string-cut-from-end input-file 4) "_.c")))
                      `(,(string-append "gsc -:search="
                                        (string-cut-from-end
                                          (apply string-append
                                                 (map (lambda (item)
                                                        (string-append item ","))
                                                      (append prepend-directories
                                                              append-directories)))
                                          1)
                                        " -link -flat -nopreload "
                                        (string-cut-from-end
                                          (apply string-append
                                                 (map (lambda (item)
                                                        (string-append item " "))
                                                      library-files-paths))
                                          1)
                                        " "
                                        input-file)
                         ,(string-append "gsc -:search="
                                         (string-cut-from-end
                                           (apply string-append
                                                  (map (lambda (item)
                                                         (string-append item ","))
                                                       (append prepend-directories
                                                               append-directories)))
                                           1)
                                         " -obj "
                                         (apply string-append
                                                (map (lambda (item)
                                                       (string-append (string-cut-from-end item 4) ".c "))
                                                     library-files-paths))
                                         " "
                                         (string-append (string-cut-from-end input-file 4) ".c")
                                         " "
                                         (string-append (string-cut-from-end input-file 4) "_.c"))
                         ,(string-append "gcc -o "
                                         output-file
                                         " "
                                         (apply string-append
                                                (map (lambda (item)
                                                       (string-append (string-cut-from-end item 4) ".o "))
                                                     library-files-paths))
                                         " "
                                         (string-append (string-cut-from-end input-file 4) ".o")
                                         " "
                                         (string-append (string-cut-from-end input-file 4) "_.o"))
                         )))))
    (gauche
      (type . interpreter)
      (command . ,(lambda (exec-cmd
                            script-file
                            args
                            input-file
                            output-file
                            prepend-directories
                            append-directories
                            library-files
                            r6rs?
                            compilation-target)
                    (apply string-append
                           `(,exec-cmd
                              ,(if (symbol=? compilation-target 'windows)
                                 " gosh.exe "
                                 " gosh ")
                              ,(util-getenv "COMPILE_R7RS_GAUCHE")
                              " -r7 "
                              ,@(map (lambda (item)
                                       (string-append " -I" " " item " "))
                                     prepend-directories)
                              ,@(map (lambda (item)
                                       (string-append " -A" " " item " "))
                                     append-directories)
                              ,script-file
                              " "
                              ,args)))))
    (guile
      (type . interpreter)
      (library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
                            (let ((library-path (search-library-file (append append-directories
                                                                             prepend-directories)
                                                                     library-file)))
                              `(,(string-append "guild compile "
                                                (if r6rs? " --r6rs " " --r7rs ")
                                                (apply string-append
                                                       (map (lambda (item)
                                                              (string-append "-L" " " item " "))
                                                            (append prepend-directories
                                                                    append-directories)))
                                                " -o "
                                                (string-append
                                                  (string-cut-from-end library-path 4)
                                                  ".go")
                                                library-path)))))
      (command . ,(lambda (exec-cmd
                            script-file
                            args
                            input-file
                            output-file
                            prepend-directories
                            append-directories
                            library-files
                            r6rs?
                            compilation-target)
                    (apply string-append
                           `(,exec-cmd
                              " guile "
                              ,(util-getenv "COMPILE_R7RS_GUILE")
                              ,(if r6rs? " --r6rs -x .sls " " --r7rs -x .sld ")
                              ,@(map (lambda (item)
                                       (string-append " -L " item " "))
                                     (append prepend-directories
                                             append-directories))
                              " -s "
                              ,script-file
                              " "
                              ,args)))))
    (ikarus
      (type . interpreter)
      (command . ,(lambda (exec-cmd
                            script-file
                            args
                            input-file
                            output-file
                            prepend-directories
                            append-directories
                            library-files
                            r6rs
                            compilation-target)
                    (apply string-append
                           `( "IKARUS_LIBRARY_PATH="
                              ,@(map (lambda (item) (string-append item ":")) prepend-directories)
                              ,@(map (lambda (item) (string-append item ":")) append-directories)
                              " "
                              ,exec-cmd
                              " ikarus "
                              ,(util-getenv "COMPILE_R7RS_IKARUS")
                              " --r6rs-script "
                              ,script-file
                              " "
                              ,args)))))
    (ironscheme
      (type . interpreter)
      (command . ,(lambda (exec-cmd
                            script-file
                            args
                            input-file
                            output-file
                            prepend-directories
                            append-directories
                            library-files
                            r6rs
                            compilation-target)
                    (apply string-append
                           `(,exec-cmd
                              " ironscheme "
                              ,(util-getenv "COMPILE_R7RS_IRONSCHEME")
                              " "
                              ,@(map (lambda (item)
                                       (string-append "-I \"" item "\" "))
                                     prepend-directories)
                              ,@(map (lambda (item)
                                       (string-append "-I \"" item "\" "))
                                     append-directories)
                              ,script-file
                              " "
                              ,args)))))
    (kawa
      (type . interpreter)
      (command . ,(lambda (exec-cmd
                            script-file
                            args
                            input-file
                            output-file
                            prepend-directories
                            append-directories
                            library-files
                            r6rs?
                            compilation-target)
                    (apply string-append
                           `(,exec-cmd
                              " kawa -J--add-exports=java.base/jdk.internal.foreign.abi=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign.layout=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign=ALL-UNNAMED -J--enable-native-access=ALL-UNNAMED -J--enable-preview --r7rs --full-tailcalls"
                              ,(util-getenv "COMPILE_R7RS_KAWA")
                              " -Dkawa.import.path="
                              ,@(map (lambda (item)
                                       (if (char=? (string-ref item 0) #\/)
                                         (string-append item "/*.sld:")
                                         (string-append pwd "/" item "/*.sld:")))
                                     (append prepend-directories
                                             append-directories
                                             (list "/usr/local/share/kawa/lib")))
                              " -f "
                              ,script-file
                              " "
                              ,args)))))
    #;(larceny
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs?
                          compilation-target)
                  (apply string-append
                         `(,exec-cmd
                            " larceny -nobanner -quiet -utf8 "
                            ,(if r6rs? " -r6 " " -r7 ")
                            ,(util-getenv "COMPILE_R7RS_LARCENY")
                            ,@(map (lambda (item)
                                     (string-append " -I " item " "))
                                   prepend-directories)
                            ,@(map (lambda (item)
                                     (string-append " -A " item " "))
                                   append-directories)
                            " -program "
                            ,script-file
                            " -- "
                            ,args)))))
  (loko
    (type . compiler)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs?
                          compilation-target)
                  (let ((out (string-cut-from-end input-file 4)))
                    `(,(string-append "LOKO_LIBRARY_PATH="
                                      (apply string-append
                                             (map (lambda (item)
                                                    (string-append item ":"))
                                                  prepend-directories))
                                      (apply string-append
                                             (map (lambda (item)
                                                    (string-append item ":"))
                                                  append-directories))
                                      " loko "
                                      (util-getenv "COMPILE_R7RS_LOKO")
                                      " "
                                      (if r6rs? "-std=r6rs" "-std=r7rs")
                                      " "
                                      "--compile"
                                      " "
                                      input-file)
                       ,(string-append "mv " out " " output-file))))))
  (meevax
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs
                          compilation-target)
                  (apply string-append
                         `(,exec-cmd
                            " meevax "
                            ,(util-getenv "COMPILE_R7RS_MEEVAX")
                            ,@(map (lambda (item)
                                     (if (char=? (string-ref item 0) #\/)
                                       (string-append " -I " pwd "/" item " ")
                                       (string-append " -I " item " ")))
                                   prepend-directories)
                            ,@(map (lambda (item)
                                     (if (char=? (string-ref item 0) #\/)
                                       (string-append " -A " pwd "/" item " ")
                                       (string-append " -A " item " ")))
                                   append-directories)
                            ,script-file
                            " "
                            ,args)))))
  (mit-scheme
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs
                          compilation-target)
                  (apply string-append
                         `(,exec-cmd
                            " mit-scheme --batch-mode --no-init-file "
                            ,@(map
                                (lambda (item)
                                  (string-append " --load "
                                                 (search-library-file (append append-directories
                                                                              prepend-directories)
                                                                      item)
                                                 " "))
                                library-files)
                            " --load "
                            ,script-file
                            " --eval '(exit 0)' "
                            ,(if (string=? args "")
                               ""
                               (string-append " --args " args)))))))
  (mosh
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs
                          compilation-target)
                  (let ((dirs (append append-directories prepend-directories)))
                    (apply string-append
                           `(,(if (> (length dirs) 0)
                                (string-append
                                  "MOSH_LOADPATH="
                                  (apply string-append
                                         (map (lambda (item) (string-append item ":")) dirs)))
                                "")
                              " "
                              ,exec-cmd
                              " mosh "
                              ,(util-getenv "COMPILE_R7RS_MOSH")
                              ,script-file
                              " "
                              ,args))))))
  (racket
    (type . interpreter)
    (library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
                          (let* ((full-path (search-library-file (append append-directories
                                                                         prepend-directories)
                                                                 library-file))
                                 (library-rkt-file (change-file-suffix full-path ".rkt")))
                            (if r6rs?
                              `("sleep 0") ;`(,(string-append "plt-r6rs --compile " library-file))
                              `(,(string-append "printf "
                                                "'#lang r7rs\\n"
                                                "(import (except (scheme base) let let-values let*-values string-copy string-copy! string-for-each string-map string-fill! string->list))\\n"
                                                "(include \"" (path->filename library-file) "\")\\n' > "
                                                library-rkt-file))))))
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs?
                          compilation-target)
                  (let ((rkt-input-file (if (string=? input-file "")
                                          ""
                                          (change-file-suffix input-file ".rkt"))))
                    (apply string-append
                           `(,exec-cmd
                              ,(if r6rs?
                                 " plt-r6rs "
                                 " racket ")
                              ,(util-getenv "COMPILE_R7RS_RACKET")
                              ,(if r6rs?  "" " -I r7rs ")
                              ,@(map (lambda (item)
                                       (string-append
                                         (if r6rs?  " ++path " " -S ")
                                         item " "))
                                     (append prepend-directories
                                             append-directories))
                              ,(if r6rs?  "" " --script ")
                              ,script-file
                              " "
                              ,args))))))
  (sagittarius
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs?
                          compilation-target)
                  (apply string-append
                         `(,exec-cmd
                            ,(if (symbol=? compilation-target 'windows)
                               " sash.exe -d "
                               " sash -d ")
                            ,(util-getenv "COMPILE_R7RS_SAGITTARIUS")
                            ,(if r6rs? " -r6 " " -r7 ")
                            ,@(map (lambda (item)
                                     (string-append " -L " item " "))
                                   prepend-directories)
                            ,@(map (lambda (item)
                                     (string-append " -A " item " "))
                                   append-directories)
                            ,script-file
                            " "
                            ,args)))))
  (skint
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs
                          compilation-target)
                  (apply string-append
                         `(,exec-cmd
                            " skint "
                            ,(util-getenv "COMPILE_R7RS_SKINT")
                            " "
                            ,@(map (lambda (item)
                                     (string-append "-I " item "/ "))
                                   prepend-directories)
                            ,@(map (lambda (item)
                                     (string-append "-A " item "/ "))
                                   append-directories)
                            " --program="
                            ,script-file
                            " "
                            ,args)))))
  (stklos
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs?
                          compilation-target)
                  (apply string-append
                         `(,exec-cmd
                            " stklos "
                            ,(util-getenv "COMPILE_R7RS_STKLOS")
                            " "
                            ,@(map (lambda (item)
                                     (string-append "-I " item " "))
                                   prepend-directories)
                            ,@(map (lambda (item)
                                     (string-append "-A " item " "))
                                   append-directories)
                            ,script-file
                            " "
                            ,args)))))
  (tr7
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs?
                          compilation-target)
                  (apply string-append
                         `(" TR7_LIB_PATH="
                           ,@(map (lambda (item)
                                    (string-append item ":"))
                                  prepend-directories)
                           ,@(map (lambda (item)
                                    (string-append item ":"))
                                  append-directories)
                           " "
                           ,exec-cmd
                           " tr7i "
                           ,(util-getenv "COMPILE_R7RS_TR7")
                           ,script-file
                           " "
                           ,args)))))
  (vicare
    (type . compiler)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs?
                          compilation-target)
                  (apply string-append
                         `("vicare "
                           ,(util-getenv "COMPILE_R7RS_VICARE")
                           ,@(map (lambda (item)
                                    (string-append " -I " item " "))
                                  prepend-directories)
                           ,@(map (lambda (item)
                                    (string-append " -A " item " "))
                                  append-directories)
                           " --compile-program")))))
  (ypsilon
    (type . interpreter)
    (command . ,(lambda (exec-cmd
                          script-file
                          args
                          input-file
                          output-file
                          prepend-directories
                          append-directories
                          library-files
                          r6rs?
                          compilation-target)
                  (apply string-append
                         `(,exec-cmd
                            " ypsilon "
                            ,(util-getenv "COMPILE_R7RS_YPSILON")
                            ,(if r6rs? " --r6rs " " --r7rs ")
                            " --mute"
                            " --quiet "
                            ,@(map (lambda (item)
                                     (string-append "--sitelib=" item " "))
                                   prepend-directories)
                            ,@(map (lambda (item)
                                     (string-append "--sitelib=" item " "))
                                   append-directories)
                            " --top-level-program "
                            ,script-file
                            " "
                            ,args)))))))))
