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
    (define data
      `((chezscheme
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (let ((separator (cond-expand (windows ";") (else ":"))))
                          (apply string-append
                                 `("chezscheme "
                                   ,(util-getenv "COMPILE_R7RS_CHEZSCHEME")
                                   " "
                                   ,(if (and (null? prepend-directories)
                                             (null? append-directories))
                                      ""
                                      (apply string-append
                                             (list "--libdirs "
                                                   "\""
                                                   (apply string-append
                                                          (map (lambda (item)
                                                                 (string-append item separator))
                                                               (append prepend-directories append-directories)))
                                                   "\"")))
                                   " --program "))))))
        (chibi
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("chibi-scheme"
                                 " "
                                 ,(util-getenv "COMPILE_R7RS_CHIBI")
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-I" " " item " "))
                                        prepend-directories)
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-A" " " item " "))
                                        append-directories))))))
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
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        `(,(string-append "csc -R r7rs -X r7rs "
                                          (util-getenv "COMPILE_R7RS_CHICKEN")
                                          " -static "
                                          " "
                                          (apply string-append
                                                 (map (lambda (item)
                                                        (string-append "-I " item " "))
                                                      (append append-directories prepend-directories)))
                                          (apply string-append
                                                 (map (lambda (library-file)
                                                       (string-append "-uses "
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
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
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
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("foment "
                                 ,(util-getenv "COMPILE_R7RS_FOMENT")
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-I" " " item " "))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append "-A" " " item " "))
                                        append-directories))))))
        #;(gambit
          (type . compiler)
          (library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
                                `(,(string-append "gsc "
                                                  (apply string-append
                                                         (map (lambda (item)
                                                                (string-append item "/ "))
                                                              (append prepend-directories
                                                                      append-directories)))
                                                  (search-library-file (append append-directories
                                                                               prepend-directories)
                                                                       library-file)))))
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (let ((output-tmp-file (string-append output-file ".tmp")))
                        `(,(string-append "echo \"#!/usr/bin/env gsi -:r7rs,search="
                                          (apply string-append
                                                 (map (lambda (item)
                                                        (string-append item "/ "))
                                                      (append prepend-directories
                                                              append-directories)))
                                          "\" > " output-tmp-file)
                          ,(string-append "cat " input-file " >> " output-tmp-file)
                          ,(string-append "gsc "
                                          (apply string-append
                                                 (map (lambda (item)
                                                        (string-append item "/ "))
                                                      (append prepend-directories
                                                              append-directories)))
                                          " -o " output-file
                                          " -exe -nopreload "
                                          output-tmp-file))))))
        (gauche
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("gosh "
                                 ,(util-getenv "COMPILE_R7RS_GAUCHE")
                                 " -r7 "
                                 ,@(map (lambda (item)
                                          (string-append "-I" " " item " "))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append "-A" " " item " "))
                                        append-directories))))))
        (guile
          (type . interpreter)
          (library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
                                (let ((library-path (search-library-file (append append-directories
                                                                               prepend-directories)
                                                                       library-file)))
                                `(,(string-append "guild compile "
                                                  (if r6rs? " --r6rs -x .sls " " --r7rs -x .sld ")
                                                  " -O0 "
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
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("guile "
                                 ,(util-getenv "COMPILE_R7RS_GUILE")
                                 ,(if r6rs? " --r6rs -x .sls " " --r7rs -x .sld ")
                                 ,@(map (lambda (item)
                                          (string-append "-L " item " "
                                                         "-L " (dirname item) " "))
                                        (append prepend-directories
                                                append-directories))
                                 " -s"
                                 ,(string #\newline)
                                 "!#")))))
        (ikarus
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `( "IKARUS_LIBRARY_PATH="
                                  ,@(map (lambda (item) (string-append item ":")) prepend-directories)
                                  ,@(map (lambda (item) (string-append item ":")) append-directories)
                                  " ikarus "
                                  ,(util-getenv "COMPILE_R7RS_IKARUS")
                                  " --r6rs-script"
                                  )))))
        (ironscheme
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("ironscheme "
                                 ,(util-getenv "COMPILE_R7RS_IRONSCHEME")
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-I \"" item "\" "))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append "-I \"" item "\" "))
                                        append-directories))))))
        (kawa
          (type . compiler)
          (library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
                                (let* ((load-paths (apply string-append
                                                          (append (list "-Dkawa.import.path=")
                                                                  (map (lambda (item)
                                                                         (string-append item "/*.sld:"))
                                                                       (append prepend-directories
                                                                               append-directories)))))
                                       (library-file-path (search-library-file (append prepend-directories
                                                                                       append-directories)
                                                                               library-file))
                                       (output-dir
                                         (let ((output-dir "."))
                                           (for-each
                                             (lambda (dir)
                                               (when (string-starts-with? library-file-path
                                                                          dir)
                                                 (set! output-dir dir)))
                                             (append prepend-directories
                                                     append-directories))
                                           output-dir))
                                       (classpath
                                         (apply
                                           string-append
                                           (map (lambda (dir)
                                                  (string-append dir ":"))
                                                (append prepend-directories append-directories)))))
                                  `(,(string-append
                                       "CLASSPATH=" classpath
                                       " kawa -J--add-exports=java.base/jdk.internal.foreign.abi=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign.layout=ALL-UNNAMED -J--add-exports=java.base/jdk.internal.foreign=ALL-UNNAMED -J--enable-native-access=ALL-UNNAMED -J--enable-preview "
                                       (util-getenv "COMPILE_R7RS_KAWA")
                                       " "
                                       load-paths
                                       " -d " output-dir
                                       " "
                                       load-paths
                                       " -C "
                                       library-file-path)))))
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (set! append-directories
                                  (append append-directories
                                          (list "/usr/local/share/kawa/lib")))
                        (let* ((output-jar (string-append output-file ".jar"))
                              (main-class
                                (string-append (string-cut-from-end (path->filename input-file)
                                                                    4)))
                              (kawa-jar-path "/usr/local/share/kawa/lib/kawa.jar")
                              (classpath
                                (string-append
                                  kawa-jar-path " "
                                  (apply
                                    string-append
                                    (map (lambda (dir)
                                           (string-append dir " "))
                                         (append prepend-directories append-directories)))))
                              (import-paths
                                (apply
                                  string-append
                                  `("-Dkawa.import.path="
                                    ,@(map (lambda (dir)
                                             (string-append dir "/*.sld:"))
                                           (append prepend-directories append-directories))
                                    "*.sld")))
                              (library-dirs (apply string-append
                                                   (append (map (lambda (item)
                                                                  (string-append item " "))
                                                                (append prepend-directories
                                                                        append-directories)))))
                              (class-files
                                (apply
                                  string-append
                                  (map
                                    (lambda (lib)
                                      (string-append
                                        (string-cut-from-end
                                          (search-library-file (append prepend-directories
                                                                       append-directories)
                                                               lib)
                                          4)
                                        ".class "))
                                    library-files))))
                          `(;,(string-append "unzip -o -d . " kawa-jar-path)
                             ,(string-append
                                "echo 'Main-Class: " main-class "\nClass-Path: . " classpath "' > MANIFEST.mf")
                             ,(string-append "kawa " import-paths " --main -C " input-file)
                             ,(string-append "jar cfm " output-jar " MANIFEST.mf " library-dirs " " main-class ".class")
                             ,(string-append "printf '#!/bin/sh\nMYSELF=$(which \"$0\" 2>/dev/null)\n[ $? -gt 0 -a -f \"$0\" ] && MYSELF=\"./$0\"\njava=java\nif test -n \"$JAVA_HOME\"; then\n java=\"$JAVA_HOME/bin/java\"\nfi\nexec \"$java\" --add-exports=java.base/jdk.internal.foreign.abi=ALL-UNNAMED --add-exports=java.base/jdk.internal.foreign.layout=ALL-UNNAMED --add-exports=java.base/jdk.internal.foreign=ALL-UNNAMED --enable-native-access=ALL-UNNAMED --enable-preview -jar $MYSELF \"$@\"\nexit 1\n' > " output-file)
                             ,(string-append "cat " output-jar " >> " output-file)
                             ,(string-append "chmod +x " output-file))))))
        (larceny
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("sh"
                                 ,(string #\newline)
                                 "filename=\"$(basename ${0})\""
                                 ,(string #\newline)
                                 "tmpfile=\"/tmp/larceny.${filename}\""
                                 ,(string #\newline)
                                 "tail -n+8 \"${0}\" > \"${tmpfile}\""
                                 ,(string #\newline)
                                 "larceny -nobanner -quiet -utf8 "
                                 ,(if r6rs? " -r6 " " -r7 ")
                                 ,(util-getenv "COMPILE_R7RS_LARCENY")
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-I " item " "))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append "-A " item " "))
                                        append-directories)
                                 " -program \"${tmpfile}\" -- \"$@\""
                                 ,(string #\newline)
                                 "rm -rf \"${tmpfile}\""
                                 ,(string #\newline)
                                 "exit")))))
        (loko
          (type . compiler)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
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
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("meevax "
                                 ,(util-getenv "COMPILE_R7RS_MEEVAX")
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-I" " " item " "))
                                        prepend-directories)
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-A" " " item " "))
                                        append-directories)
                                 )))))
        (mit-scheme
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `(,"sh"
                                  ,(string #\newline)
                                  "filename=\"$(basename ${0})\""
                                  ,(string #\newline)
                                  "tmpfile=\"/tmp/mit-scheme.${filename}\""
                                  ,(string #\newline)
                                  "tail -n+8 \"${0}\" > \"${tmpfile}\""
                                  ,(string #\newline)
                                  "mit-scheme --batch-mode --no-init-file "
                                  ,@(map
                                      (lambda (item)
                                        (string-append "--load "
                                                       (search-library-file (append append-directories
                                                                                    prepend-directories)
                                                                            item)
                                                       " "))
                                      library-files)
                                  " --load \"${tmpfile}\" --eval \"(exit 0)\" --args \"$@\""
                                  ,(string #\newline)
                                  "rm -rf \"${tmpfile}\""
                                  ,(string #\newline)
                                  "exit"
                                  ,(string #\newline))))))
        (mosh
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("mosh "
                                 ,(util-getenv "COMPILE_R7RS_MOSH")
                                 " "
                                 ,@(map (lambda (item) (string-append "--loadpath=" item " "))
                                        (append append-directories prepend-directories)))))))
        (racket
          (type . interpreter)
          (library-command . ,(lambda (library-file prepend-directories append-directories r6rs?)
                                (let* ((full-path (search-library-file (append append-directories
                                                                               prepend-directories)
                                                                       library-file))
                                       (library-rkt-file (change-file-suffix full-path ".rkt")))
                                  (if r6rs?
                                    `(,(string-append "plt-r6rs --compile " library-file))
                                    `(,(string-append "printf "
                                                      "'#lang r7rs\\n"
                                                      "(import (except (scheme base) let let-values let*-values string-copy string-copy! string-for-each string-map string-fill! string->list))\\n"
                                                      "(include \"" (path->filename library-file) "\")\\n' > "
                                                      library-rkt-file))))))
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (let ((rkt-input-file (if (string=? input-file "")
                                                ""
                                                (change-file-suffix input-file ".rkt"))))
                          (apply string-append
                                 `("racket "
                                   ,(util-getenv "COMPILE_R7RS_RACKET")
                                   " "
                                   ,@(map (lambda (item)
                                            (string-append "-S " item " "))
                                          (append prepend-directories
                                                  append-directories))
                                   ,(if r6rs?
                                      ""
                                      (string-append (string #\newline) "#lang r7rs"))))))))
        (sagittarius
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("sash "
                                 ,(util-getenv "COMPILE_R7RS_SAGITTARIUS")
                                 ,(if r6rs? " -r6 " " -r7 ")
                                 ,@(map (lambda (item)
                                          (string-append " -L " item " "))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append " -A " item " "))
                                        append-directories))))))
        (skint
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("sh"
                                 ,(string #\newline)
                                 "filename=\"$(basename ${0})\""
                                 ,(string #\newline)
                                 "tmpfile=\"/tmp/skint.${filename}\""
                                 ,(string #\newline)
                                 "tail -n+8 \"${0}\" > \"${tmpfile}\""
                                 ,(string #\newline)
                                 "skint "
                                 ,(util-getenv "COMPILE_R7RS_SKINT")
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-I " item "/ "))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append "-A " item "/ "))
                                        append-directories)
                                 " --program=\"${tmpfile}\" \"$@\""
                                 ,(string #\newline)
                                 "rm -rf \"${tmpfile}\""
                                 ,(string #\newline)
                                 "exit"
                                 ,(string #\newline))))))
        (stklos
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("stklos "
                                 ,(util-getenv "COMPILE_R7RS_STKLOS")
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-I " item " "))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append "-A " item " "))
                                        append-directories))))))
        (tr7
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("TR7_LIB_PATH="
                                 ,@(map (lambda (item)
                                          (string-append item ":"))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append item ":"))
                                        append-directories)
                                 " tr7i "
                                 ,(util-getenv "COMPILE_R7RS_TR7"))))))
        (vicare
          (type . compiler)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("vicare"
                                 " "
                                 ,(util-getenv "COMPILE_R7RS_VICARE")
                                 " "
                                 ,@(map (lambda (item)
                                          (string-append "-I " item " "))
                                        prepend-directories)
                                 ,@(map (lambda (item)
                                          (string-append "-A " item " "))
                                        append-directories)
                                 " "
                                 "--compile-program"
                                 ,input-file)))))
        (ypsilon
          (type . interpreter)
          (command . ,(lambda (input-file output-file prepend-directories append-directories library-files r6rs?)
                        (apply string-append
                               `("ypsilon "
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
                                 " --top-level-program")))))))))
