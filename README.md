<pre>
compile-scheme(1)           General Commands Manual           compile-scheme(1)

NAME
       compile-scheme - Compiling Scheme programs to executables

SYNOPSIS
       compile-scheme  {  [-A  path]  [-I  path]  [-o  path] [-t target] input-
       file{.scm | .sps} | long-option }

DESCRIPTION
       compile-scheme is a tool to compile R6RS and R7RS Scheme  programs.   It
       supports most of SRFI-138 but also adds more features.

       The program input-file is compiled into an executable file.  The result‐
       ing  executable  file  is  written  to file specified by the -o path (if
       present) or to the file named same as input-file but without the .scm or
       .sps suffix.  On Windows either .bat or .exe is appended to  the  output
       name.

   SUPPORT LIST
       Some  implementations  support  both compiling and interpreting, in that
       case only the compiler functionality is used and the  implementation  is
       marked as compiler.

       R6RS Compilers
              loko

       R6RS Interpreters
              chezscheme  guile  ikarus  ironscheme mosh racket sagittarius yp‐
              silon

       R7RS Compilers
              chicken cyclone loko

       R7RS Interpreters
              chibi foment gauche guile kawa  larceny  meevax  mit-scheme  mosh
              racket sagittarius skint stklos tr7 ypsilon

OPTIONS
       -A  path Append path to the list of directories that are searched in or‐
       der to locate imported libraries.

       -I path Prepend directory to the list of directories that  are  searched
       in order to locate imported libraries.

       -o output-file Use the output-file file, instead of the default deducted
       from input-file, for the executable file produced.

       Multiple instances of the -A, and -I options can be specified.

       -t  {  unix  |  windows | php } Set the compilation target.  This is not
       needed if you are compiling for the target OS you  are  running.   Cross
       compilation is only supported in following cases:

              From unix host to php target when chosen implementation is inter‐
              preter.

              From  unix  host  to windows target when chosen implementation is
              interpreter.

       --list-r6rs List supported R6RS implementations.

       --list-r7rs List supported R7RS implementations.

       --list-all List all supported implementations.

       --list-targets List all supported compilation targets.

ENVIRONMENT
       COMPILE_R7RS/COMPILE_SCHEME

              This environment variable must be set. Set it to the name of  the
              implementation  as  specified  in the support list.  This differs
              from SRFI-138 as the SRFI excepts a path.

STANDARDS
       SRFI    138:    Compiling    Scheme     programs     to     executables.
       https://srfi.schemers.org/srfi-138/srfi-138.html

CAVEATS
       Differences from SRFI-138

              No support for -D flag.

              Not  all  implementations  support  adding to beginning or end of
              load path so -I and -A might work the same

              Only supports one input-file.

EXAMPLES
                                                              compile-scheme(1)
</pre>