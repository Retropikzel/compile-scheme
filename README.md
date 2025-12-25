<pre>
compile-scheme(1)           General Commands Manual          compile-scheme(1)

NAME
       compile-scheme - Compiling Scheme programs to executables

SYNOPSIS
       compile-scheme [-A path] [-I path] [-o path] input-file.sps

       compile-scheme [-A path] [-I path] [-o path] input-file.scm

       compile-scheme long-option

DESCRIPTION
       compile-scheme  is a tool to compile R6RS and R7RS Scheme programs.  It
       supports most of SRFI-138 but also adds more features.

       The program input-file is compiled into an executable  file.   The  re‐
       sulting executable file is written to file specified by the -o path (if
       present) or to the file named same as input-file but without  the  .scm
       or .sps suffix.  On Windows either .bat or .exe is appended to the out‐
       put name.

SUPPORTED IMPLEMENTATIONS
       Some implementations support both compiling and interpreting,  in  that
       case  only the compiler functionality is used and the implementation is
       marked as compiler.

   R6RS Compilers
       loko

   R6RS Interpreters
       chezscheme guile ikarus ironscheme mosh racket sagittarius ypsilon

   R7RS Compilers
       chicken cyclone loko

   R7RS Interpreters
       chibi foment gauche guile kawa larceny meevax  mit-scheme  mosh  racket
       sagittarius skint stklos tr7 ypsilon

OPTIONS
       -A path Append path to the list of directories that are searched in or‐
       der to locate imported libraries.

       -I path Prepend directory to the list of directories that are  searched
       in order to locate imported libraries.

       -o  output-file  Use  the  output-file file, instead of the default de‐
       ducted from input-file, for the executable file produced.

       Multiple instances of the -A, and -I options can be specified.

       --list-r6rs List supported R6RS implementations.

       --list-r7rs List supported R7RS implementations.

       --list-all List all supported implementations.

       --version Show the software version.  --debug Turn on debug output.

       --help Shows you command to read this manual page. :)

ENVIRONMENT
       COMPILE_R7RS

       COMPILE_SCHEME

              Either of these environment variables must be set.   Set  either
              to  the  name  of the implementation as specified in the support
              list.  This differs from SRFI-138 as the SRFI excepts a path.

       COMPILE_SCHEME_DEBUG

              Another way in addition to --debug to turn on debug output.

STANDARDS
       SRFI    138:    Compiling    Scheme    programs     to     executables.
       https://srfi.schemers.org/srfi-138/srfi-138.html

CAVEATS
       Differences from SRFI-138

              No support for -D flag.

              Not  all  implementations  support adding to beginning or end of
              load path so -I and -A might work the same

              Only supports one input-file.

EXAMPLES
       Compile R6RS file with all dependencies in the same directory.

              COMPILE_SCHEME=SCHEME compile-scheme main.sps

       Compile R7RS file with all dependencies in the same directory.

              COMPILE_SCHEME=SCHEME compile-scheme main.scm

       Compile R6RS file with dependencies in libs directory.

              COMPILE_SCHEME=SCHEME compile-scheme -I ./libs main.sps

       Compile R7RS file with dependencies in libs directory.

              COMPILE_SCHEME=SCHEME compile-scheme -I ./libs main.scm

       Compile R6RS file with dependencies in libs directory, to output  named
       foo.

              COMPILE_SCHEME=SCHEME compile-scheme -I ./libs -o foo main.sps

       Compile  R7RS file with dependencies in libs directory, to output named
       foo.

              COMPILE_SCHEME=SCHEME compile-scheme -I ./libs -o foo main.scm

       Makefile for interpreter

              PREFIX=/usr/local
              SCHEME=chibi

              build:
                   COMPILE_SCHEME=${SCHEME} compile-scheme -I ${PREFIX}/lib/myapp/librarydirectory -o myapp myapp.scm

              install:
                   mkdir -p ${PREFIX}/lib/myapp
                   cp -r librarydirectory ${PREFIX}/lib/myapp/
                   cp  myapp.scm ${PREFIX}/lib/myapp/myapp.scm
                   install myapp ${PREFIX}/bin/myapp

                                                             compile-scheme(1)

# Install

## Linux

First install Gauche Scheme, on Debian/Ubuntu for example

    apt-get install gauche

Then build and install compile-scheme

    make
    make install

## Mac OS

First install Gauche Scheme, with brew for example

    brew install gauche

Then build and install compile-scheme

    make
    make install

## Windows
To install on windows first install Gauche Scheme. Then run install.bat as
administrator.

# Usage

## Powershell

    $env:COMPILE_SCHEME="gauche" ; compile-scheme -o main main.scm
    .\main.bat

## Wine cmd

    set COMPILE_SCHEME=gauche


<pre>
