compile-r7rs is a tool to compile Scheme programs, it aims for compability
with [SRFI-138](https://srfi.schemers.org/srfi-138/srfi-138.html).

Despite it's name it also supports R6RS. Schemers, unite! <3

[Jenkins](https://jenkins.scheme.org/job/retropikzel/job/compile-r7rs/)

## Notes

- No support for -D flag yet.
- Not all implementations support adding to beginning or end o load path so
-I and -A might work the same

## Build and install

You can run compile-r7rs on Chibi, Chicken, Gauche, Guile, Kawa, Sagittarius or
STklos.

    snow-chibi --impls=SCHEME "(foreign c)"
    snow-chibi --impls=SCHEME "(srfi 170)"
    make build-SCHEME
    make install

## Usage

### R6RS

Replace the .scm file with .sps file in the next section.

### R7RS

You need to install each Scheme implementation yourself.

The environment variable COMPILE\_R7RS must be set to the **name** of the
implementation as specified in the support list.
**This differs from the SRFI** as the SRFI excepts a path.

To get the list of supported R6RS implementations run:

    compile-r7rs --list-r6rs-schemes

To get the list of supported R7RS implementations run:

    compile-r7rs --list-r7rs-schemes

To get the list of all supported implementations run:

    compile-r7rs --list-schemes

Then run it with the .scm file for r7rs, or .sps file for r6rs.

    COMPILE_R7RS=<implementation name> compile-r7rs -I . -o main main.scm

Which produces file called main, which you can run. Note that when given Scheme
is interpreter the file contains commands that run the script, and even when
the file is combiled binary it might need the compiled libraries.

No other file suffixes are supported at the moment.

Setting value of COMPILE\_R7RS to implementation name that supports only r7rs
and input file to .sps file and other way around is undefined behaviour.

### Environment variables

- COMPILE\_R7RS
    - **Name** of the implementation you want to compile with
    - **This differs from the SRFI** as it excepts a path
- COMPILE\_R7RS\_SCHEME_NAME
    - Additional string to insert right after the command and it's arguments
    can be used for example to pass C compiler flags on implementations that
    compile to C or anything or otherwise as backdoor
    - For example for Chicken to link with libcurl you would set
    COMPILE\_R7RS\_CHICKEN="-L -lcurl"
    - If implementation has - it is changed to \_, for example mit-scheme ->
    MIT\_SCHEME
    - **This differs from the SRFI** as it's not in there

## Supported implementations

Some implementations support both compiling and interpreting, in that
case only the compiler functionality is used and the implementation is marked
as compiler.

- chezscheme
    - interpreter
    - R6RS
- chibi
    - interpreter
    - R7RS
- chicken
    - compiler
    - R7RS
- cyclone
    - compiler
    - R7RS
- foment
    - interpreter
    - R7RS
- gauche
    - interpreter
    - R7RS
- guile
    - interpreter
    - R6RS
    - R7RS
        - Has include bug https://debbugs.gnu.org/cgi/bugreport.cgi?bug=66046
        but for some reason it seems to work for me atleast sometimes
- ikarus
    - interpreter
    - R6RS
- ironscheme
    - interpreter
    - R6RS
- kawa
    - interpreter
    - R7RS
    - Native access is enabled by default so pffi can work
- larceny
    - interpreter
    - R6RS
    - R7RS
- loko
    - compiler
    - R6RS
    - R7RS
- meevax
    - r7rs
- mit-scheme
    - interpreter
    - R7RS
- mosh
    - interpreter
    - R6RS
    - R7RS
- racket
    - interpreter
        - Has compiling capabilities but I havent got them to work yet
    - r6rs
    - r7rs
- sagittarius
    - interpreter
    - R6RS
    - R7RS
- skint
    - interpreter
    - R7RS
- stklos
    - interpreter
    - R7RS
- tr7
    - interpreter
    - R7RS
- ypsilon
    - interpreter
    - R6RS
    - R7RS

## Roadmap

- Support for more implementations
    - gambit
    - husk
        - Dont know how to add directories to load path yet, might not be
        implemented
        - r7rs
    - picrin
        - Might not be possible, seems to not have (include...) that works like
        others
        - r7rs
    - scheme-rs
        - Waiting for implementation support
        - r6rs
        - r7rs
    - stak
        - Waiting for implementation support https://github.com/raviqqe/stak/issues/2355
        - r7rs
    - vicare
        - So old that I have problems compiling it in Docker, so testing is
        hard but I expect it to work once I get it to compile as it is R6RS
        implementation
        - r6rs
- Better and tested support for Windows
    - Right now there is support for running this but I can not quarantee it
    works on all if any cases
- Support for -D
    - Most implementations dont have this or equivalent flag, but it would be
    really nice feature to have so filing issues and implementing it myself is
    something I would like to do
- Ask implementations to support adding to the front and back of load path, or
    implement this onto implementations myself
    - This might not be as important, but it would be nice to go towards SRFI-138
    conformaty
- Environment variable to force the target operating system
    - Since for example for interpreters the program produces .bat file with
    command to run the interpreter "cross compiling" is easy.


