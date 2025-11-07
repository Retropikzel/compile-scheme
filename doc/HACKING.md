## Roadmap

- Support for more implementations
    - gambit
        - Adding paths to compiled executable is such a pain that no support yet
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
