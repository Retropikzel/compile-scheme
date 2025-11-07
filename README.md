<pre>
compile-r7rs(1)             General Commands Manual             compile-r7rs(1)

NAME
       compile-r7rs - a tool to compile R6RS and R7RS Scheme programs

ENVIRONMENT
       COMPILE_R7RS  environment variable must be set to the name of the imple‐
       mentation as specified in the support list.   **This  differs  from  the
       SRFI** as the SRFI excepts a path.

SYNOPSIS
       compile-r7rs [-n BITS] [--bits BITS] file...  compile-r7rs

DESCRIPTION
       compile-r7rs

       It  aims  to support Scheme SRFI-138 - Compiling Scheme programs to exe‐
       cutables.

CAVEATS
       -      No support for -D flag.

       -      Not all implementations support adding to  beginning  or  end  of
              load path so -I and -A might work the same

OPTIONS
       -n, --bits=BITS Set the number of bits to modify.  Default is one bit.

                                                                compile-r7rs(1)
</pre>