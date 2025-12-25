
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
