@echo off

if not exist "%ProgramFiles%\compile-scheme" mkdir "%ProgramFiles%\compile-scheme"
if not exist "%ProgramFiles%\compile-scheme" mkdir "%ProgramFiles%\compile-scheme\libs"
xcopy compile-scheme.bat "%ProgramFiles%\compile-scheme\" /y
xcopy compile-scheme.scm "%ProgramFiles%\compile-scheme\" /y
xcopy libs\implementations.sld "%ProgramFiles%\compile-scheme\libs\" /y
xcopy libs\library-util.sld "%ProgramFiles%\compile-scheme\libs\" /y
xcopy libs\util.sld "%ProgramFiles%\compile-scheme\libs\" /y
setx path "%PATH%;%ProgramFiles%\compile-scheme"
