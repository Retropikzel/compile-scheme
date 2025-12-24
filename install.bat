@echo off

echo Installing...

if not exist "%ProgramFiles%\compile-scheme" mkdir "%ProgramFiles%\compile-scheme"
if not exist "%ProgramFiles%\compile-scheme\libs" mkdir "%ProgramFiles%\compile-scheme\libs"
copy compile-scheme.bat "%ProgramFiles%\compile-scheme\" > nul
copy compile-scheme.scm "%ProgramFiles%\compile-scheme\" > nul
copy libs\implementations.sld "%ProgramFiles%\compile-scheme\libs\" > nul
copy libs\library-util.sld "%ProgramFiles%\compile-scheme\libs\" > nul
copy libs\util.sld "%ProgramFiles%\compile-scheme\libs\" > nul

set pathone='%PATH: =_%'
set pathtwo='%pathone:compile-scheme=%'

if not "%pathone%"=="%pathtwo%" setx PATH "%PATH%;%ProgramFiles%\compile-scheme"

echo Install complete
