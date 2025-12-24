
if not exist "%ProgramFiles%\compile-scheme" mkdir "%ProgramFiles%\compile-scheme"
if not exist "%ProgramFiles%\compile-scheme\libs" mkdir "%ProgramFiles%\compile-scheme\libs"
copy compile-scheme.bat "%ProgramFiles%\compile-scheme\"
copy compile-scheme.scm "%ProgramFiles%\compile-scheme\"
copy libs\implementations.sld "%ProgramFiles%\compile-scheme\libs\"
copy libs\library-util.sld "%ProgramFiles%\compile-scheme\libs\"
copy libs\util.sld "%ProgramFiles%\compile-scheme\libs\"
if x%PATH:compile-scheme=%==x%PATH% setx PATH "%PATH%;%ProgramFiles%\compile-scheme"
