set pkgname=easylegend

cd /D "%rPackagesDir%\%pkgname%\pkg" 

R CMD check %pkgname%

@REM --as-cran --no-tests 

pause
