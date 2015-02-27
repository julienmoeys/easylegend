set pkgname=easylegend

cd /D "%rPackagesDir%\%pkgname%\pkg" 

R CMD INSTALL --build --compact-docs --byte-compile %pkgname%

pause
