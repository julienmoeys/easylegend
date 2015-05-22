set pkgname=easylegend
set version=1.2.6

cd /D "%rPackagesDir%\%pkgname%\pkg" 

R CMD Rd2pdf %pkgname% --output=%pkgname%.%version%.pdf

pause
