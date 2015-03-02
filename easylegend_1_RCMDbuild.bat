set pkgname=easylegend

cd /D "%rPackagesDir%\%pkgname%\pkg" 

git log -n 1 --oneline --no-notes > %pkgname%\inst\GIT_VERSION

R CMD build --compact-vignettes="gs+qpdf" %pkgname% 

pause
