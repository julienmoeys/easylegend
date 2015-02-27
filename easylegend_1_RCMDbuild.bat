set pkgname=easylegend

cd /D "%rPackagesDir%\%pkgname%\pkg" 

svnversion > %pkgname%\inst\SVN_VERSION

R CMD build --compact-vignettes="gs+qpdf" %pkgname% 

pause
