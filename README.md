Linux: [![Linux Build Status (Travis CI)](https://travis-ci.org/julienmoeys/easylegend.svg?branch=master)](https://travis-ci.org/julienmoeys/easylegend). 
Windows: [![Windows Build Status (AppVeyor)](https://ci.appveyor.com/api/projects/status/github/julienmoeys/easylegend?branch=master&svg=true)](https://ci.appveyor.com/project/julienmoeys/easylegend)

easylegend
==========

_Automatic plot overlay and legend for categorical and continuous 
variables (symbols, colors, ...)_

Author: **Julien MOEYS**.
Package description: See [DESCRIPTION](/pkg/easylegend/DESCRIPTION).

Introduction
------------

`easylegend`  is a [package][RPackages] for the [R Project for 
Statistical Computing][R]

`easylegend` makes it easier to display additional categorical 
or continuous variable(s) (say `Z`) on a plot (`xy` scatter plot, 
image, spatial data), with `Z`-dependent symbol characters and / or
colors (for symbols, lines or fillings), and preparing the plot 
legend(s). Color scales can be categorical (if `Z` is categorical), 
or continuous (if `Z` is continuous).

`easylegend` generates `Z`-dependent plot aesthetics automatically, 
but the user can customise many aesthetics parameters.

Continuous color scales (or ramps) in `easylegend` can be highly 
customised, and support **multi-step color ramps** with intermediate 
color gradient and custom breaks. **The legend for continuous color 
scales is not linearly proportional to `Z`, and thus can be used 
with data that spans across several order of magnitudes** (Such as 
pesticide concentrations, which is the primary motivation for that 
package), or with infinite values in the breaks (_i.e._ Upper or 
lower bounds not clearly defined). They can be used with standard 
plots, but also with `image()` or `raster::plot()`.


Examples
--------

Here are a simple examples of what can be obtained with `easylegend`:

### Example 01

[![Image 01, Example, package easylegend](www/img/example01.png "Image 01, Example, package easylegend")](www/example01.R)

_Click on the image to view the source code of this example_.

### Example 02

[![Image 02, Example, package easylegend](www/img/example02.png "Image 02, Example, package easylegend")](www/example02.R)

_Click on the image to view the source code of this example_.

### Example 03

This example show how `easylegend` can be used with the package 
[raster](http://cran.r-project.org/web/packages/raster/index.html) 
to generate custom legends for data that span across a log-scale. 

[![Image 03, Example, package easylegend](www/img/example03.png "Image 03, Example, package easylegend")](www/example02.R)

_Click on the image to view the source code of this example_.

Please notice that the package [rasterVis][] provides advanced 
way to customise a raster-plot (but as far as I know not for custom 
non-linear legends like the one above)



<!--- Links         -->
[R]:                http://www.r-project.org/ "The R Project for Statistical Computing"
[RPackages]:        http://en.wikipedia.org/wiki/R_%28programming_language%29#Packages "R packages (Wikipedia)" 
[rasterVis]:        http://cran.r-project.org/web/packages/rasterVis/index.html "package rasterVis (CRAN)"
[raster]:           http://cran.r-project.org/web/packages/raster/index.html "package raster (CRAN)" 
