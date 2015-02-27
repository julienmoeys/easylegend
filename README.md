easylegend
==========

_Automatic plot overlay and legend for categorical and continuous 
variables (symbols, colors, ...)_

Author: **Julien MOEYS**.
Package description: See [DESCRIPTION](/pkg/easylegend/DESCRIPTION).

`easylegend`  is a [package][RPackages] for the [R Project for 
Statistical Computing][R]


`easylegend` makes it easier to display additional categorical 
or continuous variable(s) (say '**Z**') on a plot (x-y scatter plot, 
image, spatial data), with Z-dependent symbol characters and / or
colors (for symbols, lines or fillings), and preparing the plot 
legend(s). Color scales can be categorical (if Z is categorical), 
or continuous (if Z is continuous).


`easylegend` generates Z-dependent plot 'aesthetics' automatically, 
but the user can customise many 'aesthetics' parameters.


Continuous color scales (or ramps) in `easylegend` can be highly 
customised, and support **multi-step color ramps** with intermediate 
color gradient and custom breaks. **The legend for continuous color 
scales is not (Z)'proportional', and thus can be used with data 
that spans across several order of magnitudes**, or with infinite 
values in the breaks. They can be used with standard plots, but 
also with `image()` or `raster::plot()`.

More information coming soon.


<!--- List of links used in the page -->
[helpPage]:     easylegend/00Index.html "easylegend help page"
[R]:            http://www.r-project.org/ "The R Project for Statistical Computing"
[RPackages]:    http://en.wikipedia.org/wiki/R_%28programming_language%29#Packages "R packages (Wikipedia)" 
