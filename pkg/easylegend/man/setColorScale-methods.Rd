% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easylegend.R
\name{setColorScale}
\alias{setColorScale}
\alias{setColorScale.RasterLayer}
\alias{setColorScale.default}
\alias{setColorScale.matrix}
\title{Prepare color scale and legend (col, fill) from continuous-numeric data.}
\usage{
setColorScale(x, ...)

\method{setColorScale}{default}(x, col = FALSE, fill = FALSE,
  breaks = NULL, int = 1L, naCol = "lightgray", naLeg = "na",
  brackets = c("", " ", ""), labels = NULL, right = FALSE,
  include.lowest = TRUE, digits = 3, nsmall = 3, decreasing = TRUE,
  y.intersp = 1.5, ...)

\method{setColorScale}{matrix}(x, ...)

\method{setColorScale}{RasterLayer}(x, ...)
}
\arguments{
\item{x}{A vector of numerical values (continous).}

\item{col}{Logical value or vector of character strings representing colors. 
If \code{TRUE} or vector of colors, a continuous color scale 
is defined for 'x'. \code{col} and \code{fill} can't be set 
simultaneously (the legend would not work). \code{int} must be 
1 if \code{col} is set.}

\item{fill}{Logical value or vector of character strings representing fill-colors. 
If \code{TRUE} or vector of colors, a continuous fill-color scale 
is defined for 'x'. \code{col} and \code{fill} can't be set 
simultaneously (the legend would not work).}

\item{breaks}{See \code{\link[base]{cut}}.}

\item{int}{Single integer value. Number of color intervals to be defined 
in between the main fill-colors. \code{int} must be 1 if 
\code{col} is set. Intermediate colors are generated with 
\code{\link[grDevices]{colorRampPalette}}.}

\item{naCol}{Single character string. Color for \code{NA} (missing) values.}

\item{naLeg}{Single character string. Legend (label) for \code{NA} (missing) 
values.}

\item{brackets}{Vector of 3 characters. Used to generate customise legend-labels, 
if \code{labels} is \code{NULL}.}

\item{labels}{See \code{\link[base]{cut}}.}

\item{right}{See \code{\link[base]{cut}}.}

\item{include.lowest}{See \code{\link[base]{cut}}.}

\item{digits}{See \code{\link[base]{format}}.}

\item{nsmall}{See \code{\link[base]{format}}.}

\item{decreasing}{Logical value. If \code{TRUE} (default), the legend is ordered 
(sorted) in decreasing order (i.e. low values on the bottom of 
the colour scale and high values on the top of the colour scale).}

\item{y.intersp}{Single numerical value. Character interspacing factor for 
vertical (y) spacing of the color legend. Passed to 
\code{\link[graphics]{legend}}.}

\item{\dots}{Additional parameters passed to specific methods.}
}
\value{
Returns a list of 2 \code{\link[base]{function}}: 
 \code{col} or \code{fill}, a function that converts 
 \code{x}-like values into (fill)colors; 
 \code{legend}, a function to draw a legend on a plot with the 
 correct (fill)colors and legend. That function accepts \code{...} 
 arguments, passed to the original \code{\link[graphics]{legend}} 
 function.
}
\description{
Prepare color scale and legend (col, fill) from continuous-numeric data.
}
\examples{

# GENERATE DUMMY DATASET X-Y-Z OF CORRELATED VARIABLES + GROUPS
# =============================================================

#  Base variable (x) and groups (g)
n <- 200 
x <- rnorm( n ) 
g <- sample( x = c("a","b"), size = n, replace = TRUE ) 

#   y is a function of x and group and random noise
y <- 2*x + rnorm( n ) 
y[ g == "a" ] <- y[ g == "a" ] + 1
y[ g == "b" ] <- y[ g == "b" ] + 2

#   z is a function of x and random noise
z <- 3*x + rnorm( n ) 


#   Bind into a data.frame
xyz <- data.frame( "x" = x, "y" = y, "z" = z, "g" = g, 
    stringsAsFactors = FALSE )
rm( x, y, z, g )

#   Missing group values
xyz[ sample(x=1:n,size=round(n/5)), "g" ] <- NA

#   Inspect the data.frame
head( xyz )



# X-Y PLOT WITH OVERLAY OF CONTINUOUS COLOR & CATEGORICAL SYMBOL 
# ==============================================================

library( "easylegend" )

#   'Calibrate' the legend
fg <- setFactorGraphics( x = xyz[, "g" ], pch = TRUE  )
cs <- setColorScale(     x = xyz[, "z" ], fill = TRUE ) 

#   Plot x y and g
plot( x = xyz[, "x" ], y = xyz[, "y" ], col = cs$fill( xyz[, "z" ] ), 
    pch = fg$pch( xyz[, "g" ] ), panel.first = grid() )

#   Add legend
fg$legend( x = "bottomright", title = "Group:", bty = "n" )
cs$legend( x = "topleft",     title = "Z:",    bty = "n" )

rm( fg, cs )



# X-Y PLOT , CUSTOMISED
# =====================

#   'Calibrate' the legend (1)
fg <- setFactorGraphics( x = xyz[, "g" ], pch = 15:16 )

fill <- hsv( h = 0.21, s = .8, v = seq( .8, .2, length.out = 5 ) )

#   'Calibrate' the legend (2)
cs  <- setColorScale( x = xyz[, "z" ], fill = fill, int = 4, 
    nsmall = 1, digits = 1 ) 

#   Plot x y and g
plot( x = xyz[, "x" ], y = xyz[, "y" ], col = cs$fill( xyz[, "z" ] ), 
    pch = fg$pch( xyz[, "g" ] ), panel.first = grid() )

#   Add legend
fg$legend( x = "bottomright", title = "Group:", bty = "n" )
cs$legend( x = "topleft", title = "Z:", bty = "n" ) # , style = 1

rm( fg, cs, fill ) 



# X-Y PLOT, MISSING Z-VALUES(with intermediate colors)
# ====================================================

xyz[ is.na(xyz[, "g" ]), "z" ] <- NA

#   'Calibrate' the legend
cs <- setColorScale( x = xyz[, "z" ], fill = TRUE, int = 4 ) 

#   Plot x y and g
plot( x = xyz[, "x" ], y = xyz[, "y" ], col = cs$fill( xyz[, "z" ] ), 
    pch = 16, panel.first = grid() )

#   Add legend
cs$legend( x = "topleft",     title = "Z:",    bty = "n" )

rm( cs )



# MATRIX AS COLORED IMAGE
# =======================

m <- matrix( data = rnorm(8*10), 8, 10 ) 
cs  <- setColorScale( m, fill = TRUE ) 

image( x = m, col = cs[[ "iFill" ]], breaks = cs[[ "iBreaks" ]], 
    bty = "n" ) # , xlim = c(0,1.5), asp = 1

cs$legend( x = "topright", bg = gray(1,alpha=.5) )

rm( cs )



# MATRIX, CUSTOMISED
# ==================

cs  <- setColorScale( m, fill = TRUE, int = 4, nsmall = 1, 
    digits = 1 ) 

par( "mar" = c( 5, 4, 4, 5 ) + .1 )

image( x = m, col = cs[[ "iFill" ]], breaks = cs[[ "iBreaks" ]], 
    bty = "n" ) # , xlim = c(0,1.5), asp = 1

plotAnywhere( cs$legend( x = "right", "title" = "Values" ) )

rm( cs )



# MATRIX WITH LARGE RANGE OF VALUES
# =================================

breaks <- c(100,10,1,0)
m <- matrix( 
    data = c( 
        runif( 300, breaks[2], breaks[1] ), 
        runif( 300, breaks[3], breaks[2] ), 
        runif( 300, breaks[4], breaks[3] ) ), 
    30, 30, byrow = TRUE ) 

#   Add missing values
m[ sample(1:nrow(m),3), sample(1:ncol(m),3) ] <- NA 


cs  <- setColorScale( m, fill = c("lightgreen","orange","darkred"), 
    int = 4, breaks = breaks, digits = 0, nsmall = 0 ) 

#   Standard plot: CAN'T SEE ANYTHING
image( matrix2image( m ) ) 

par( "mar" = c(5.1, 4.1, 4.1, 6.1) ) 

image( x = matrix2image( m ), col = cs[[ "iFill" ]], breaks = cs[[ "iBreaks" ]], 
    bty = "n" ) 

plotAnywhere( expr = cs$legend( x = "right", bty = "n" ) )

rm( cs )



# MORE TESTS
# ==========

# X-Y PLOT, MISSING Z-VALUES (without intermediate colors)
# --------------------------------------------------------

#   'Calibrate' the legend
cs <- setColorScale( x = xyz[, "z" ], fill = TRUE ) 

#   Plot x y and g
plot( x = xyz[, "x" ], y = xyz[, "y" ], col = cs$fill( xyz[, "z" ] ), 
    pch = 16, panel.first = grid() )

#   Add legend
cs$legend( x = "topleft",     title = "Z:",    bty = "n" )

rm( cs )



# X-Y PLOT, MISSING Z-VALUES AND INFINTE BOUNDS
# ---------------------------------------------

#   'Calibrate' the legend
fill <- hsv( h = 0.21, s = .8, v = seq( .8, .2, length.out = 5 ) ) 

breaks <- quantile( xyz[, "z" ], na.rm = TRUE, probs = seq( 1, 0, -.2 ) )
breaks[ c(1,length(breaks)) ] <- c( +Inf, -Inf )

cs <- setColorScale( x = xyz[, "z" ], fill = fill, int = 4, 
    breaks = breaks ) 

#   Plot x y and g
plot( x = xyz[, "x" ], y = xyz[, "y" ], col = cs$fill( xyz[, "z" ] ), 
    pch = 16, panel.first = grid() )

#   Add legend
cs$legend( x = "topleft",     title = "Z:",    bty = "n" )

rm( cs, fill, breaks )



# MATRIX, CUSTOMISED (2 CATEGORIES)
# ---------------------------------

m <- matrix( data = rnorm(8*10), 8, 10 ) 

cs  <- setColorScale( m, fill = c("darkred","lightyellow"), 
    int = 4, breaks = c(+Inf,0,-Inf), digits = 1, nsmall = 1 ) 

par( "mar" = c(5, 4, 4, 5) + .1 ) 

image( x = matrix2image( m ), col = cs[[ "iFill" ]], 
    breaks = cs[[ "iBreaks" ]], bty = "n", asp = 1 ) 

plotAnywhere( expr = cs$legend( x = "right", bty = "n" ) )

rm( cs )



# MATRIX, CUSTOMISED (3 CATEGORIES)
# ---------------------------------

cs  <- setColorScale( m, fill = c("darkred","orange","lightyellow"), 
    int = 4, breaks = c(+Inf,1,-1,-Inf), digits = 1, nsmall = 1 ) 

par( "mar" = c(5.1, 4.1, 4.1, 6.1) ) 

image( x = matrix2image( m ), col = cs[[ "iFill" ]], 
    breaks = cs[[ "iBreaks" ]], bty = "n", asp = 1 ) 

plotAnywhere( expr = cs$legend( x = "right", bty = "n" ) )

#   Extra legend in reverse order
cs2  <- setColorScale( m, fill = c("lightyellow","orange","darkred"), 
    int = 4, breaks = c(-Inf,-1,+1,+Inf), digits = 1, nsmall = 1, 
    decreasing = FALSE ) 

plotAnywhere( expr = cs2$legend( x = "bottomright", bty = "n" ) )

rm( cs, cs2 )



# MATRIX, MISSING VALUES
# ----------------------

#   Add missing values
m[ sample(1:nrow(m),3), sample(1:ncol(m),3) ] <- NA 

cs  <- setColorScale( m, fill = TRUE, int = 4, digits = 1, 
    nsmall = 1 ) 

par( "mar" = c(5.1, 4.1, 4.1, 6.1) ) 

image( x = matrix2image( m ), col = cs[[ "iFill" ]], 
    breaks = cs[[ "iBreaks" ]], bty = "n", asp = 1 ) 

plotAnywhere( expr = cs$legend( x = "right", bty = "n" ) )

rm( cs )

}

