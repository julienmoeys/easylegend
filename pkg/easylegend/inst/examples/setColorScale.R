
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
        runif( 300, breaks[1], breaks[2] ), 
        runif( 300, breaks[2], breaks[3] ), 
        runif( 300, breaks[3], breaks[4] ) ), 
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

breaks <- quantile( xyz[, "z" ], na.rm = TRUE, probs = seq( 0, 1, .2 ) )
breaks[ c(1,length(breaks)) ] <- c( -Inf, +Inf )

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

cs  <- setColorScale( m, fill = c("lightyellow","darkred"), 
    int = 4, breaks = c(-Inf,0,+Inf), digits = 1, nsmall = 1 ) 

par( "mar" = c(5, 4, 4, 5) + .1 ) 

image( x = matrix2image( m ), col = cs[[ "iFill" ]], 
    breaks = cs[[ "iBreaks" ]], bty = "n", asp = 1 ) 

plotAnywhere( expr = cs$legend( x = "right", bty = "n" ) )

rm( cs )



# MATRIX, CUSTOMISED (3 CATEGORIES)
# ---------------------------------

cs  <- setColorScale( m, fill = c("lightyellow","orange","darkred"), 
    int = 4, breaks = c(-Inf,-1,1,+Inf), digits = 1, nsmall = 1 ) 

par( "mar" = c(5.1, 4.1, 4.1, 6.1) ) 

image( x = matrix2image( m ), col = cs[[ "iFill" ]], 
    breaks = cs[[ "iBreaks" ]], bty = "n", asp = 1 ) 

plotAnywhere( expr = cs$legend( x = "right", bty = "n" ) )

rm( cs )



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

