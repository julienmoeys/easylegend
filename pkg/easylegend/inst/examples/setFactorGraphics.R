
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



# CATEGORY OVERLAY ON A X-Y PLOT (POINTS) AND REGRESSION LINE
# ===========================================================

library( "easylegend" ) 

#   'Calibrate' the legend
fg <- setFactorGraphics( x = xyz[, "g" ], col = TRUE, pch = TRUE )

#   Plot x y and g
plot( x = xyz[, "x" ], y = xyz[, "y" ], col = fg$col( xyz[, "g" ] ), 
    pch = fg$pch( xyz[, "g" ] ), panel.first = grid() )

#   Add regression lines
for( gi in unique( xyz[, "g" ] ) ){ 
    abline( lm( y ~ x, data = subset( xyz, g == gi ) ), col = fg$col( gi ) ) 
}   

#   Add legend
fg$legend( x = "bottomright", title = "Group:", bty = "n" )
fg$legend( x = "topleft", pch = NULL, lwd = 1, title = "Regression:", bty = "n" )

rm( fg )



# SAME, with MISSING GROUP-VALUES
# ===============================

xyz[ sample(x=1:n,size=round(n/5)), "g" ] <- NA

#   'Calibrate' the legend
fg <- setFactorGraphics( x = xyz[, "g" ], col = TRUE, pch = TRUE )

#   Plot x y and g
plot( x = xyz[, "x" ], y = xyz[, "y" ], col = fg$col( xyz[, "g" ] ), 
    pch = fg$pch( xyz[, "g" ] ), panel.first = grid() )

#   Add regression lines
abline( lm( y ~ x, data = subset( xyz, g == "a" )   ), col = fg$col( "a" ) ) 
abline( lm( y ~ x, data = subset( xyz, g == "b" )   ), col = fg$col( "b" ) ) 
abline( lm( y ~ x, data = subset( xyz, is.na( g ) ) ), col = fg$col( NA ) ) 

fg$legend( x = "bottomright", title = "Group:", bty = "n", lwd = 1 )

rm( fg )
