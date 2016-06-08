
setwd( wd <- sprintf( "%s/easylegend/www", Sys.getenv("rPackagesDir") ) ) 

# GENERATE DUMMY DATASET X-Y-Z OF CORRELATED VARIABLES + GROUPS
# =============================================================

set.seed( 20160608 )

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
#   with MISSING GROUP-VALUES

library( "easylegend" ) 

xyz[ sample(x=1:n,size=round(n/5)), "g" ] <- NA

#   'Calibrate' the legend
fg <- setFactorGraphics( x = xyz[, "g" ], col = TRUE, pch = TRUE )

#   Generate the image-plot
png( 
    filename = "img/example02.png", 
    width    = 500, 
    height   = 500, 
    res      = 75 ) 
    
    par( las = 1, family = "serif", font = 2, font.lab = 2, 
        font.axis = 2, lwd = 1 )
    
    #   Plot x y and g
    plot( 
        x    = xyz[, "x" ], 
        y    = xyz[, "y" ], 
        col  = fg$col( xyz[, "g" ] ), 
        pch  = fg$pch( xyz[, "g" ] ), 
        xlab = "X", 
        ylab = "Y", 
        main = "Categorical aesthetics & legends", 
        panel.first = grid() )
    
    #   Add regression lines
    abline( lm( y ~ x, data = subset( xyz, g == "a" )   ), col = fg$col( "a" ) ) 
    abline( lm( y ~ x, data = subset( xyz, g == "b" )   ), col = fg$col( "b" ) ) 
    abline( lm( y ~ x, data = subset( xyz, is.na( g ) ) ), col = fg$col( NA ) ) 
    
    fg$legend( x = "bottomright", title = "Group:", bty = "n", lwd = 1 )
 
dev.off() 

rm( fg )
