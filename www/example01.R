
setwd( "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/easylegend/www" )


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



# X-Y PLOT , CUSTOMISED
# =====================

#   'Calibrate' the legend (1)
fg <- setFactorGraphics( x = xyz[, "g" ], pch = 15:16 )

fill <- hsv( h = 0.21, s = .8, v = seq( .8, .2, length.out = 5 ) )

#   'Calibrate' the legend (2)
cs  <- setColorScale( x = xyz[, "z" ], fill = fill, int = 4, 
    nsmall = 1, digits = 1 ) 

#   Generate the image-plot
png( 
    filename = "img/example01.png", 
    width    = 500, 
    height   = 500, 
    res      = 75 ) 
    
    par( las = 1, family = "serif", font = 2, font.lab = 2, 
        font.axis = 2, lwd = 1 )
    
    #   Plot x y and g
    plot( 
        x    = xyz[, "x" ], 
        y    = xyz[, "y" ], 
        col  = cs$fill( xyz[, "z" ] ), 
        pch  = fg$pch( xyz[, "g" ] ), 
        xlab = "X", 
        ylab = "Y", 
        main = "Categorical and continuous aesthetics & legends", 
        panel.first = grid() )
    
    #   Add legend
    fg$legend( x = "bottomright", title = "Group:", bty = "n" )
    cs$legend( x = "topleft", title = "Z:", bty = "n" ) # , style = 1
    
dev.off() 

rm( fg, cs, fill ) 


