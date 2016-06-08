
setwd( wd <- sprintf( "%s/easylegend/www", Sys.getenv("rPackagesDir") ) ) 

library( "raster" ) 
library( "easylegend" )

# RasterLayer
r <- raster(nrows=10, ncols=10)
r <- setValues(r, log10( 1:ncell(r) ))  # NOTICE THE LOG SCALE

qu <- quantile( x = values(r), probs = seq( 1, 0, length.out = 5 ) )

#   'Calibrate' the legend (2)
cs  <- setColourRampScale( 
    x      = r, 
    breaks = qu, 
    fill   = c( "darkred", "red", "orange", "lightgreen", "lightblue" ), 
    int    = 4, 
    nsmall = 1, digits = 1 ) 


#   Generate the image-plot
png( 
    filename = "img/example03.png", 
    width    = 500, 
    height   = 500, 
    res      = 75 ) 
    
    par( las = 1, family = "serif", font = 2, font.lab = 2, 
        font.axis = 2, lwd = 1 )
    
    #   Plot x y and g
    plot( 
        x      = r, 
        breaks = cs$iBreaks, 
        col    = cs$iFill, 
        xlab   = "Longitude", 
        ylab   = "Latitude", 
        main   = "Continuous aesthetics & legends for a raster", 
        legend = FALSE ) # Important
    
    plotAnywhere( cs$legend( x = "right", title = "Values:", bty = "n" ) )
    
dev.off() 

