
rm(list=ls(all=TRUE)) 
pkgName <- "easylegend"
pkgDir  <- sprintf( 
    "%s/%s/pkg", 
    Sys.getenv("rPackagesDir"), 
    pkgName )



library( "roxygen2" )


# Source some utility functions
source( file.path( pkgDir, "..","packageUtilities.R" ) ) 


# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = pkgDir, 
    pkgVersion  = "1.4.1", 
    pkgDepends  = NULL, 
    pkgImports  = c( "graphics", "grDevices", "utils", "stats" ), 
    pkgSuggests = NULL, 
    RVersion    = NULL   
)   



roxygenize( 
    package.dir   = file.path( pkgDir, pkgName ), 
    # unlink.target = TRUE, 
    roclets       = c( "namespace", "rd" ) # "collate" 
)   


pkgRemove( pkgName = pkgName ) 
