
rm(list=ls(all=TRUE)) 
pkgName <- "easylegend"
pkgDir  <- sprintf( "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/%s/pkg", pkgName )


library( "roxygen2" )


# Source some utility functions
source( file.path( pkgDir, "..","packageUtilities.R" ) ) 


# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = pkgDir, 
    pkgVersion  = "1.2.4", 
    pkgDepends  = NULL, 
    pkgImports  = "grDevices", 
    pkgSuggests = NULL, 
    RVersion    = NULL   
)   



roxygenize( 
    package.dir   = file.path( pkgDir, pkgName ), 
    # unlink.target = TRUE, 
    roclets       = c( "namespace", "rd" ) # "collate" 
)   


pkgRemove( pkgName = pkgName ) 
