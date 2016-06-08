
#'@importFrom utils packageVersion
NULL 

.onAttach <- function(# Internal. Message displayed when loading the package.
 libname, 
 pkgname  
){      
    # Welcome message
    if( interactive() ){ 
        # svnVersion <- system.file( "SVN_VERSION", package = pkgname ) 
        
        # if( svnVersion != "" ){ 
            # svnVersion <- readLines( con = svnVersion )[ 1L ] 
            # svnVersion <- sprintf( "(svn revision: %s)", svnVersion ) 
        # }else{ 
            # svnVersion <- "(svn revision: ?)" 
        # }   
        
        gitVersion <- system.file( "GIT_VERSION", package = pkgname ) 
        
        if( gitVersion != "" ){ 
            gitVersion <- readLines( con = gitVersion )[ 1L ] 
            gitVersion <- strsplit( x = gitVersion, split = " ", 
                fixed = TRUE )[[ 1L ]][ 1L ]
            
            gitVersion <- sprintf( "(git revision: %s)", gitVersion ) 
        }else{ 
            gitVersion <- "(git revision: ?)" 
        }   
        
        msg <- sprintf( 
            "%s %s %s. For help type: help(pack='%s')", 
            pkgname, 
            as.character( utils::packageVersion( pkgname ) ), 
            gitVersion, # svnVersion
            pkgname ) 
        
        packageStartupMessage( msg ) 
    }   
}   

