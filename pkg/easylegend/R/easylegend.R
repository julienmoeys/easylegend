
# +-------------------------------------------------------------+
# | Language: R + roxygen2 inline documentation
# | Package: easylegend 
# | Author(s): Julien Moeys <Julien.Moeys@@slu.se> 
# | License: AGPL3, Affero General Public License version 3 
# +-------------------------------------------------------------+

# source( "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/easylegend/pkg/easylegend/R/easylegend.R" )
# rm( list = ls( all = TRUE ) )

# http://www.statmethods.net/advgraphs/parameters.html



# .generatePch ==================================================

.generatePch <- function(# Generate a range of point symbol codes
### Generate a range of point symbol codes.

 n, 
### Number of point symbol values to be generated

 exclude=numeric(0)

){  
    testIn <- exclude %in% 1:25
    if( any( testIn ) ){ 
        minus <- length( exclude[ testIn ] ) 
    }else{ 
        minus <- 0
    };  rm( testIn ) 
    
    if( n <= (25-minus) ){ 
        pch <- 1:25
        pch <- pch[ !(pch %in% exclude) ]
        pch <- (1:25)[ 1:n ] 
        
    }else{ 
        l <- c( letters, LETTERS ) 
        l <- l[ !(l %in% exclude) ] 
        
        if( n <= length( l ) ){ 
            pch <- l[ 1:n ] 
            
        }else{ 
            pch <- c( l, as.character( 1:( n - length( l ) ) ) ) 
            
            if( any( exclude %in% pch ) ){ 
                pch <- pch[ !(pch %in% exclude) ] 
                pch <- c( pch, as.character( n - length( l ) + 1 ) )
            }   
            
        }   
    }   
    
    return( pch ) 
}   

    # .generatePch( 25 )
    # .generatePch( 25, exclude = 1 ) 
    # .generatePch( 25, exclude = 25 ) 
    # .generatePch( 54, exclude = 1 )



# setFactorGraphics =============================================

#' Prepare a graphics and legend (col, pch, lty) from categorical data.
#'
#' Prepare a graphics and legend (col, pch, lty) from categorical data.
#'
#'
#'@param x
#'  A vector of numerical, character, boolean or factor values, 
#'  representing categories (non-continuous, with a limited number 
#'  of unique values).
#'
#'@param col
#'  Logical value. If \code{TRUE}, a color will be attributed to 
#'  each category in \code{x}, and the legend prepared for 
#'  lines or symbols colors. Alternatively, can also be a vector 
#'  of character strings representing R colors (see 'Color 
#'  Specification' in \code{\link[graphics]{par}}), with one value 
#'  per category in \code{x}. \bold{Should be in the same order as 
#'  values in \code{unique(x)}}.
#'
#'@param pch
#'  Logical value. If \code{TRUE}, a symbol will be attributed to 
#'  each category in \code{x}. Alternatively, can also be a vector 
#'  of point symbols, with one value per category in \code{x}. 
#'  \bold{Should be in the same order as values in \code{unique(x)}}.
#'
#'@param fill
#'  Logical value. If \code{TRUE}, a color will be attributed to 
#'  each category in \code{x}, and the legend prepared for 
#'  filled colors. Alternatively, can also be a vector 
#'  of character strings representing R colors (see 'Color 
#'  Specification' in \code{\link[graphics]{par}}), with one value 
#'  per category in \code{x}. \bold{Should be in the same order as 
#'  values in \code{unique(x)}}.
#'
#'@param leg
#'  A vector of character strings with the label of each 
#'  category in the legend. If \code{NULL}, legend's \code{legends} 
#'  will be the same as \code{as.character(values)}.
#'
#'@param ord
#'  Logical value. If \code{TRUE}, the legend is ordered (sorted).
#'
#'@param naCol
#'  Single character string. Color for \code{NA} (missing) values.
#'
#'@param naFill
#'  Single character string. Fill-color for \code{NA} (missing) values.
#'
#'@param naPch
#'  Single character string. Symbol for \code{NA} (missing) values.
#'
#'@param naLeg
#'  Single character string. Legend (label) for \code{NA} (missing) 
#'  values.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a list of 3 \code{\link[base]{function}}: 
#'  \code{col}, a function that converts \code{x}-like values into 
#'  colors; 
#'  \code{pch}, a function that converts \code{x}-like values into 
#'  symbols; 
#'  \code{legend}, a function to draw a legend on a plot with the 
#'  correct colors and symbols. That function accepts \code{...} 
#'  arguments, passed to the original \code{\link[graphics]{legend}} 
#'  function.
#'
#'
#'@example inst/examples/setFactorGraphics.R
#'
#'@rdname setFactorGraphics-methods
#'
#'@export 
#'
setFactorGraphics <- function(
 x, 
 ...
){  
    UseMethod( "setFactorGraphics" )
}   



#'@rdname setFactorGraphics-methods
#'
#'@method setFactorGraphics default
#'
#'@export 
#'
setFactorGraphics.default <- function(
 x, 
 col=FALSE, 
 pch=FALSE, 
 fill=FALSE, 
 leg=NULL, 
 ord=TRUE, 
 #colList=NULL, 
 #pchList=NULL, 
 #fillList=NULL, 
 naCol="black", 
 naPch=25, 
 naFill="black", 
 naLeg="na", 
 ...
){  
    if( !is.logical( col ) ){ 
        colList <- col
        col     <- TRUE 
    }else{ 
        colList <- NULL 
    }   
    
    if( !is.logical( fill ) ){ 
        fillList <- fill
        fill     <- TRUE 
    }else{ 
        fillList <- NULL 
    }   
    
    if( !is.logical( pch ) ){ 
        pchList <- pch
        pch     <- TRUE 
    }else{ 
        pchList <- NULL 
    }   
    
    if( !any( c( col, pch, fill ) ) ){ 
        stop( "At least one legend item (col, pch, fill) should be TRUE" ) 
    }   
    
    
    #   Prepare the conversion table:
    convert <- data.frame( 
        "values"    = unique(x), 
        #"col"       = NA_character_, 
        #"pch"       = as.integer( NA_real_ ), 
        #"lty"       = as.integer( NA_real_ ), 
        #"legends"   = NA_character_, 
        stringsAsFactors = FALSE 
    )   
    
    
    if( ord ){ 
        convert <- convert[ order( convert[, "values" ] ), , drop = FALSE ] 
    }   
    
    
    #   Are there NA in the data
    isNA  <- is.na( convert[, "values" ] )
    hasNA <- any( isNA ) 
    
    
    #   Output list
    out <- list( 
        "legend" = function( x, y = NULL, legend, col = NULL, 
            pch = NULL, fill = NULL, ... # lty = NULL,
        ){  
            #   Prepare the arguments
            arguments <- list( "x" = x, "y" = y, "legend" = legend ) 
            
            if( !is.null( col ) ){ 
                arguments <- c( arguments, list( "col" = col ) ) 
            }   
            
            if( !is.null( fill ) ){ 
                arguments <- c( arguments, list( "fill" = fill ) ) 
            }   
            
            if( !is.null( pch ) ){ 
                arguments <- c( arguments, list( "pch" = pch ) ) 
            }   
            
            # if( !is.null( lty ) ){ 
                # arguments <- c( arguments, list( "lty" = lty ) ) 
            # }   
            
            arguments <- c( arguments, list(...) )
            
            do.call( 
                what = get( "legend", pos = "package:graphics" ), 
                args = arguments ) 
            
            return( invisible( arguments) ) 
        }   
    )    
    
    environment( out[[ "legend" ]] ) <- new.env() 
    
    class( out ) <- "factorGraphics"
    
    
    #   Color legend ============================================
    if( col ){ 
        if( is.null(colList) ){ 
            # library( "colorspace" )
            
            # convert[ !isNA, "col" ] <- rainbow( 
                # n = nrow( convert ) - as.integer( hasNA ), 
                # s = 0.60, v = 0.80, end = 4/6 ) 
            
            convert[ !isNA, "col" ] <- hcl( 
                h = seq( from = 15, to = 360+15, length.out = (nrow( convert ) - as.integer( hasNA ))+1 ), 
                c = 100, 
                l = 50 )[ -((nrow( convert ) - as.integer( hasNA ))+1) ] 
            
            
            convert[ isNA, "col" ] <- naCol
            
        }else{ 
            testColList <- length( colList ) == (nrow( convert ) - sum( isNA )) 
            
            if( !testColList ){ 
                stop( "'col' should be the same length as 'unique(x)', NA excluded (or a logical value)" )
            };  rm(testColList)
            
            # if( !is.data.frame( colList ) ){ 
                # stop( "'colList' must be a data.frame" ) 
            # }   
            
            # vc <- c( "values", "col" )
            # testColCol <- vc %in% colnames(colList) 
            
            # if( !all(testColCol) ){ 
                # stop( sprintf( "Some columns are missing in 'colList': %s", 
                    # paste( vc[ !testColCol ], collapse = "; " ) ) )
            # };  rm( testColCol )
            
            # if( nrow( convert ) != nrow( colList ) ){ 
                # stop( "'length(unique(x))' and 'nrow(colList)' must be identical (incl. NA values)." ) 
            # }   
            
            if( is.factor( colList ) ){ 
                colList <- as.character( colList ) 
            }   
            
            convert[ !isNA, "col" ] <- colList
            
            # convert <- merge( 
                # x     = convert, 
                # y     = colList, 
                # by    = "values", 
                # all.x = TRUE, 
                # sort  = FALSE 
            # );  rm( vc )
            
            convert[ isNA, "col" ] <- naCol
            
            testNa <- is.na( convert[, "col" ] ) & (!is.na( convert[, "values" ] ))
            
            if( any( testNa  ) ){ 
                stop( "Some non-NA values in 'x' don't have a color in 'col'" ) 
            };  rm( testNa )
        }   
        
        
        out[[ "col" ]] <- function(x){ 
            #   Cast x into a data.frame
            x <- data.frame( "values" = x, "id" = 1:length(x), 
                stringsAsFactors = FALSE ) 
            
            #   Merge with the colors
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colors" ) ]
                by    = "values", 
                all.x = TRUE, 
                sort  = FALSE 
            )   
            
            x <- x[ order( x[, "id" ] ), ] 
            
            if( any( is.na( x[, "col" ] ) ) ){ 
                warning( "Some values in 'x' are new. Color set to NA" ) 
            }   
            
            return( as.character( x[, "col" ] ) ) 
        }   
        
        #   Give the function a clean environment
        environment( out[[ "col" ]] ) <- new.env() 
        
        #   Populate the environment
        assign( "convert", convert[,c( "values", "col" ) ], 
            envir = environment( out[[ "col" ]] ) ) 
            
        #   Legend function
        formals( out[[ "legend" ]] )[[ "col" ]] <- convert[, "col" ] 
    }   
    
    
    #   Fill legend ============================================
    if( fill ){ 
        if( is.null(fillList) ){ 
            
            # convert[ !isNA, "fill" ] <- rainbow( 
                # n = nrow( convert ) - as.integer( hasNA ), 
                # s = 0.60, v = 0.80, end = 4/6 ) 
            
            convert[ !isNA, "fill" ] <- hcl( 
                h = seq( from = 0, to = 360, length.out = (nrow( convert ) - as.integer( hasNA ))+1 ), 
                c = 100, 
                l = 65 )[ -((nrow( convert ) - as.integer( hasNA ))+1) ] 
            
            convert[ isNA, "fill" ] <- naFill
            
        }else{ 
            testFillList <- length( fillList ) == (nrow( convert ) - sum( isNA )) 
            
            if( !testFillList ){ 
                stop( "'fill' should be the same length as 'unique(x)', NA excluded (or a logical value)" )
            };  rm(testFillList)
            
            # if( !is.data.frame( fillList ) ){ 
                # stop( "'fillList' must be a data.frame" ) 
            # }   
            
            # vc <- c( "values", "fill" )
            # testColCol <- vc %in% colnames(fillList) 
            
            # if( !all(testColCol) ){ 
                # stop( sprintf( "Some columns are missing in 'fillList': %s", 
                    # paste( vc[ !testColCol ], collapse = "; " ) ) )
            # };  rm( testColCol )
            
            # if( nrow( convert ) != nrow( fillList ) ){ 
                # stop( "'length(unique(x))' and 'nrow(fillList)' must be identical (incl. NA values)." ) 
            # }   
            
            if( is.factor( fillList ) ){ 
                fillList <- as.character( fillList ) 
            }   
            
            convert[ !isNA, "fill" ] <- fillList
            
            # convert <- merge( 
                # x     = convert, 
                # y     = fillList, 
                # by    = "values", 
                # all.x = TRUE, 
                # sort  = FALSE 
            # );  rm( vc )
            
            convert[ isNA, "fill" ] <- naFill 
            
            testNa <- is.na( convert[, "fill" ] ) & (!is.na( convert[, "values" ] ))
            
            if( any( testNa  ) ){ 
                stop( "Some non-NA values in 'x' don't have a color in 'fill'" ) 
            };  rm( testNa )
        }   
        
        
        out[[ "fill" ]] <- function(x){ 
            #   Cast x into a data.frame
            x <- data.frame( "values" = x, "id" = 1:length(x), 
                stringsAsFactors = FALSE ) 
            
            #   Merge with the colors
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colors" ) ]
                by    = "values", 
                all.x = TRUE, 
                sort  = FALSE 
            )   
            
            x <- x[ order( x[, "id" ] ), ] 
            
            if( any( is.na( x[, "fill" ] ) ) ){ 
                warning( "Some values in 'x' are new. Color set to NA" ) 
            }   
            
            return( as.character( x[, "fill" ] ) ) 
        }   
        
        #   Give the function a clean environment
        environment( out[[ "fill" ]] ) <- new.env() 
        
        #   Populate the environment
        assign( "convert", convert[,c( "values", "fill" ) ], 
            envir = environment( out[[ "fill" ]] ) ) 
            
        #   Legend function
        formals( out[[ "legend" ]] )[[ "fill" ]] <- convert[, "fill" ] 
    }   
    
    
    #   Symbol legend ===========================================
    if( pch ){ 
        if( is.null(pchList) ){ 
            convert[ !isNA, "pch" ] <- .generatePch( 
                n = nrow(convert) - as.integer( hasNA ), exclude = naPch ) 
            
            if( any( isNA ) ){ 
                convert[ isNA, "pch" ] <- naPch
            }   
            
        }else{ 
            testPchList <- length( pchList ) == (nrow( convert ) - sum( isNA ))
            
            if( !testPchList ){ 
                stop( "'pch' should be the same length as 'unique(x)', NA excluded (or a logical value)" )
            };  rm(testPchList)
            
            # if( !is.data.frame( pchList ) ){ 
                # stop( "'pchList' must be a data.frame" ) 
            # }   
            
            # vp <- c( "values", "pch" )
            # testPchCol <- vp %in% colnames(pchList) 
            
            # if( !all(testPchCol) ){ 
                # stop( sprintf( "Some columns are missing in 'pchList': %s", 
                    # paste( vp[ !testPchCol ], collapse = "; " ) ) )
            # };  rm( testPchCol )
            
            # if( nrow( convert ) != nrow( pchList ) ){ 
                # stop( "'length(unique(x))' and 'nrow(pchList)' must be identical (incl. NA values)." ) 
            # }   
            
            if( is.factor( pchList ) ){ 
                warning( "pchList is factor-class. Ambiguous format! Forced to character." )
                
                pchList <- as.character( pchList ) 
            }   
            
            convert[ !isNA, "pch" ] <- pchList
            
            # convert <- merge( 
                # x     = convert, 
                # y     = pchList, 
                # by    = "values", 
                # all.x = TRUE, 
                # sort  = FALSE 
            # );  rm( vp ) 
            
            convert[ isNA, "pch" ] <- naPch
            
            testNa <- is.na( convert[, "pch" ] ) & (!is.na( convert[, "values" ] ))
            
            if( any( testNa  ) ){ 
                stop( "Some non-NA values in 'x' don't have a symbol in 'pch'" ) 
            };  rm( testNa ) 
        }   
        
        
        out[[ "pch" ]] <- function(x){ 
            #   Cast x into a data.frame
            x <- data.frame( "values" = x, "id" = 1:length(x), 
                stringsAsFactors = FALSE ) 
            
            #   Merge with the colors
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colors" ) ]
                by    = "values", 
                all.x = TRUE, 
                sort  = FALSE 
            )   
            
            x <- x[ order( x[, "id" ] ), ] 
            
            if( any( is.na( x[, "pch" ] ) ) ){ 
                warning( "Some values in 'x' are new. Symbol set to NA" ) 
            }   
            
            # suppressWarnings( try( x2 <- as.integer( x ) ) ) 
            
            return( x[, "pch" ] ) 
        }   
        
        #   Give the function a clean environment
        environment( out[[ "pch" ]] ) <- new.env() 
        
        #   Populate the environment
        assign( "convert", convert[,c( "values", "pch" ) ], 
            envir = environment( out[[ "pch" ]] ) ) 
            
        #   Legend function
        formals( out[[ "legend" ]] )[[ "pch" ]] <- convert[, "pch" ] 
    }   
    
    
    if( col | fill ){ 
        # isNotNA <- !is.na( convert[, "values" ] )
        
        # formals( vg[[ "legend" ]] )[[ "col" ]][ isNA ] <- 
            # NA_character_ 
        
        out[[ "iCol" ]]    <- convert[ !isNA, "col" ] 
        out[[ "iFill" ]]   <- convert[ !isNA, "fill" ] 
        
        if( is.character( convert[, "values" ] ) ){ 
            out[[ "iBreaks" ]] <- numeric(0) 
        }else{ 
            if( is.factor( convert[, "values" ] ) ){ 
                out[[ "iBreaks" ]] <- as.numeric( convert[ !isNA, "values" ] ) 
            }else{ 
                out[[ "iBreaks" ]] <- convert[ !isNA, "values" ] 
            }   
            
            db2 <- diff( out[[ "iBreaks" ]] ) / 2
            l   <- length( out[[ "iBreaks" ]] ) 
            
            if( l == 1 ){ 
                out[[ "iBreaks" ]] <- out[[ "iBreaks" ]] + c( -1, 1 ) 
            }else if( l == 2 ){ 
                out[[ "iBreaks" ]] <- c( 
                    out[[ "iBreaks" ]][ 1 ] - db2, 
                    out[[ "iBreaks" ]][ 1 ] + db2, 
                    out[[ "iBreaks" ]][ 2 ] + db2
                )   
            }else{ 
                out[[ "iBreaks" ]] <- c( 
                    out[[ "iBreaks" ]][ 1 ] - db2[1], 
                    out[[ "iBreaks" ]][ 1:(l-1) ] + db2, 
                    out[[ "iBreaks" ]][ l ] + db2[ length( db2 ) ]
                )   
            }   
        }   
    }   
    
    
    if( !is.null( leg ) ){ 
        testLeg <- length( leg ) == (nrow( convert ) - sum( isNA ))
        
        if( !testLeg ){ 
            stop( "'leg' should be the same length as 'unique(x)', NA excluded (or NULL)" )
        };  rm( testLeg )
        
        # if( !is.data.frame( leg ) ){ 
            # stop( "'leg' must be a data.frame" ) 
        # }   
        
        # vl <- c( "values", "legends" )
        # testLegCol <- vl %in% colnames(leg) 
        
        # if( !all(testLegCol) ){ 
            # stop( sprintf( "Some columns are missing in 'leg': %s", 
                # paste( vl[ !testLegCol ], collapse = "; " ) ) )
        # };  rm( testLegCol )
        
        # if( is.factor( leg[, "legends" ] ) ){ 
            # leg[, "legends" ] <- as.character( leg[, "legends" ] ) 
        # }   
        
        convert[ !isNA, "legends" ] <- leg 
        
        #   Convert factors to character
        if( is.factor( convert[, "legends" ] ) ){ 
            convert[, "legends" ] <- as.character( convert[, "legends" ] )
        }   
        
        # convert <- merge( 
            # x     = convert, 
            # y     = leg, 
            # by    = "values", 
            # all.x = TRUE, 
            # sort  = FALSE 
        # );  rm( vl )
        
        convert[ isNA, "legends" ] <- naLeg 
        
        testNa <- is.na( convert[, "legends" ] ) & (!is.na( convert[, "values" ] ))
        
        if( any( testNa  ) ){ 
            stop( "Some non-NA values in 'x' don't have a legend in 'leg'" ) 
        };  rm( testNa )
        
    }else{ 
        convert[, "legends" ] <- as.character( convert[, "values" ] ) 
        convert[ isNA, "legends" ] <- naLeg  
    }   
    
    
    #   Add the legend function
    formals( out[[ "legend" ]] )[[ "legend" ]] <- convert[, "legends" ] 
    
    
    #   Return output (list of functions)
    return( out )
}   



#'@rdname setFactorGraphics-methods
#'
#'@method setFactorGraphics matrix
#'
#'@export 
setFactorGraphics.matrix <- function(
 x, 
 ...
){  #   x dimentions
    dx <- dim( x )
    
    #   Matrix -> vector
    x <- as.vector( x ) 
    
    #   
    vg <- setFactorGraphics(
        x = x, 
        ...
    )   
    
    
    #   New color function
    if( "col" %in% names(vg) ){ 
        colOriginal <- vg[[ "col" ]] 
        
        vg[[ "col" ]] <- function(x){ 
            return( matrix( 
                data  = colOriginal( as.vector( x ) ), 
                nrow  = dx[1], 
                ncol  = dx[2], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( vg[[ "col" ]] ) <-  new.env() 
        assign( pos = environment( vg[[ "col" ]] ), x = "colOriginal", value = colOriginal,  ) 
        assign( pos = environment( vg[[ "col" ]] ), x = "dx",          value = dx ) 
        
        convert <- as.list( environment( colOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "values" ] ), ] 
        
        isNotNA <- !is.na( convert[, "values" ] )
        
        formals( vg[[ "legend" ]] )[[ "col" ]][ !isNotNA ] <- 
            NA_character_ 
    }   
    
    
    #   New fill function
    if( "fill" %in% names(vg) ){ 
        fillOriginal <- vg[[ "fill" ]] 
        
        vg[[ "fill" ]] <- function(x){ 
            return( matrix( 
                data  = fillOriginal( as.vector( x ) ), 
                nrow  = dx[1], 
                ncol  = dx[2], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( vg[[ "fill" ]] ) <-  new.env() 
        assign( pos = environment( vg[[ "fill" ]] ), x = "fillOriginal", value = fillOriginal,  ) 
        assign( pos = environment( vg[[ "fill" ]] ), x = "dx",           value = dx ) 
        
        convert <- as.list( environment( fillOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "values" ] ), ] 
        
        isNotNA <- !is.na( convert[, "values" ] )
        
        formals( vg[[ "legend" ]] )[[ "fill" ]][ !isNotNA ] <- 
            NA_character_ 
    }   
    
    
    #   New pch function
    if( "pch" %in% names(vg) ){ 
        pchOriginal <- vg[[ "pch" ]] 
        
        vg[[ "pch" ]] <- function(x){ 
            return( matrix( 
                data  = pchOriginal( as.vector( x ) ), 
                nrow  = dx[1], 
                ncol  = dx[2], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( vg[[ "pch" ]] ) <-  new.env() 
        assign( pos = environment( vg[[ "pch" ]] ), x = "pchOriginal", value = colOriginal,  ) 
        assign( pos = environment( vg[[ "pch" ]] ), x = "dx",          value = dx ) 
    }   
    
    return( vg )
}   



# asCol =========================================================

#' Converts categorical values to colors. Wrapper for setFactorGraphics.
#'
#' Converts categorical values to colors. Wrapper for setFactorGraphics.
#'
#'
#'@param x
#'  A vector of numerical, character, boolean or factor values, 
#'  representing categories (non-continuous, with a limited number 
#'  of unique values).
#'
#'@param export
#'  Single logical value. If \code{TRUE}, the result of 
#'  \code{\link[easylegend]{setFactorGraphics}} will be saved into 
#'  the output attribute \code{fg}.
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[easylegend]{setFactorGraphics}}.
#'
#'
#'@return 
#'  Returns a list of colors, with \code{\link[base]{attr}}ibutes 
#'  \code{fg} set to output of \code{\link[easylegend]{setFactorGraphics}}.
#'
#'
#'@example inst/examples/asCol.R
#'
#'@export 
#'
asCol <- function(
 x, 
 export = FALSE, 
 ...
){  
    fg <- setFactorGraphics( x = x, col = TRUE, ... ) 
    
    out <- fg$col( x ) 
    
    if( export ){ 
        attr( out, "factorGraphics" ) <- fg 
    }   
    
    return( out ) 
}   



# asPch =========================================================

#' Converts categorical values to symbols. Wrapper for setFactorGraphics.
#'
#' Converts categorical values to symbols. Wrapper for setFactorGraphics.
#'
#'
#'@param x
#'  A vector of numerical, character, boolean or factor values, 
#'  representing categories (non-continuous, with a limited number 
#'  of unique values).
#'
#'@param export
#'  Single logical value. If \code{TRUE}, the result of 
#'  \code{\link[easylegend]{setFactorGraphics}} will be saved into 
#'  the output attribute \code{fg}.
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[easylegend]{setFactorGraphics}}.
#'
#'
#'@return 
#'  Returns a list of symbols.
#'
#'
#'@example inst/examples/asPch.R
#'
#'@export 
#'
asPch <- function(
 x, 
 export = FALSE, 
 ...
){  
    fg <- setFactorGraphics( x = x, pch = TRUE, ... ) 
    
    out <- fg$pch( x ) 
    
    if( export ){ 
        attr( out, "factorGraphics" ) <- fg 
    }   
    
    return( out ) 
}   



#'@rdname setFactorGraphics-methods
#'
#'@method setFactorGraphics RasterLayer
#'
#'@export 
#'
setFactorGraphics.RasterLayer <- function( 
 x, 
 ...
){  
    if( !"raster" %in% rownames( installed.packages() ) ){ 
        stop( "setFactorGraphics.RasterLayer requires the package 'raster' to be installed" ) 
    }   
    
    
    #   Load the package raster:
    pkg <- "raster"
    if( !require( pkg, character.only = TRUE ) ){ 
        stop( "The package raster can not be loaded" )
    }   
    
    
    # hasNA <- any( is.na( values( x ) ) )
    u <- get( "unique", "package:raster" )
    x <- u( x )
    
    isna  <- get( "is.na", "package:raster" )
    hasNa <- any( u( isna( x ) ) ) 
    
    if( any( u( isna( x ) ) ) ){ 
        x <- c( x, NA )  
    }   
    
    cs <- setFactorGraphics(
        x = x, 
        ...
    )   
    
    
    #   New color function
    if( "col" %in% names(cs) ){ 
        cs[[ "col" ]] <- function(x){ 
            stop( "Not implemented. Technically can't return a raster of colors (character strings)" )
        }   
    }   
    
    
    #   New fill function
    if( "fill" %in% names(cs) ){ 
        cs[[ "fill" ]] <- function(x){ 
            stop( "Not implemented. Technically can't return a raster of fill-colors (character strings)" )
        }   
    }   
    
    
    return( cs )
}   



# setColorScale ===================================================

.col2hsv <- function(col){ 
    return( as.data.frame( t( rgb2hsv( r = col2rgb( col ) ) ) ) )
}   



.addColorGradientLegend <- function( 
 l,      # output from legend, called a 1st time 
 fill,   # fill colors
 groups, # fill groups
 #legend, # labels for each break-point 
 border = "black" 
){  
    n       <- length( l$text$x ) 
    uGroups <- unique( groups ) 
    
    # browser() 
    
    if( length( uGroups ) != n ){ 
        stop( sprintf( 
            "Number of fill-groups %s and number of original legend-labels %s differ (internal error)", 
            length( uGroups ), n 
        ) ) 
    }   
    
    
    if( length( groups ) != length( fill ) ){ 
        stop( "Length of 'fill' and 'groups' differ (internal error)" ) 
    }   
    
    
    # dx <- max( abs( diff(l$text$x) ) )
    dy <- max( abs( diff(l$text$y) ) ) 
    
    fillWidth <- min(l$text$x) - l$rect$left 
    
    yy <- rep( NA_real_, n+1 )
    
    for( i in 1:n ){
        if( i == 1 ){ 
            ytop <- l$text$y[ i ] + dy/2 # + dy/4
        }else{ 
            ytop <- l$text$y[ i ] + dy/2
        }   
        
        if( i == n ){ 
            ybottom <- l$text$y[i] - dy/2 # - dy/4
        }else{ 
            ybottom <- l$text$y[i] - dy/2
        }   
        
        dy2 <- ytop - ybottom
        
        sel      <- groups == uGroups[ i ]
        nSubFill <- sum( sel )
        subFill  <- fill[ sel ] 
        
        for( j in 1:nSubFill ){ 
            rect(
                xleft   = l$rect$left + fillWidth*1/3, 
                ybottom = ytop - dy2 * j/nSubFill, 
                xright  = l$rect$left + fillWidth*2/3, 
                ytop    = ytop - dy2 * (j-1)/nSubFill, 
                border  = NA, 
                col     = subFill[ j ] ) 
        }   

        
        # rect(
            # xleft   = l$rect$left + fillWidth*1/3, 
            # ybottom = ybottom, 
            # xright  = l$rect$left + fillWidth*2/3, 
            # ytop    = ytop, 
            # border  = border ) 
        
        # segments(
            # x0  = l$rect$left + fillWidth*2/3, 
            # y0  = l$text$y[ i ], 
            # x1  = l$rect$left + fillWidth*3/4, 
            # y1  = l$text$y[ i ], 
            # col = border 
        # )   
        
        
        #   Segment for the boundary thick mark
        segments(
            x0  = l$rect$left + fillWidth*2/3, 
            y0  = ytop, 
            x1  = l$rect$left + fillWidth*3/4, 
            y1  = ytop, 
            col = border 
        )   
        
        
        #   Save y values
        yy[ i ] <- ytop 
        
        if( i == n ){ 
            yy[ i+1 ] <- ybottom 
            
            segments(
                x0  = l$rect$left + fillWidth*2/3, 
                y0  = ybottom, 
                x1  = l$rect$left + fillWidth*3/4, 
                y1  = ybottom, 
                col = border 
            )   
        }   
    }   
    
    #   Big rectangle
    rect(
        xleft   = l$rect$left + fillWidth*1/3, 
        ybottom = min(l$text$y) - dy/2, # - dy/4
        xright  = l$rect$left + fillWidth*2/3, 
        ytop    = max(l$text$y) + dy/2, # + dy/4
        border  = border ) 
    
    
    return( yy ) 
}   

    # plot( 1, 1 ) 
    # n    <- 4
    # l    <- legend("topright",fill="transparent",legend=1:n,border="transparent")
    # fill <- hsv( h = seq( 0, 1, length.out = (n*2)+1 )[ -((n*2)+1) ] ) 
    # .addColorGradientLegend( l = l, fill = fill, groups = rep( 1:n, each = 2 ) )



#' Prepare color scale and legend (col, fill) from continuous-numeric data.
#'
#' Prepare color scale and legend (col, fill) from continuous-numeric data.
#'
#'
#'@param x
#'  A vector of numerical values (continous).
#'
#'@param col
#'  Logical value or vector of character strings representing colors. 
#'  If \code{TRUE} or vector of colors, a continuous color scale 
#'  is defined for 'x'. \code{col} and \code{fill} can't be set 
#'  simultaneously (the legend would not work). \code{int} must be 
#'  1 if \code{col} is set.
#'
#'@param fill
#'  Logical value or vector of character strings representing fill-colors. 
#'  If \code{TRUE} or vector of colors, a continuous fill-color scale 
#'  is defined for 'x'. \code{col} and \code{fill} can't be set 
#'  simultaneously (the legend would not work).
#'
#'@param breaks
#'  See \code{\link[base]{cut}}.
#'
#'@param int
#'  Single integer value. Number of color intervals to be defined 
#'  in between the main fill-colors. \code{int} must be 1 if 
#'  \code{col} is set. Intermediate colors are generated with 
#'  \code{\link[grDevices]{colorRampPalette}}.
#'
#'@param brackets
#'  Vector of 3 characters. Used to generate customise legend-labels, 
#'  if \code{labels} is \code{NULL}. 
#'
#'@param labels
#'  See \code{\link[base]{cut}}.
#'
#'@param right
#'  See \code{\link[base]{cut}}.
#'
#'@param include.lowest
#'  See \code{\link[base]{cut}}.
#'
#'@param digits
#'  See \code{\link[base]{format}}.
#'
#'@param nsmall
#'  See \code{\link[base]{format}}.
#'
#'@param naCol
#'  Single character string. Color for \code{NA} (missing) values.
#'
#'@param naLeg
#'  Single character string. Legend (label) for \code{NA} (missing) 
#'  values.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a list of 2 \code{\link[base]{function}}: 
#'  \code{col} or \code{fill}, a function that converts 
#'  \code{x}-like values into (fill)colors; 
#'  \code{legend}, a function to draw a legend on a plot with the 
#'  correct (fill)colors and legend. That function accepts \code{...} 
#'  arguments, passed to the original \code{\link[graphics]{legend}} 
#'  function.
#'
#'
#'@example inst/examples/setColorScale.R
#'
#'@importFrom grDevices colorRampPalette
#'
#'@rdname setColorScale-methods
#'
#'@export 
#'
setColorScale <- function(
 x, 
 ...
){  
    UseMethod( "setColorScale" )
}   


#'@rdname setColorScale-methods
#'
#'@method setColorScale default
#'
#'@export 
#'
setColorScale.default <- function( 
    x, 
    col      = FALSE, 
    fill     = FALSE, 
    breaks   = NULL, 
    int      = 1L, 
    naCol    = "lightgray", 
    naLeg    = "na", 
    brackets = c( "", " ", "" ), 
    labels   = NULL, 
    right    = FALSE,       # see cut()
    include.lowest = TRUE,  # see cut()
    digits   = 3,           # see format()
    nsmall   = 3,           # see format()
    ...      # passed to format
){  
    sCol  <- ifelse( all( is.logical( col ) ), col[1], TRUE ) 
    sFill <- ifelse( all( is.logical( fill ) ), fill[1], TRUE ) 
    
    if( !any( c( sCol, sFill ) ) ){ 
        stop( "Either 'col' or 'fill' must be set (to TRUE or a vector of colors)" ) 
        
    }else if( sCol & sFill ){ 
        stop( "Specify either 'col', or 'fill', but not both at the same time" ) 
        
    }
    
    if( sCol & all( is.logical( col ) ) ){ 
        # col  <- gray( c( .75, .50, .25, 0 ) ) 
        col  <- hsv( 
            h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
            s = seq( .20,  .8, length.out = 5 ), 
            v = seq( .95,  .3, length.out = 5 ) )
            #        light dark
    }
    
    if( sFill & all( is.logical( fill ) ) ){ 
        # fill <- gray( c( .75, .50, .25, 0 ) ) 
        fill <- hsv( 
            h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
            s = seq( .20,  .8, length.out = 5 ), 
            v = seq( .95,  .3, length.out = 5 ) )
            #        light dark
        
    }   
    
    #   Check and sanitise int
    int <- as.integer( round( int, 0 ) ) 
    int <- ifelse( int[1] < 1L, 1L, int[1] )
    
    if( sCol & (int > 1) ){ 
        stop( "If 'col' is specified, then 'int' must be 1 (legend won't work otherwise). Use 'fill' as a workaround" ) 
        
    }   
    
    if( (int%%2 == 1) & (int != 1) ){ 
        stop( "'int' must be an odd integer or 1" )
    }   
    
    
    #   Check sCol and sFill
    if( sCol & (length( col ) < 2) ){ 
        stop( "At least 2 colors ('col') should be provided" )
    }   
    if( sFill & (length( fill ) < 2) ){ 
        stop( "At least 2 colors ('fill') should be provided" )
    }   
    
    
    n <- ifelse( sCol, length(col), length(fill) ) 
    
    
    #   Has x NA values?
    hasNA <- any( is.na( x ) ) 
    
    
    if( is.null( breaks ) ){ 
        breaks <- seq( 
            from = min( x, na.rm = hasNA ), 
            to   = max( x, na.rm = hasNA ), 
            length.out = n + 1 )
    }else{ 
        test <- any( sort( breaks, na.last = TRUE ) != breaks ) | 
                (length( unique( breaks ) ) != length( breaks )) 
        
        if( test ){ 
            stop( "'breaks' should be ordered, with no replica" ) 
        };  rm( test )
    }   
    
    
    #   Output list
    out <- list( 
        "legend" = function( x, y = NULL, legend, col = NULL, 
            fill = NULL, border = "black", groups = NULL, cex = 1, 
            text.col = par( "col" ), text.font = NULL, 
            title.col = par( "col" ), horiz = FALSE, 
            title = NULL, ... 
        ){  
            if( !exists( "style" ) ){ style <- 1 }
            
            if( horiz ){ stop( "'horiz' = TRUE not supported in easylegend" ) }
            
            #   Prepare the arguments
            arguments <- list( "x" = x, "y" = y, title.col = title.col, 
                title = title ) 
            
            if( !is.null(col) ){ 
                arguments <- c( arguments, list( "col" = col, 
                    "legend" = legend ) ) 
                
            }else if( style != 1 ){ # !is.null( fill )
                transpLeg <- legend[ order( nchar( legend ) ) ] 
                transpLeg <- transpLeg[ -1 ]
                
                arguments <- c( arguments, list( 
                    "fill"   = "transparent", 
                    border   = "transparent", 
                    legend   = transpLeg, 
                    text.col = "transparent" ) ) 
                
            }else{ 
                arguments <- c( arguments, list( "fill" = fill, 
                    "legend" = legend ) ) 
            }   
            
            arguments <- c( arguments, list(...) )
            
            lRes <- do.call( 
                what = get( "legend", pos = "package:graphics" ), 
                args = arguments ) 
            
            if( style != 1 ){ # (!is.null( fill )) & (!is.null( groups ))
                yy <- .addColorGradientLegend( 
                    l      = lRes, 
                    fill   = fill, 
                    groups = groups, 
                    border = border ) 
                
                # if( missing( "cex"       ) ){ cex       <- 1            } 
                # if( missing( "text.col"  ) ){ text.col  <- par( "col" ) } 
                # if( missing( "text.font" ) ){ text.font <- NULL         } 
                
                text( x = lRes$text$x[1], y = yy, 
                    labels = legend, # cex = cex*par("cex"), 
                    col = text.col, pos = 4, offset = 0 ) # vfont = text.font, 
                
            }   
            
            return( invisible( arguments) ) 
        }   
    )    
    
    environment( out[[ "legend" ]] ) <- new.env() 
    
    class( out ) <- "numericGraphics"
    
    
    #   convert between class breaks and colors
    mid  <- breaks[ 1:(length(breaks)-1) ] + diff( breaks )/2 
    from <- breaks[ 1:(length(breaks)-1) ]
    to   <- breaks[ 2:length(breaks) ] 
    mid[ is.infinite( mid ) ] <- to[ is.infinite( mid ) ] 
    mid[ is.infinite( mid ) ] <- from[ is.infinite( mid ) ] 
    
    convert <- data.frame( 
        "from"   = from, 
        "to"     = to, 
        "mid"    = mid, 
        "col"    = if( sCol ){ col }else{ fill }, 
        "labels" = NA_character_, 
        stringsAsFactors = FALSE 
    )   
    rm( breaks, col, from, to, mid )  
    
    
    #   Prepare the color-class labels
    if( is.null( labels ) ){ 
        convert[, "labels" ] <- paste0( brackets[1], 
            format( convert[, "from" ], digits = digits, nsmall = nsmall, ... ), brackets[2], 
            format( convert[, "to" ], digits = digits, nsmall = nsmall, ... ), brackets[3] ) 
    }else{ 
        convert[, "labels" ] <- labels 
        rm( labels )
    }   
    
    
    nConvert <- nrow( convert ) 
    
    if( int == 1L ){ 
        convert2                   <- convert 
        convert2[, "groups" ]      <- 1:nConvert 
        #convert2[, "groupLabels" ] <- convert2[, "labels" ] 
        
    }else{ 
        nConvert2 <- nConvert*int
        
        convert2 <- data.frame( 
            "from"   = rep( NA_real_, nConvert2 ), 
            "to"     = rep( NA_real_, nConvert2 ), 
            "mid"    = rep( NA_real_, nConvert2 ), 
            "col"    = rep( NA_character_, nConvert2 ), 
            "labels" = rep( NA_character_, nConvert2 ), 
            #"groups"     = as.integer( rep( NA_real_, nConvert*int ) ), 
            "groups"      = as.integer( rep( 1:nConvert, each = int ) ), 
            #"groupLabels" = rep( NA_character_, nConvert*int ), 
            stringsAsFactors = FALSE 
        )   
        
        
        #   Interpolate colors, adding mid-point colors
        cr  <- colorRampPalette( 
            colors = convert[, "col" ], 
            bias   = 1, 
            space  = "Lab" ) 
        cr  <- cr( 2 * nConvert - 1 ) 
        
        
        #   Interpolate colors, adding intermediate colors
        cr  <- colorRampPalette( 
            colors = c( 
                convert[ 1, "col" ], 
                cr, 
                convert[ nConvert, "col" ] ), 
            bias   = 1, 
            space  = "Lab" ) 
        convert2[, "col" ]  <- cr( nConvert * int ) 
        
        
        convert2 <- do.call( what = "rbind", args = lapply( 
            X   = split( x = convert2, f = convert2[, "groups" ] ), 
            FUN = function(X){ 
                i <- X[ 1, "groups" ] 
                n <- nrow( X )
                
                fromTo2 <- fromTo <- unlist( convert[ i, c( "from", "to" ) ] ) 
                
                #   Neutralise infinite values
                isInf <- is.infinite( fromTo ) 
                
                if( isInf[ 1 ] ){ 
                    fromTo[ 1 ] <- fromTo[ 2 ] - 1
                    
                }
                
                if( isInf[ 2 ] ){ 
                    fromTo[ 2 ] <- fromTo[ 1 ] + 1
                    
                }   
                
                #   Set values sequence
                X[, "from" ] <- seq( 
                    from       = fromTo[1], 
                    to         = fromTo[2], 
                    length.out = n+1 )[ -(n+1) ]   
                
                X[, "to" ] <- c( X[ -1, "from" ], fromTo[2] ) 
                
                #   Re-attribute infinte values
                if( isInf[ 1 ] ){ X[ 1, "from" ] <- fromTo2[ 1 ] } 
                if( isInf[ 2 ] ){ X[ n, "to"   ] <- fromTo2[ 2 ] } 
                
                X[, "mid" ] <- rowMeans( x = X[, c( "from", "to" ) ] ) 
                
                return( X ) 
            }   
        ) ) 
        
        convert2 <- unique( convert2 ) 
        
        convert2[, "labels" ] <- paste0( brackets[1], 
                format( convert2[, "from" ] ), brackets[2], 
                format( convert2[, "to" ] ), brackets[3] ) 
        
        #   Make a new legend for each break point
        attr( convert2, "legend" ) <- c( convert[, "from" ], 
            convert[ nConvert, "to" ] ) 
        
        attr( convert2, "legend" ) <- format( attr( convert2, "legend" ), 
            digits = digits, nsmall = nsmall, ... ) 
        
        # convert2[, "groupLabels" ] <- paste0( brackets[1], 
                # format( min( convert2[, "from" ] ), digits = digits, nsmall = nsmall, ... ), brackets[2], 
                # format( max( convert2[, "to" ] ), digits = digits, nsmall = nsmall, ... ), brackets[3] ) 
        
    }   # end int != 1L
    
    
    if( hasNA ){ 
        convert  <- rbind( convert,  NA ) 
        
        nConvert <- nrow( convert ) 
        convert[ nConvert, "col" ]    <- naCol 
        convert[ nConvert, "labels" ] <- naLeg 
        convert[ nConvert, "groups" ] <- nConvert 
        
        if( int != 1 ){ 
            attr( convert2, "legend" ) <- c( attr( convert2, "legend" ), 
                naLeg ) 
        }   
    }   
    
    
    if( sCol | sFill ){ 
        out[[ "iCol" ]] <- out[[ "iFill" ]] <- convert2[, "col" ]  #  !isNA
        
        out[[ "iBreaks" ]] <- sort( unique( c( convert2[, "from" ], 
            convert2[ nrow( convert2 ), "to" ] ) ), na.last = TRUE ) # !isNA
        
        mi_x <- c( x, out[[ "iBreaks" ]] )
        ma_x <- max( mi_x[ is.finite( mi_x ) ], na.rm = TRUE )
        mi_x <- min( mi_x[ is.finite( mi_x ) ], na.rm = TRUE ) 
        
        out[[ "iBreaks" ]][ out[[ "iBreaks" ]] == -Inf ] <- mi_x - abs( mi_x )*2
        out[[ "iBreaks" ]][ out[[ "iBreaks" ]] == +Inf ] <- ma_x + abs( ma_x )*2
    }   
    
    
    if( sCol ){ 
        out[[ "col" ]] <- function(x){ 
            #   Cast x into a data.frame
            x <- data.frame( "values" = x, "id" = 1:length(x), 
                "labels" = as.character( cut( x = x, 
                    breaks = c( convert[, "from" ], convert[ nrow(convert), "to" ] ), 
                    labels = convert[, "labels" ], right = right, 
                    include.lowest = include.lowest ) ), 
                stringsAsFactors = FALSE ) 
            
            #   Merge with the colors
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colors" ) ]
                by    = "labels", 
                all.x = TRUE, 
                sort  = FALSE 
            )   
            
            x <- x[ order( x[, "id" ] ), ] 
            
            #   Add color for missing values
            x[ is.na( x[, "values" ] ), "col" ] <- naCol 
            
            # if( any( is.na( x[, "col" ] ) ) ){ 
                # warning( "Some values in 'x' are new. Color set to NA" ) 
            # }   
            
            return( as.character( x[, "col" ] ) ) 
        }   
        
        #   Give the function a clean environment
        environment( out[[ "col" ]] ) <- new.env() 
        
        #   Populate the environment
        assign( "convert", convert2[, c( "labels", "col", "from", "to", "groups" ) ], # , "groupLabels"
            envir = environment( out[[ "col" ]] ) ) 
        
        assign( "right",          right,          envir = environment( out[[ "col" ]] ) ) 
        assign( "include.lowest", include.lowest, envir = environment( out[[ "col" ]] ) ) 
        assign( "naCol",          naCol,          envir = environment( out[[ "col" ]] ) ) 
        #assign( "style",          1,              envir = environment( out[[ "col" ]] ) ) 
        
        #   Legend function
        formals( out[[ "legend" ]] )[[ "col" ]] <- convert2[, "col" ] 
        
        #   Add the legend function
        formals( out[[ "legend" ]] )[[ "legend" ]] <- convert2[, "labels" ] 
    }   
    
    
    if( sFill ){ 
        out[[ "fill" ]] <- function(x){ 
            #   Cast x into a data.frame
            x <- data.frame( "values" = x, "id" = 1:length(x), 
                "labels" = as.character( cut( x = x, 
                    breaks = c( convert[, "from" ], convert[ nrow(convert), "to" ] ), 
                    labels = convert[, "labels" ], right = right, 
                    include.lowest = include.lowest ) ), 
                stringsAsFactors = FALSE ) 
            
            #   Merge with the colors
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colors" ) ]
                by    = "labels", 
                all.x = TRUE, 
                sort  = FALSE 
            )   
            
            x <- x[ order( x[, "id" ] ), ] 
            
            #   Add color for missing values
            x[ is.na( x[, "values" ] ), "col" ] <- naCol 
            
            # if( any( is.na( x[, "col" ] ) ) ){ 
                # warning( "Some values in 'x' are new. Color set to NA" ) 
            # }   
            
            return( as.character( x[, "col" ] ) ) 
        }   
        
        #   Give the function a clean environment
        environment( out[[ "fill" ]] ) <- new.env() 
        
        #   Populate the environment
        assign( "convert", convert2[, c( "labels", "col", "from", "to", "groups" ) ], # , "groupLabels"
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign( "right",          right,          envir = environment( out[[ "fill" ]] ) ) 
        assign( "include.lowest", include.lowest, envir = environment( out[[ "fill" ]] ) ) 
        assign( "naCol",          naCol,          envir = environment( out[[ "fill" ]] ) ) 
        
        # assign( "style", ifelse( int == 1, 1, 2 ), envir = environment( out[[ "legend" ]] ) ) 
        
        if( hasNA ){ 
            convert2 <- rbind( convert2, NA ) 
            
            nconvert2 <- nrow( convert2 ) 
            convert2[ nconvert2, "col" ]    <- naCol 
            # convert2[ nconvert2, "labels" ] <- naLeg 
            convert2[ nconvert2, "groups" ] <- 
                max( convert2[, "groups" ], na.rm = TRUE ) + 1  
        }else{ 
            convert2 <- convert2 
        }   
        
        
        #   Prepare the legend function
        formals( out[[ "legend" ]] )[[ "fill" ]] <- convert2[, "col" ] 
        
        if( int != 1L ){    # Otherwise drawn continuous legend
            formals( out[[ "legend" ]] )[[ "groups" ]] <- convert2[, "groups" ] 
            formals( out[[ "legend" ]] )[[ "legend" ]] <- attr( convert2, "legend" )
            formals( out[[ "legend" ]] )[[ "style" ]]  <- 2 
        }else{ 
            formals( out[[ "legend" ]] )[[ "legend" ]] <- convert[, "labels" ] 
        }   
    }   

    
    
    return( out ) 
}   



#'@rdname setColorScale-methods
#'
#'@method setColorScale matrix
#'
#'@export 
#'
setColorScale.matrix <- function( 
 x, 
 ...
){  #   x dimentions
    dx <- dim( x )
    
    #   Matrix -> vector
    x <- as.vector( x ) 
    
    #   
    cs <- setColorScale(
        x     = x, 
        naCol = NA, 
        ...
    )   
    
    
    #   New color function
    if( "col" %in% names(cs) ){ 
        colOriginal <- cs[[ "col" ]] 
        
        cs[[ "col" ]] <- function(x){ 
            return( matrix( 
                data  = colOriginal( as.vector( x ) ), 
                nrow  = dx[1], 
                ncol  = dx[2], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( cs[[ "col" ]] ) <-  new.env() 
        assign( pos = environment( cs[[ "col" ]] ), x = "colOriginal", value = colOriginal,  ) 
        assign( pos = environment( cs[[ "col" ]] ), x = "dx",          value = dx ) 
        
        convert <- as.list( environment( colOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "from" ] ), ] 
        
        isNotNA <- (!is.na( convert[, "from" ] )) & 
            (!is.na( convert[, "to" ] ))
        
        formals( cs[[ "legend" ]] )[[ "col" ]][ !isNotNA ] <- 
            NA_character_ 
    }   
    
    
    #   New fill function
    if( "fill" %in% names(cs) ){ 
        fillOriginal <- cs[[ "fill" ]] 
        
        cs[[ "fill" ]] <- function(x){ 
            return( matrix( 
                data  = fillOriginal( as.vector( x ) ), 
                nrow  = dx[1], 
                ncol  = dx[2], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( cs[[ "fill" ]] ) <-  new.env() 
        assign( pos = environment( cs[[ "fill" ]] ), x = "fillOriginal", value = fillOriginal,  ) 
        assign( pos = environment( cs[[ "fill" ]] ), x = "dx",           value = dx ) 
        
        convert <- as.list( environment( fillOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "from" ] ), ] 
        
        isNotNA <- (!is.na( convert[, "from" ] )) & 
            (!is.na( convert[, "to" ] ))
        
        formals( cs[[ "legend" ]] )[[ "fill" ]][ !isNotNA ] <- 
            NA_character_ 
    }   
    
    
    return( cs )
}   



#'@rdname setColorScale-methods
#'
#'@method setColorScale RasterLayer
#'
#'@export 
#'
setColorScale.RasterLayer <- function( 
 x, 
 ...
){  
    if( !"raster" %in% rownames( installed.packages() ) ){ 
        stop( "setColorScale.RasterLayer requires the package 'raster' to be installed" ) 
    }   
    
    
    #   Load the package raster:
    pkg <- "raster"
    if( !require( pkg, character.only = TRUE ) ){ 
        stop( "The package raster can not be loaded" )
    }   
    
    
    # hasNA <- any( is.na( values( x ) ) )
    u <- get( "unique", "package:raster" )
    x <- u( x )
    
    isna  <- get( "is.na", "package:raster" )
    hasNa <- any( u( isna( x ) ) ) 
    
    if( any( u( isna( x ) ) ) ){ 
        x <- c( x, NA )  
    }   
    
    cs <- setColorScale(
        x = x, 
        ...
    )   
    
    
    #   New color function
    if( "col" %in% names(cs) ){ 
        cs[[ "col" ]] <- function(x){ 
            stop( "Not implemented. Technically can't return a raster of colors (character strings)" )
        }   
    }   
    
    
    #   New fill function
    if( "fill" %in% names(cs) ){ 
        cs[[ "fill" ]] <- function(x){ 
            stop( "Not implemented. Technically can't return a raster of fill-colors (character strings)" )
        }   
    }   
    
    
    return( cs )
}   



# plotAnywhere ==================================================

#' Plot (a legend) anywhere in the figure (margins included)
#'
#' Plot (a legend) anywhere in the figure (margins included). Creates 
#'  a temporary figure-overlay with extra small margins (see 
#'  \code{mar}) and with figure extent of 0 to 1 in the x and y 
#'  axis, so that some plot (a legend) can be overlain on top 
#'  of an existing figure.
#'
#'
#'@param expr
#'  An \code{\link[base]{expression}}, presumably some plot-overlay.
#'
#'@param mar
#'  See \code{\link[graphics]{par}}.
#'
#'@param .clip
#'  Single logical value. If \code{TRUE} (default), the 
#'  \code{\link[graphics]{clip}}pping region is preserved too.
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[graphics]{par}}.
#'
#'
#'@return 
#'  Invisibly returns the original \code{\link[graphics]{par}} 
#'  settings
#'
#'
#'@export 
#'
plotAnywhere <- function( 
 expr  = NULL, 
 mar   = rep(.1,4), 
 .clip = TRUE, 
 ... 
){  
    # Get the original expression
    # expr <- deparse( substitute( expr ) )
    
    # #   Get old graphical parameters
    oldPar <- par() 
    
    #   Set new graphical parameters
    par( "mar" = mar, new = TRUE, ... ) 
    
    #   Ghost plot (empty), to set the new figure
    plot( x = c(0,1), y = c(0,1), type = "n", bty = "n", 
        xaxt = "n", yaxt = "n", xlab = "", ylab = "" ) 
    
    #   Call the function
    eval( expr )
    
    #   Reset margins, figure region and figure region relative 
    #   extent
    par( "mar" = oldPar[[ "mar" ]], "usr" = oldPar[[ "usr" ]], 
        "plt" = oldPar[[ "plt" ]], new = TRUE, ... ) 
    
    #   Reset the clip region
    if( .clip ){ 
        do.call( "clip", as.list( oldPar[[ "usr" ]] ) ) 
    }   
    
    par( new = FALSE ) 
    
    return( invisible( oldPar[[ "usr" ]] ) ) 
}   



# matrix2image ==================================================

#' Arrange a matrix for display with image() that respects original rows and columns
#'
#' Arrange a matrix for display with image() that respects original rows and columns
#'
#'
#'@param x
#'  An \code{\link[base]{matrix}}
#'
#'
#'@return 
#'  Return a transposed and column-flipped matrix
#'
#'
#'@export 
#'
matrix2image <- function(x){ 
    dm <- dim( x )
    return( t( x )[ , dm[1]:1 ] ) # nc:1
}   

