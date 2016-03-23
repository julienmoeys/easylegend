
# +-------------------------------------------------------------+
# | Language: R + roxygen2 inline documentation
# | Package: easylegend 
# | Author(s): Julien Moeys <Julien.Moeys@@slu.se> 
# | License: AGPL3, Affero General Public License version 3 
# +-------------------------------------------------------------+

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
#'@param decreasing
#'  Logical value. If \code{TRUE} (not default), and \code{ord = TRUE} 
#'  the legend is ordered (sorted) in decreasing order (i.e. low 
#'  values on the bottom of the color scale and high values on the 
#'  top of the color scale).
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
#'@example inst/examples/setFactorGraphics-examples.R
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
 decreasing=FALSE, 
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
    
    
    #   Prepare a conversion table: From x-values to colors, symbols, 
    #   line type, etc.
    convert <- data.frame( 
        "values"    = unique(x), 
        #"col"       = NA_character_, 
        #"pch"       = as.integer( NA_real_ ), 
        #"lty"       = as.integer( NA_real_ ), 
        #"legends"   = NA_character_, 
        stringsAsFactors = FALSE 
    )   
    
    
    if( ord ){ 
        convert <- convert[ order( convert[, "values" ], decreasing = decreasing ), , 
            drop = FALSE ] 
            # So if there is just one column it 
            # does not become a vector
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
                    out[[ "iBreaks" ]][ 1L ] - db2, 
                    out[[ "iBreaks" ]][ 1L ] + db2, 
                    out[[ "iBreaks" ]][ 2L ] + db2
                )   
                
            }else{ 
                out[[ "iBreaks" ]] <- c( 
                    out[[ "iBreaks" ]][ 1L ] - db2[ 1L ], 
                    out[[ "iBreaks" ]][ 1L:(l-1L) ] + db2, 
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
#'@example inst/examples/asCol-examples.R
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
#'@example inst/examples/asPch-examples.R
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
 fill,   # fill colours
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
    
    #   Find the width of the filled boxes
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
#'@param decreasing
#'  Logical value. If \code{TRUE} (default), the legend is ordered 
#'  (sorted) in decreasing order (i.e. low values on the bottom of 
#'  the colour scale and high values on the top of the colour scale).
#'
#'
#'@param y.intersp 
#'  Single numerical value. Character interspacing factor for 
#'  vertical (y) spacing of the color legend. Passed to 
#'  \code{\link[graphics]{legend}}.
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
#'@example inst/examples/setColorScale-examples.R
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
    message( "easylegend::setColorScale will be deprecated in the future. Use setColourScale or setColourRampScale instead" )
    
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
    decreasing = TRUE, 
    y.intersp = 1.5, 
    ...      # passed to format
){  
    # sCol is TRUE if colour-legend should be calculated, FALSE otherwise
    sCol  <- ifelse( all( is.logical( col ) ), col[1L], TRUE ) 
    
    # sFill is TRUE if fill-legend should be calculated, FALSE otherwise
    sFill <- ifelse( all( is.logical( fill ) ), fill[1L], TRUE ) 
    
    if( !any( c( sCol, sFill ) ) ){ 
        stop( "Either 'col' or 'fill' must be set (to TRUE or a vector of colors)" ) 
        
    }else if( sCol & sFill ){ 
        stop( "Specify either 'col', or 'fill', but not both at the same time" ) 
        
    }
    
    #   Case: a colour-legend should be generated
    #       and no colour are specified. 
    #       Generating a 5-colour range
    if( sCol & all( is.logical( col ) ) ){ 
        # col  <- gray( c( .75, .50, .25, 0 ) ) 
        col  <- hsv( 
            h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
            s = seq( .20,  .8, length.out = 5 ), 
            v = seq( .95,  .3, length.out = 5 ) )
            #        light dark
    }
    
    #   Case: a fill-legend should be generated
    #       and no colour are specified
    #       Generating a 5-colour range
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
    
    
    #   Check sCol and sFill (enough colour provided)
    if( sCol & (length( col ) < 2) ){ 
        stop( "At least 2 colors ('col') should be provided" )
    }   
    
    if( sFill & (length( fill ) < 2) ){ 
        stop( "At least 2 colors ('fill') should be provided" )
    }   
    
    
    #   Find out how many colours were provided
    n <- ifelse( sCol, length(col), length(fill) ) 
    
    
    #   Has x any NA values?
    hasNA <- any( is.na( x ) ) 
    
    
    #   Define the breaks in x values
    if( is.null( breaks ) ){ 
        if( length( x ) == 0 ){
            stop( "length(x) (or number of values in 'x') is 0" ) 
        }   
        
        #   Unique x values:
        u_x <- unique( x ) 
        u_x <- u_x[ !is.na( u_x ) ] 
        u_x <- u_x[ !is.nan( u_x ) ] 
        u_x <- u_x[ !is.infinite( u_x ) ] 
        
        #   Handle single unique values
        if( length( u_x ) == 0 ){
            stop( "All values in 'x' are NA, NaN or infinite" ) 
            
        }else if( length( u_x ) == 1L ){
            if( u_x == 0 ){
                u_x <- c( -1e-3, u_x, 1e-3 )
                
            }else{
                u_x <- c( u_x - (u_x/1000), u_x, u_x + (u_x/1000) )
                
            }   
        }   
        
        if( decreasing ){ 
            breaks <- seq( 
                from = max( u_x, na.rm = hasNA ), 
                to   = min( u_x, na.rm = hasNA ), 
                length.out = n + 1 ) 
        }else{ 
            breaks <- seq( 
                from = min( u_x, na.rm = hasNA ), 
                to   = max( u_x, na.rm = hasNA ), 
                length.out = n + 1 ) 
        }   
    }else{ 
        # Test that the breaks are sorted correctly
        test <- sort( breaks, na.last = TRUE, decreasing = decreasing )
        test <- any( test != breaks ) | 
                (length( unique( breaks ) ) != length( breaks )) 
        
        if( test ){ 
            stop( "'breaks' should be ordered (NA last), with no replica, and consistent with argument 'decreasing'" ) 
        };  rm( test )
    }   
    
    
    #   Output list
    out <- list( 
        "legend" = function( 
            # Arguments that exists in legend()
            x, y = NULL, legend, col = NULL, 
            fill = NULL, border = "black", cex = 1, 
            text.col = par( "col" ), text.font = NULL, 
            title.col = par( "col" ), horiz = FALSE, 
            title = NULL, y.intersp = y.intersp, ..., 
            
            #   Arguments that do not exists in legend() (extra)
            groups = NULL,  
            style  = 1L     # style == 1L means no intermediate colors. 
                            # style != 1L means with intermediate colors
            
        ){  
            # if( !exists( "style" ) ){ style <- 1L }
            
            if( horiz ){ stop( "'horiz' = TRUE not supported in easylegend" ) }
            
            #   Prepare a list (of arguments) that will be passed 
            #   to the function legend using do.call()
            
            arguments <- list( "x" = x, "y" = y, title.col = title.col, 
                title = title, y.intersp = y.intersp ) 
            
            if( !is.null(col) ){ 
                arguments <- c( arguments, list( "col" = col, 
                    "legend" = legend ) ) 
                
            }else if( style != 1 ){ # !is.null( fill )
                # style != 1 means a color ramp with intermediate colors
                # 
                
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
            
            if( style != 1L ){ # (!is.null( fill )) & (!is.null( groups ))
                # style != 1 means a color ramp with intermediate colors
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
    
    
    # breaks <- c(Inf, 1, 0, -1, -Inf); decreasing <- TRUE  
    # breaks <- c(-Inf, -1, 0, 1, Inf); decreasing <- FALSE  
    
    
    #   convert between class breaks and colors
    mid  <- breaks[ 1:(length(breaks)-1) ] + (diff( breaks )/2) # ifelse( decreasing, +1, -1 )
    from <- breaks[ 1:(length(breaks)-1) ]
    to   <- breaks[ 2:length(breaks) ] 
    
    
    
    #   Set the midpoint values for cases with infinite from or to
    
    .sanitiseMid <- function( mid, from, to ){ 
        dff <- diff( c( from, to[ length( to ) ] ) )/2
        
        testInf <- from == +Inf
        mid[ testInf ] <- to[ which( testInf ) ] - dff[ which( testInf ) + 1L ]
        #   By definition from == +Inf implied that decreasing is TRUE
        
        testInf <- from == -Inf
        mid[ testInf ] <- to[ which( testInf ) ] - dff[ which( testInf ) + 1L ]
        #   By definition from == -Inf implied that decreasing is FALSE
        
        testInf <- to == -Inf
        mid[ testInf ] <- from[ which( testInf ) ] + dff[ which( testInf ) - 1L ]
        #   By definition to == -Inf implied that decreasing is TRUE
        
        testInf <- to == +Inf
        mid[ testInf ] <- from[ which( testInf ) ] + dff[ which( testInf ) - 1L ]
        #   By definition to == +Inf implied that decreasing is FALSE
        
        return( mid )
    }   
    
    mid <- .sanitiseMid( "mid" = mid, "from" = from, "to" = to ) 
    
    
    convert <- data.frame( 
        "from"           = from, 
        "to"             = to, 
        "mid"            = mid, 
        "col"            = if( sCol ){ col }else{ fill }, 
        "labels"         = NA_character_, 
        "internalLabels" = NA_character_, 
        stringsAsFactors = FALSE 
    )   
    rm( breaks, col, from, to, mid )  
    
    
    #   Prepare the color-class labels
    .makeLabels <- function( 
        from, 
        to, 
        brackets = brackets, 
        digits   = digits, 
        nsmall   = nsmall, 
        ... 
    ){  
        labs <- paste0( 
            brackets[1], 
            format( from, digits = digits, nsmall = nsmall, ... ), 
            brackets[2], 
            format( to,   digits = digits, nsmall = nsmall, ... ), 
            brackets[3] ) 
        
        if( any( testInf <- from == +Inf ) ){ 
            labs[ testInf ] <- sprintf( 
                "> %s", 
                format( to[ testInf ], digits = digits, nsmall = nsmall, ... ) )       
        }   
        
        if( any( testInf <- from == -Inf ) ){ 
            labs[ testInf ] <- sprintf( 
                "< %s", 
                format( to[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        }   
        
        if( any( testInf <- to == +Inf ) ){ 
            labs[ testInf ] <- sprintf( 
                "> %s", 
                format( from[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        }   
        
        if( any( testInf <- to == -Inf ) ){ 
            labs[ testInf ] <- sprintf( 
                "< %s", 
                format( from[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        }   
        
        return( labs )
    }   
    
    if( is.null( labels ) ){ 
        convert[, "labels" ] <- .makeLabels( 
            from     = convert[, "from" ], 
            to       = convert[, "to" ], 
            brackets = brackets, 
            digits   = digits, 
            nsmall   = nsmall, 
            ... 
        )   
        
    }else{ 
        convert[, "labels" ] <- labels 
        rm( labels )
    }   
    
    #   Internal labels are necessary to account for the 
    #   that rounding may accidentally creates the same 
    #   labels for two different intervals
    convert[, "internalLabels" ] <- .makeLabels( 
        from     = convert[, "from" ], 
        to       = convert[, "to" ], 
        brackets = brackets, 
        digits   = 16, 
        nsmall   = 16, 
        ... 
    )   
    
    nConvert <- nrow( convert ) 
    
    if( int == 1L ){ 
        convert2              <- convert 
        convert2[, "groups" ] <- 1:nConvert 
        #convert2[, "groupLabels" ] <- convert2[, "labels" ] 
        
    }else{ 
        nConvert2 <- nConvert*int
        
        # if( decreasing ){
            # grp <- as.integer( rep( nConvert:1, each = int ) ) 
        # }else{
            # grp <- as.integer( rep( 1:nConvert, each = int ) ) 
        # }   
        
        convert2 <- data.frame( 
            "from"           = rep( NA_real_, nConvert2 ), 
            "to"             = rep( NA_real_, nConvert2 ), 
            "mid"            = rep( NA_real_, nConvert2 ), 
            "col"            = rep( NA_character_, nConvert2 ), 
            "labels"         = rep( NA_character_, nConvert2 ), 
            "internalLabels" = rep( NA_character_, nConvert2 ), 
            #"groups"        = as.integer( rep( NA_real_, nConvert*int ) ), 
            "groups"         = as.integer( rep( 1:nConvert, each = int ) ) , 
            #"groupLabels"   = rep( NA_character_, nConvert*int ), 
            stringsAsFactors = FALSE 
        )   
        
        # rm( grp ) 
        
        #   Interpolate colours, adding mid-point colors
        cr  <- colorRampPalette( 
            colors = convert[, "col" ], 
            bias   = 1, 
            space  = "Lab" ) 
        cr  <- cr( 2 * nConvert - 1 ) 
        
        
        #   Interpolate colours, adding intermediate colours
        cr  <- colorRampPalette( 
            colors = c( 
                convert[ 1, "col" ], 
                cr, 
                convert[ nConvert, "col" ] ), 
            bias   = 1, 
            space  = "Lab" ) 
        convert2[, "col" ]  <- cr( nConvert2 ) 
        
        # if( decreasing ){ 
            # # 2015-05-22 Attempt to fix a bug that causes the 
            # # legend to have colours in the wrong order!
            # convert2[, "col" ] <- rev( convert2[, "col" ] ) 
        # }   
        
        convert2 <- do.call( what = "rbind", args = lapply( 
            X   = split( 
                x = convert2, 
                f = convert2[, "groups" ] ), 
            FUN = function(X){ 
                # tmp <- split( x = convert2, f = convert2[, "groups" ] ); X <- tmp[[1L]]
                
                i <- X[ 1L, "groups" ] 
                n <- nrow( X )
                
                # fromTo2 <- fromTo <- sort( 
                    # unlist( convert[ i, c( "from", "to" ) ] ), 
                    # decreasing = decreasing ) 
                
                fromTo2 <- fromTo <- unlist( convert[ i, c( "from", "to" ) ] ) 
                
                #   Neutralise infinite values
                isInf      <- is.infinite( fromTo ) 
                infReplace <- c( 
                    "from" = NA_real_, 
                    "to"   = NA_real_ ) 
                
                if( isInf[ "from" ] ){ 
                    if( fromTo[ "from" ] == +Inf ){
                        fromTo[ "from" ] <- 
                            infReplace[ "from" ] <- 
                            fromTo[ "to" ] + 1L
                        
                    }else{
                        fromTo[ "from" ] <- 
                            infReplace[ "from" ] <- 
                            fromTo[ "to" ] - 1L
                        
                    }   
                    # fromTo[ "from" ] <- fromTo[ "to" ] - 1L
                    # # ifelse( decreasing, +1, -1 ) 
                }   
                
                if( isInf[ "to" ] ){ 
                    if( fromTo[ "to" ] == +Inf ){
                        fromTo[ "to" ] <- 
                            infReplace[ "to" ] <- 
                            fromTo[ "from" ] + 1L
                        
                    }else{
                        fromTo[ "to" ] <- 
                            infReplace[ "to" ] <- 
                            fromTo[ "from" ] - 1L
                        
                    }   
                    # fromTo[ "to" ] <- fromTo[ "from" ] + 1L
                    # # ifelse( decreasing, -1, +1 ) 
                }   
                
                # X[, "from" ] <- seq( 
                    # from       = fromTo[ ifelse( decreasing, 1L, 2L ) ], 
                    # to         = fromTo[ ifelse( decreasing, 2L, 1L ) ], 
                    # length.out = n+1L )[ -(n+1L) ] # -(n+1L)
                
                X[, "from" ] <- seq( 
                    from       = fromTo[ 1L ], 
                    to         = fromTo[ 2L ], 
                    length.out = n+1L )[ -(n+1L) ] # -(n+1L)
                
                # X[, "to" ] <- c( 
                    # X[ -1L, "from" ], 
                    # fromTo[ ifelse( decreasing, 2L, 1L ) ] ) 
                
                X[, "to" ] <- c( 
                    X[ -1L, "from" ], 
                    fromTo[ 2L ] ) 
                
                #   Re-attribute infinite values
                if( isInf[ "from" ] ){ 
                    X[ 
                        X[, "from" ] == infReplace[ "from" ], 
                        "from" 
                    ] <- fromTo2[ "from" ] 
                }   
                
                if( isInf[ "to" ] ){ 
                    X[ 
                        X[, "to" ] == infReplace[ "to" ],  
                        "to" 
                    ] <- fromTo2[ "to"   ] 
                }   
                
                rm( infReplace, isInf )
                
                X[, "mid" ] <- rowMeans( x = X[, c( "from", "to" ) ] ) 
                
                X[, "mid" ] <- .sanitiseMid( # Function defined above
                    "mid"  = X[, "mid" ], 
                    "from" = X[, "from" ], 
                    "to"   = X[, "to" ] ) 
                
                return( X ) 
            }   
        ) ) 
        
        convert2 <- unique( convert2 ) 
        
        convert2 <- convert2[ order( convert2[, "from" ], 
            decreasing = decreasing ), ]
        
        #   Add a pretty label will be necessary when 
        #   creating the fill function that will convert 
        #   numeric values into fill-colours (with gradients)
        convert2[, "labels" ] <- .makeLabels( 
            from     = convert2[, "from" ], 
            to       = convert2[, "to" ], 
            brackets = brackets, 
            digits   = digits, 
            nsmall   = nsmall, 
            ... 
        )   
        
        #   Internal labels are necessary to account for the 
        #   that rounding may accidentally creates the same 
        #   labels for two different intervals
        convert2[, "internalLabels" ] <- .makeLabels( 
            from     = convert2[, "from" ], 
            to       = convert2[, "to" ], 
            brackets = brackets, 
            digits   = 16, 
            nsmall   = 16, 
            ... 
        )   
        
        #   Make a new legend for each break point
        attr( convert2, "legend" ) <- c( convert[, "from" ], 
            convert[ nConvert, "to" ] ) 
        
        isInfLeg <- is.infinite( attr( convert2, "legend" ) ) 
        
        attr( convert2, "legend" ) <- format( attr( convert2, "legend" ), 
            digits = digits, nsmall = nsmall, ... ) 
        
        attr( convert2, "legend" )[ isInfLeg ] <- ""
        
        rm( isInfLeg ) 
        
        # convert2[, "groupLabels" ] <- paste0( brackets[1], 
                # format( min( convert2[, "from" ] ), digits = digits, nsmall = nsmall, ... ), brackets[2], 
                # format( max( convert2[, "to" ] ), digits = digits, nsmall = nsmall, ... ), brackets[3] ) 
        
    }   # end int != 1L
    
    # browser() 
    
    if( hasNA ){ 
        convert  <- rbind( convert,  NA ) 
        
        nConvert <- nrow( convert ) 
        convert[ nConvert, "col" ]            <- naCol 
        convert[ nConvert, "labels" ]         <- naLeg 
        convert[ nConvert, "internalLabels" ] <- naLeg 
        convert[ nConvert, "groups" ]         <- nConvert 
        
        if( int != 1 ){ 
            attr( convert2, "legend" ) <- c( attr( convert2, "legend" ), 
                naLeg ) 
        }   
    }   
    
    
    if( sCol | sFill ){ 
        if( decreasing ){ 
            out[[ "iCol" ]] <- out[[ "iFill" ]] <- rev( convert2[, "col" ] ) #  !isNA
        }else{ 
            out[[ "iCol" ]] <- out[[ "iFill" ]] <- convert2[, "col" ] #  !isNA
        }   
        
        # # 2015-05-22 Attempt to fix a bug in colour order (see above)
        # out[[ "iCol" ]] <- out[[ "iFill" ]] <- convert2[, "col" ] #  !isNA
        
        out[[ "iBreaks" ]] <- sort( 
            unique( c( convert2[, "from" ], convert2[ nrow( convert2 ), "to" ] ) ), 
            na.last = TRUE, 
            decreasing = FALSE ) # This is not a mistake
        
        mi_x <- c( x, out[[ "iBreaks" ]] )
        ma_x <- max( mi_x[ is.finite( mi_x ) ], na.rm = TRUE )
        mi_x <- min( mi_x[ is.finite( mi_x ) ], na.rm = TRUE ) 
        
        out[[ "iBreaks" ]][ out[[ "iBreaks" ]] == -Inf ] <- mi_x - abs( mi_x )*2
        out[[ "iBreaks" ]][ out[[ "iBreaks" ]] == +Inf ] <- ma_x + abs( ma_x )*2
    }   
    
    
    if( sCol ){ 
        out[[ "col" ]] <- function(x){ 
            
            #   Find which rows are NA
            rowIsNa <- apply( 
                X      = convert[, c( "from", "to" ) ], 
                MARGIN = 1, # rows
                FUN    = function(x){ any( is.na( x ) ) } 
            )   
            
            #   Find the breaks and labels (and remove NA's)
            breaks <- c( 
                convert[ !rowIsNa, "from" ], 
                convert[ !rowIsNa, ][ nrow( convert[ !rowIsNa, ] ), "to" ] )
            lab    <- convert[ !rowIsNa, "internalLabels" ]
            
            if( decreasing ){
                breaks <- rev( breaks ) 
                lab    <- rev( lab ) 
            }   
            
            #   Add NA's again
            if( any( rowIsNa ) ){
                breaks <- c( breaks, convert[ rowIsNa, "from" ][ 1L ] )
                lab    <- c( lab, convert[ rowIsNa, "internalLabels" ][ 1L ] )
            }   
            
            #   Cast x into a data.frame
            x <- data.frame( 
                "values"         = x, 
                "id"             = 1:length(x), 
                "internalLabels" = as.character( cut( 
                    x      = x, 
                    breaks = breaks, 
                    labels = lab, 
                    right  = right, 
                    include.lowest = include.lowest 
                ) ), 
                stringsAsFactors = FALSE ) 
            
            rm( breaks, lab ) 
            
            #   Merge with the colors
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colors" ) ]
                by    = "internalLabels", 
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
        assign( 
            x     = "convert", 
            value = convert2[, c( "labels", "internalLabels", "col", 
                "from", "to", "groups" ) ], # , "groupLabels"
            envir = environment( out[[ "col" ]] ) ) 
        
        assign( 
            x     = "right",          
            value = right,          
            envir = environment( out[[ "col" ]] ) ) 
        
        assign( 
            x     = "include.lowest", 
            value = include.lowest, 
            envir = environment( out[[ "col" ]] ) ) 
        
        assign( 
            x     = "naCol",          
            value = naCol,          
            envir = environment( out[[ "col" ]] ) ) 
        
        assign(   # Part of bug fix for colour order
            x     = "decreasing",          
            value = decreasing,          
            envir = environment( out[[ "col" ]] ) ) 
        
        #assign( "style",          1,              envir = environment( out[[ "col" ]] ) ) 
        
        #   Legend function
        formals( out[[ "legend" ]] )[[ "col" ]] <- convert2[, "col" ] 
        
        #   Add the legend function
        formals( out[[ "legend" ]] )[[ "legend" ]] <- convert2[, "labels" ] 
    }   
    
    
    if( sFill ){ 
        out[[ "fill" ]] <- function(x){ 
            
            #   Find which rows are NA
            rowIsNa <- apply( 
                X      = convert[, c( "from", "to" ) ], 
                MARGIN = 1, # rows
                FUN    = function(x){ any( is.na( x ) ) } 
            )   
            
            #   Find the breaks and labels (and remove NA's)
            breaks <- c( 
                convert[ !rowIsNa, "from" ], 
                convert[ !rowIsNa, ][ nrow( convert[ !rowIsNa, ] ), "to" ] )
            lab    <- convert[ !rowIsNa, "internalLabels" ]
            
            if( decreasing ){
                breaks <- rev( breaks ) 
                lab    <- rev( lab ) 
            }   
            
            #   Add NA's again
            if( any( rowIsNa ) ){
                breaks <- c( breaks, convert[ rowIsNa, "from" ][ 1L ] )
                lab    <- c( lab, convert[ rowIsNa, "internalLabels" ][ 1L ] )
            }   
            
            #   Cast x into a data.frame
            x <- data.frame( 
                "values"         = x, 
                "id"             = 1:length(x), 
                "internalLabels" = as.character( cut( 
                    x      = x, 
                    breaks = breaks, 
                    labels = lab, 
                    right  = right, 
                    include.lowest = include.lowest ) ), 
                stringsAsFactors = FALSE ) 
            
            rm( breaks, lab ) 
            
            #   Merge with the colors
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colors" ) ]
                by    = "internalLabels", 
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
        assign( 
            x     = "convert", 
            value = convert2[, c( "labels", "internalLabels", 
                "col", "from", "to", "groups" ) ], # , "groupLabels"
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign( 
            x     = "right",          
            value = right,          
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign( 
            x     = "include.lowest", 
            value = include.lowest, 
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign( 
            x     = "naCol",          
            value = naCol,          
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign(   # Part of bug fix for colour order
            x     = "decreasing",          
            value = decreasing,          
            envir = environment( out[[ "fill" ]] ) ) 
        
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
        # if( decreasing ){ # Attempt to fix a bug. Not sure
            # formals( out[[ "legend" ]] )[[ "fill" ]] <- rev( convert2[, "col" ] ) 
        # }else{
            # formals( out[[ "legend" ]] )[[ "fill" ]] <- convert2[, "col" ] 
        # }   
        
        formals( out[[ "legend" ]] )[[ "fill" ]] <- convert2[, "col" ]
        
        if( int != 1L ){    # Otherwise drawn continuous legend
            formals( out[[ "legend" ]] )[[ "groups" ]] <- convert2[, "groups" ] 
            formals( out[[ "legend" ]] )[[ "legend" ]] <- attr( convert2, "legend" )
            formals( out[[ "legend" ]] )[[ "style" ]]  <- 2 
        }else{ 
            formals( out[[ "legend" ]] )[[ "legend" ]] <- convert[, "labels" ] 
        }   
    }   
    
    formals( out[[ "legend" ]] )[[ "y.intersp" ]] <- y.intersp 
    
    out[[ "convert" ]]  <- convert
    out[[ "convert2" ]] <- convert2
    
    # browser()
    
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
    
    
    #   Find argument "decreasing"
    decreasing <- list(...)[[ "decreasing" ]]
    if( is.null( decreasing ) ){
        decreasing <- formals( setColorScale.default )[[ "decreasing" ]]
    }   
    
    
    #   New color function
    if( "col" %in% names(cs) ){ 
        colOriginal <- cs[[ "col" ]] 
        
        cs[[ "col" ]] <- function(x){ 
            return( matrix( 
                data  = colOriginal( as.vector( x ) ), 
                nrow  = dx[1L], 
                ncol  = dx[2L], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( cs[[ "col" ]] ) <-  new.env() 
        assign( pos = environment( cs[[ "col" ]] ), x = "colOriginal", value = colOriginal,  ) 
        assign( pos = environment( cs[[ "col" ]] ), x = "dx",          value = dx ) 
        
        convert <- as.list( environment( colOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "from" ], decreasing = decreasing ), ] 
        
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
                nrow  = dx[1L], 
                ncol  = dx[2L], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( cs[[ "fill" ]] ) <-  new.env() 
        assign( pos = environment( cs[[ "fill" ]] ), x = "fillOriginal", value = fillOriginal,  ) 
        assign( pos = environment( cs[[ "fill" ]] ), x = "dx",           value = dx ) 
        
        convert <- as.list( environment( fillOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "from" ], decreasing = decreasing ), ] 
        
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



# setColourScale ===========================================

#' Prepare colour scale and legend (col, fill) from continuous-numeric data.
#'
#' Prepare colour scale and legend (col, fill) from 
#'  continuous-numeric data. \code{setColourScale} is a 
#'  partial rewriting of \code{\link{setColorScale}}.
#'
#'
#'@seealso \code{\link{setColourRampScale}}, 
#'  \code{\link{setFactorGraphics}} and 
#'  \code{\link[grDevices]{colorRampPalette}}.
#'
#'
#'@param x
#'  A vector of numerical values (continuous).
#'
#'@param col
#'  Logical value or vector of character strings representing 
#'  colours. If \code{TRUE} or vector of colours, a continuous 
#'  colour scale is defined for 'x'. \code{col} and \code{fill} 
#'  can't be set simultaneously (the corresponding legend would 
#'  not work). \code{length(col)} must be equal to 
#'  \code{length(breaks) - 1} or to \code{nBreaks - 1} 
#'  (contrary to \code{\link{setColourRampScale}}).
#'
#'@param fill
#'  Logical value or vector of character strings representing 
#'  fill-colours. If \code{TRUE} or vector of colours, a 
#'  continuous fill-colour scale is defined for 'x'. \code{col} 
#'  and \code{fill} can't be set simultaneously (the 
#'  corresponding legend would not work). 
#'  \code{length(fill)} must be equal to 
#'  \code{length(breaks) - 1} or to \code{nBreaks - 1} 
#'  (contrary to \code{\link{setColourRampScale}}).
#'
#'@param breaks
#'  See \code{\link[base]{cut}}. \code{length(breaks)} must 
#'  be equal to \code{length(col) + 1} or 
#'  \code{length(fill) + 1}. If \code{NULL}, \code{nBreaks} 
#'  breaks will be generated internally (see below). 
#'  \code{breaks} should be ordered (no \code{NA}), with no 
#'  replica, and consistent with the argument \code{decreasing}. 
#'  See \code{naCol} and \code{naLeg} regarding the handling 
#'  of \code{NA}-values.
#'
#'@param brackets
#'  Vector of 3 characters. Used to generate customise 
#'  legend-labels, if \code{labels} is \code{NULL}. 
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
#'  See \code{\link[base]{format}}. Number formatting in the 
#'  legend.
#'
#'@param nsmall
#'  See \code{\link[base]{format}}. Number formatting in the 
#'  legend.
#'
#'@param naCol
#'  Single character string. Colour for \code{NA} (missing) 
#'  values. Notice that in the case of an 
#'  \code{\link[graphics]{image}} plot this parameter is 
#'  silently be ignored and replaced by \code{NA} as 
#'  the function displays missing values as transparent pixels.
#'  In the case of \code{raster::plot}, One should make sure 
#'  that the value of \code{naCol}-argument in 
#'  \code{setColourRampScale} is coherent with the value of 
#'  \code{naCol}-argument in \code{raster::plot}.
#'
#'@param naLeg
#'  Single character string. Legend (label) for \code{NA} 
#'  (missing) values.
#'
#'@param decreasing
#'  Logical value. If \code{TRUE} (default), the legend is 
#'  ordered (sorted) in decreasing order (i.e. low values on 
#'  the bottom of the colour scale and high values on the top 
#'  of the colour scale).
#'
#'@param y.intersp 
#'  Single numerical value. Character inter-spacing factor 
#'  for vertical (y) spacing of the colour legend. Passed to 
#'  \code{\link[graphics]{legend}}.
#'
#'@param nBreaks
#'  Single integer value. Number of breaks that must be 
#'  generated internally when \code{breaks} is not specified 
#'  (\code{NULL}). See argument \code{breaks} for details 
#'   on how \code{nBreaks} and \code{col} or \code{fill} 
#'  are related.
#'
#'@param hideMinMax
#'  Vector of two logical value. If \code{TRUE}, the min or 
#'  the max values in breaks (or both) will be replaced by 
#'  \code{> x} or \code{< y} where \code{x} is the 2nd highest 
#'  value in \code{breaks} and \code{y} is the 2nd smallest 
#'  value in \code{breaks}. \code{hideMinMax[1L]} is for the 
#'  min value and \code{hideMinMax[2L]} is for the max-value.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a list of 2 \code{\link[base]{function}}: 
#'  \code{col} or \code{fill}, a function that converts 
#'  \code{x}-like values into (fill)colours; 
#'  \code{legend}, a function to draw a legend on a plot with 
#'  the correct (fill)colours and legend. That function accepts 
#'  \code{...} arguments, passed to the original 
#'  \code{\link[graphics]{legend}} function.
#'
#'
#'@example inst/examples/setColourScale-examples.R
#'
#'@importFrom grDevices colorRampPalette
#'
#'@rdname setColourScale-methods
#'
#'@export 
#'
setColourScale <- function(
 x, 
 ...
){  
    UseMethod( "setColourScale" )
}   


#'@rdname setColourScale-methods
#'
#'@method setColourScale default
#'
#'@export 
#'
setColourScale.default <- function( 
    x, 
    col      = FALSE, 
    fill     = FALSE, 
    breaks   = NULL, 
    # int      = 1L, 
    naCol    = "lightgray", 
    naLeg    = "na", 
    brackets = c( "", " ", "" ), 
    labels   = NULL, 
    right    = FALSE,       # see cut()
    include.lowest = TRUE,  # see cut()
    digits   = 3L,          # see format()
    nsmall   = 3L,          # see format()
    decreasing = TRUE, 
    y.intersp = 1.5, 
    nBreaks = 6L, # So 5 colours
    hideMinMax = c( FALSE, FALSE ), 
    # alpha = FALSE, 
    ...      # passed to format
){  
    #   sCol is TRUE if colour-legend should be calculated, 
    #   FALSE otherwise
    sCol  <- ifelse( all( is.logical( col ) ), col[1L], TRUE ) 
    
    #   sFill is TRUE if fill-legend should be calculated, 
    #   FALSE otherwise
    sFill <- ifelse( all( is.logical( fill ) ), fill[1L], TRUE ) 
    
    #   Check that either col or fill was specified (not both)
    if( !any( c( sCol, sFill ) ) ){ 
        stop( "Either 'col' or 'fill' must be set (to TRUE or a vector of colours)" ) 
        
    }else if( sCol & sFill ){ 
        stop( "Specify either 'col', or 'fill', but not both at the same time" ) 
        
    }
    
    
    #   Reset 'nBreak's if 'breaks' is supplied and check that 
    #   there are enough breaks
    if( !is.null( breaks ) ){
        # test that there are no NA in breaks
        if( any( is.na( breaks ) ) ){
            stop( "NA not allowed in breaks. Use arguments 'naLeg' and 'naCol' instead" )
        }   
        
        if( length( breaks ) < 3L ){
            stop( sprintf( 
                "When non-NULL, length(breaks) must be at least 3 (now %s)", 
                length(breaks)
            ) ) 
        }   
        
        # if( length( breaks[ is.finite( breaks ) ] ) < 1L ){
            # stop( sprintf( 
                # "The number of finite breaks must be at least 1 (now %s)", 
                # length( breaks[ is.finite( breaks ) ] ) 
            # ) ) 
        # }   
        
        nBreaks <- length( breaks ) 
    }else{
        if( nBreaks < 3L ){
            stop( sprintf( 
                "nBreaks < 3. nBreaks must be at least 3 (now %s)", 
                nBreaks
            ) ) 
        }   
        
    }   
    
    
    #   Case: a colour-legend should be generated
    #       and no colour are specified. 
    #       Generating a 5-colour range
    if( sCol & all( is.logical( col ) ) ){ 
        if( decreasing ){
            col <- hsv( 
                h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
                s = seq( .8,  .20, length.out = nBreaks - 1L ), 
                v = seq( .3,  .95, length.out = nBreaks - 1L ) )
                #        dark light 
        }else{
            col <- hsv( 
                h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
                s = seq( .20,  .8, length.out = nBreaks - 1L ), 
                v = seq( .95,  .3, length.out = nBreaks - 1L ) )
                #        light dark
        }   

    }
    
    
    #   Case: a fill-legend should be generated
    #       and no colour are specified
    #       Generating a 5-colour range
    if( sFill & all( is.logical( fill ) ) ){ 
        # fill <- gray( c( .75, .50, .25, 0 ) ) 
        if( decreasing ){
            fill <- hsv( 
                h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
                s = seq( .8,  .20, length.out = nBreaks - 1L ), 
                v = seq( .3,  .95, length.out = nBreaks - 1L ) )
                #        dark light 
        }else{
            fill <- hsv( 
                h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
                s = seq( .20,  .8, length.out = nBreaks - 1L ), 
                v = seq( .95,  .3, length.out = nBreaks - 1L ) )
                #        light dark
        }   
        
    }   
    
    
    #   Check that col or fill are consistent (in length) 
    #   with breaks
    if( sCol & (length(col) != (nBreaks - 1L)) ){
        stop( sprintf(
            "length(col) must be equal to nBreaks - 1 or length(breaks) - 1. Now respectively %s, %s and %s", 
            length(col), nBreaks, length(breaks) 
        ) ) 
    }   
    
    if( sFill & (length(fill) != (nBreaks - 1L)) ){
        stop( sprintf(
            "length(fill) must be equal to nBreaks - 1 or length(breaks) - 1. Now respectively %s, %s and %s", 
            length(fill), nBreaks, length(breaks) 
        ) ) 
    }   
    
    
    # #   Check sCol and sFill (enough colour provided) # # Useless now
    # if( sCol & (length( col ) < 2) ){ 
        # stop( "At least 2 colours ('col') should be provided" )
    # }   
    
    # if( sFill & (length( fill ) < 2) ){ 
        # stop( "At least 2 colours ('fill') should be provided" )
    # }   
    
    
    # #   Find out how many colours were provided
    # n <- ifelse( sCol, length(col), length(fill) ) 
    
    
    #   Has x any NA values?
    hasNA <- any( is.na( x ) ) 
    
    
    #   Define the breaks in x values
    if( is.null( breaks ) ){ 
        if( length( x ) == 0 ){
            stop( "length(x) (or number of values in 'x') is 0" ) 
        }   
        
        #   Unique x values:
        u_x <- unique( x ) 
        u_x <- u_x[ !is.na( u_x ) ] 
        u_x <- u_x[ !is.nan( u_x ) ] 
        u_x <- u_x[ !is.infinite( u_x ) ] 
        
        #   Handle single unique values
        if( length( u_x ) == 0 ){
            stop( "All values in 'x' are NA, NaN or infinite" ) 
            
        }else if( length( u_x ) == 1L ){
            if( u_x == 0 ){
                u_x <- c( -1e-3, u_x, 1e-3 )
                
            }else{
                u_x <- c( u_x - (u_x/1000), u_x, u_x + (u_x/1000) )
                
            }   
        }   
        
        if( decreasing ){ 
            breaks <- seq( 
                from = max( u_x, na.rm = hasNA ), 
                to   = min( u_x, na.rm = hasNA ), 
                length.out = nBreaks ) 
        }else{ 
            breaks <- seq( 
                from = min( u_x, na.rm = hasNA ), 
                to   = max( u_x, na.rm = hasNA ), 
                length.out = nBreaks ) 
        }   
        
        breaksSanitized <- breaks
    }else{ 
        # Test that the breaks are sorted correctly
        test <- sort( breaks, na.last = TRUE, decreasing = decreasing )
        #   Note: na.last not needed in principle
        
        test <- any( test != breaks ) | 
                (length( unique( breaks ) ) != length( breaks )) 
        
        if( test ){ 
            stop( "'breaks' should be ordered, with no replica, and consistent with argument 'decreasing'" ) 
        };  rm( test )
        
        
        if( any( is.infinite( breaks ) ) ){
            stop( "Some values in breaks are infinite. Replace them by a high or low value and set 'hideMinMax' instead." )
        }   
        
        
        # #   breaksSanitized is a copy of breaks where infinite values
        # #   will be removed. Useful for estimating intermediate
        # #   steps in the breaks
        # breaksSanitized <- breaks
        
        # #   Test for infinite values
        # if( any( is.infinite( breaks ) ) ){
            # #   Calculate the maximum non-infinite difference 
            # #   between the breaks (absolute value)
            # maxAbsDiff <- abs( diff( breaks ) ) 
            # maxAbsDiff <- maxAbsDiff[ is.finite( maxAbsDiff ) ] 
            
            # if( length( maxAbsDiff ) == 0 ){
                # #   Case: less than 2 finite breaks
                # maxAbsDiff <- 1
            # }else{
                # #   Case: at least 2 finite breaks
                # maxAbsDiff <- max( maxAbsDiff )
            # }   
            
            # if( decreasing ){
                # if( breaks[ 1L ] == +Inf ){
                    # # breaksSanitized[ 1L ] <- max( breaks[ is.finite( breaks ) ] ) + maxAbsDiff 
                    # breaksSanitized[ 1L ] <- +.Machine[[ "double.xmax" ]] 
                # }   
                
                # if( breaks[ length( breaks ) ] == -Inf ){
                    # # breaksSanitized[ length( breaks ) ] <- min( breaks[ is.finite( breaks ) ] ) - maxAbsDiff 
                    # breaksSanitized[ length( breaks ) ] <- -.Machine[[ "double.xmax" ]] 
                # }   
            # }else{ # increasing
                # if( breaks[ 1L ] == -Inf ){
                    # # breaksSanitized[ 1L ] <- min( breaks[ is.finite( breaks ) ] ) - maxAbsDiff 
                    # breaksSanitized[ 1L ] <- -.Machine[[ "double.xmax" ]] 
                # }   
                
                # if( breaks[ length( breaks ) ] == +Inf ){
                    # # breaksSanitized[ length( breaks ) ] <- max( breaks[ is.finite( breaks ) ] ) + maxAbsDiff 
                    # breaksSanitized[ length( breaks ) ] <- +.Machine[[ "double.xmax" ]] 
                # }   
            # }   
        # }   
        
    }   
    
    
    #   Output list
    out <- list( 
        "legend" = function( 
            # Arguments that exists in legend()
            x, 
            y           = NULL, 
            legend, 
            col         = NULL, 
            fill        = NULL, 
            border      = "black", 
            cex         = 1, 
            text.col    = par( "col" ), 
            text.font   = NULL, 
            title.col   = par( "col" ), 
            horiz       = FALSE, 
            title       = NULL, 
            y.intersp   = y.intersp, 
            ..., 
            
            #   Arguments that do not exists in legend() (extra)
            groups      = NULL  
            # style     = 1L    # style == 1L means no intermediate colours. 
                                # style != 1L means with intermediate colours
            
        ){  
            if( horiz ){ stop( "'horiz' = TRUE not supported in easylegend::setColourScale" ) }
            
            #   Prepare a list (of arguments) that will be passed 
            #   to the function legend using do.call()
            arguments <- list( 
                "x"         = x, 
                "y"         = y, 
                "title.col" = title.col, 
                "title"     = title, 
                "y.intersp" = y.intersp ) 
            
            if( !is.null(col) ){ 
                arguments <- c( arguments, list( "col" = col, 
                    "legend" = legend ) ) 
                
            }else{
                arguments <- c( arguments, list( "fill" = fill, 
                    "legend" = legend ) ) 
            }   
            
            arguments <- c( arguments, list(...) )
            
            lRes <- do.call( 
                what = get( "legend", pos = "package:graphics" ), 
                args = arguments ) 
            
            return( invisible( arguments ) ) 
        }   
    )    
    
    environment( out[[ "legend" ]] ) <- new.env() 
    
    class( out ) <- "numericGraphics"
    
    from <- breaks[ 1L:(nBreaks - 1L) ]
    to   <- breaks[ 2L:nBreaks ]
    
    # fromSanitized <- breaksSanitized[ 1L:(nBreaks - 1L) ]
    # toSanitized   <- breaksSanitized[ 2L:nBreaks ]
    
    convert <- data.frame( 
        "from"           = from, 
        "to"             = to, 
        "col"            = NA_character_, 
        "labels"         = NA_character_, 
        "internalLabels" = NA_character_, 
        stringsAsFactors = FALSE 
    )   
    
    if( sCol ){ 
        convert[, "col" ] <- col 
    }else{ 
        convert[, "col" ] <- fill 
    }   
    
    rm( breaks, col, fill, from, to ) 
    
    
    #   Prepare the colour-class labels (including replacing 
    #   infinite with > 2nd last break or lower than 2nd break)
    .makeLabels <- function( 
        from, 
        to, 
        hideMinMax = hideMinMax, 
        brackets   = brackets, 
        digits     = digits, 
        nsmall     = nsmall, 
        ... 
    ){  
        labs <- paste0( 
            brackets[1], 
            format( from, digits = digits, nsmall = nsmall, ... ), 
            brackets[2], 
            format( to,   digits = digits, nsmall = nsmall, ... ), 
            brackets[3] ) 
        
        if( hideMinMax[ 1L ] ){ # Case: The min value should be hidden
            
            testFrom <- from == min( c( from, to ) )
            
            if( any( testFrom ) ){
                labs[ testFrom ] <- sprintf( 
                    "< %s", 
                    format( 
                        to[ testFrom ], 
                        digits = digits, 
                        nsmall = nsmall, 
                        ... 
                    ) 
                )    
            };  rm( testFrom ) 
            
            testTo <- to == min( c( from, to ) )
            
            if( any( testTo ) ){
                labs[ testTo ] <- sprintf( 
                    "< %s", 
                    format( 
                        from[ testTo ], 
                        digits = digits, 
                        nsmall = nsmall, 
                        ... 
                    ) 
                )    
            };  rm( testTo ) 
        }   
        
        if( hideMinMax[ 2L ] ){ # Case: The max value should be hidden
            testFrom <- from == max( c( from, to ) )
            if( any( testFrom ) ){
                labs[ testFrom ] <- sprintf( 
                    "> %s", 
                    format( 
                        to[ testFrom ], 
                        digits = digits, 
                        nsmall = nsmall, 
                        ... 
                    )   
                )    
            };  rm( testFrom ) 
            
            
            testTo <- to == max( c( from, to ) )
            if( any( testTo ) ){
                labs[ testTo ] <- sprintf( 
                    "> %s", 
                    format( 
                        from[ testTo ], 
                        digits = digits, 
                        nsmall = nsmall, 
                        ... 
                    )   
                )    
            };  rm( testTo ) 
        }   
        
        # if( any( testInf <- from == +Inf ) ){ 
            # labs[ testInf ] <- sprintf( 
                # "> %s", 
                # format( to[ testInf ], digits = digits, nsmall = nsmall, ... ) )       
        # }   
        
        # if( any( testInf <- from == -Inf ) ){ 
            # labs[ testInf ] <- sprintf( 
                # "< %s", 
                # format( to[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        # }   
        
        # if( any( testInf <- to == +Inf ) ){ 
            # labs[ testInf ] <- sprintf( 
                # "> %s", 
                # format( from[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        # }   
        
        # if( any( testInf <- to == -Inf ) ){ 
            # labs[ testInf ] <- sprintf( 
                # "< %s", 
                # format( from[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        # }   
        
        return( labs )
    }   
    
    if( is.null( labels ) ){ 
        convert[, "labels" ] <- .makeLabels( 
            from       = convert[, "from" ], 
            to         = convert[, "to" ], 
            hideMinMax = hideMinMax, 
            brackets   = brackets, 
            digits     = digits, 
            nsmall     = nsmall, 
            ... 
        )   
        
    }else{ 
        if( length( labels ) != (nBreaks - 1L) ){
            stop( sprintf(
                "length( labels ) should be equal to length(col) or length(fill) or length(breaks) - 1L (now %s)", 
                length( labels ) 
            ) ) 
        }   
        
        convert[, "labels" ] <- labels 
        rm( labels )
    }   
    
    #   Internal labels are necessary to account for the 
    #   that rounding may accidentally creates the same 
    #   labels for two different intervals
    convert[, "internalLabels" ] <- .makeLabels( 
        from       = convert[, "from" ], 
        to         = convert[, "to" ], 
        hideMinMax = c( FALSE, FALSE ), 
        brackets   = brackets, 
        digits     = 16, 
        nsmall     = 16, 
        ... 
    )   
    
    
    # convert2              <- convert 
    # convert2[, "groups" ] <- 1:nBreaks 
    
    
    # browser() 
    
    if( hasNA ){ 
        convert  <- rbind( convert,  NA ) 
        
        nConvert <- nrow( convert ) 
        convert[ nConvert, "col" ]            <- naCol 
        convert[ nConvert, "labels" ]         <- naLeg 
        convert[ nConvert, "internalLabels" ] <- naLeg 
        
        # convert[ nConvert, "groups" ]         <- nConvert 
        
        # if( int != 1 ){ 
            # attr( convert2, "legend" ) <- c( attr( convert2, "legend" ), 
                # naLeg ) 
        # }   
        
        rm( nConvert )
    }   
    
    
    if( sCol | sFill ){ 
        if( decreasing ){ 
            # out[[ "iCol" ]] <- out[[ "iFill" ]] <- rev( convert2[, "col" ] ) #  !isNA
            out[[ "iCol" ]] <- out[[ "iFill" ]] <- rev( convert[, "col" ] )
        }else{ 
            # out[[ "iCol" ]] <- out[[ "iFill" ]] <- convert2[, "col" ] #  !isNA
            out[[ "iCol" ]] <- out[[ "iFill" ]] <- convert[, "col" ] 
        }   
        
        out[[ "iBreaks" ]] <- sort( 
            unique( c( convert[, "from" ], convert[ nrow( convert ), "to" ] ) ), 
            na.last = TRUE, 
            decreasing = FALSE ) # This is not a mistake
        
        # mi_x <- c( x, out[[ "iBreaks" ]] )
        # ma_x <- max( mi_x[ is.finite( mi_x ) ], na.rm = TRUE )
        # mi_x <- min( mi_x[ is.finite( mi_x ) ], na.rm = TRUE ) 
        
        # out[[ "iBreaks" ]][ out[[ "iBreaks" ]] == -Inf ] <- -.Machine[[ "double.xmax" ]] 
        # out[[ "iBreaks" ]][ out[[ "iBreaks" ]] == +Inf ] <- +.Machine[[ "double.xmax" ]]
    }   
    
    
    if( sCol ){ 
        out[[ "col" ]] <- function(x){ 
            
            #   Find which rows are NA
            rowIsNa <- apply( 
                X      = convert[, c( "from", "to" ) ], 
                MARGIN = 1, # rows
                FUN    = function(x){ any( is.na( x ) ) } 
            )   
            
            #   Find the breaks and labels (and remove NA's)
            breaks <- c( 
                convert[ !rowIsNa, "from" ], 
                convert[ !rowIsNa, ][ nrow( convert[ !rowIsNa, ] ), "to" ] )
            lab    <- convert[ !rowIsNa, "internalLabels" ]
            
            if( decreasing ){
                breaks <- rev( breaks ) 
                lab    <- rev( lab ) 
            }   
            
            
            #   Test that no values in x is outside the 'breaks'
            #   bounds
            if( any( testHigh <- na.omit( x ) > max( breaks ) ) ){
                warning( sprintf( 
                    "Some values in x (%s) are higher than max(breaks). They may be displayed wrongly (or as NA)", 
                    sum( testHigh )
                ) ) 
            }   
            
            if( any( testLow <- na.omit( x ) < min( breaks ) ) ){
                warning( sprintf( 
                    "Some values in x (%s) are lower than min(breaks). They may be displayed wrongly (or as NA)", 
                    sum( testLow )
                ) ) 
            }   
            rm( testHigh, testLow )
            
            
            # #   Add NA's again # # Handled internally by cut()
            # if( any( rowIsNa ) ){
                # breaks <- c( breaks, convert[ rowIsNa, "from" ][ 1L ] )
                # lab    <- c( lab, convert[ rowIsNa, "internalLabels" ][ 1L ] )
            # }   
            
            
            #   Cast x into a data.frame
            x <- data.frame( 
                "values"         = x, 
                "id"             = 1:length(x), 
                "internalLabels" = as.character( cut( 
                    x      = x, 
                    breaks = breaks, 
                    labels = lab, 
                    right  = right, 
                    include.lowest = include.lowest 
                ) ), 
                stringsAsFactors = FALSE ) 
            
            rm( breaks, lab ) 
            
            #   Merge with the colours
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colours" ) ]
                by    = "internalLabels", 
                all.x = TRUE, 
                sort  = FALSE 
            )   
            
            x <- x[ order( x[, "id" ] ), ] 
            
            #   Add colour for missing values
            x[ is.na( x[, "values" ] ), "col" ] <- naCol 
            
            # if( any( is.na( x[, "col" ] ) ) ){ 
                # warning( "Some values in 'x' are new. Colour set to NA" ) 
            # }   
            
            return( as.character( x[, "col" ] ) ) 
        }   
        
        #   Give the function a clean environment
        environment( out[[ "col" ]] ) <- new.env() 
        
        #   Populate the environment
        assign( 
            x     = "convert", 
            value = convert[, c( "labels", "internalLabels", "col", 
                "from", "to" ) ], # , "groupLabels" , "groups"
            envir = environment( out[[ "col" ]] ) ) 
        
        assign( 
            x     = "right",          
            value = right,          
            envir = environment( out[[ "col" ]] ) ) 
        
        assign( 
            x     = "include.lowest", 
            value = include.lowest, 
            envir = environment( out[[ "col" ]] ) ) 
        
        assign( 
            x     = "naCol",          
            value = naCol,          
            envir = environment( out[[ "col" ]] ) ) 
        
        assign(   # Part of bug fix for colour order
            x     = "decreasing",          
            value = decreasing,          
            envir = environment( out[[ "col" ]] ) ) 
        
        #   Legend function
        formals( out[[ "legend" ]] )[[ "col" ]] <- convert[, "col" ] 
        
        #   Add the legend function
        formals( out[[ "legend" ]] )[[ "legend" ]] <- convert[, "labels" ] 
    }   
    
    
    if( sFill ){ 
        out[[ "fill" ]] <- function(x){ 
            
            #   Find which rows are NA
            rowIsNa <- apply( 
                X      = convert[, c( "from", "to" ) ], 
                MARGIN = 1, # rows
                FUN    = function(x){ any( is.na( x ) ) } 
            )   
            
            #   Find the breaks and labels (and remove NA's)
            breaks <- c( 
                convert[ !rowIsNa, "from" ], 
                convert[ !rowIsNa, ][ nrow( convert[ !rowIsNa, ] ), "to" ] )
            lab    <- convert[ !rowIsNa, "internalLabels" ]
            
            if( decreasing ){
                breaks <- rev( breaks ) 
                lab    <- rev( lab ) 
            }   
            
            
            #   Test that no values in x is outside the 'breaks'
            #   bounds
            if( any( testHigh <- na.omit( x ) > max( breaks ) ) ){
                warning( sprintf( 
                    "Some values in x (%s) are higher than max(breaks). They may be displayed wrongly (or as NA)", 
                    sum( testHigh )
                ) ) 
            }   
            
            if( any( testLow <- na.omit( x ) < min( breaks ) ) ){
                warning( sprintf( 
                    "Some values in x (%s) are lower than min(breaks). They may be displayed wrongly (or as NA)", 
                    sum( testLow )
                ) ) 
            }   
            rm( testHigh, testLow )
            
            
            # #   Add NA's again # # NO! cut handles NAs internally
            # if( any( rowIsNa ) ){
                # breaks <- c( breaks, convert[ rowIsNa, "from" ][ 1L ] )
                # lab    <- c( lab, convert[ rowIsNa, "internalLabels" ][ 1L ] )
            # }   
            
            #   Cast x into a data.frame
            x <- data.frame( 
                "values"         = x, 
                "id"             = 1:length(x), 
                "internalLabels" = as.character( cut( 
                    x      = x, 
                    breaks = breaks, 
                    labels = lab, 
                    right  = right, 
                    include.lowest = include.lowest ) ), 
                stringsAsFactors = FALSE ) 
            
            rm( breaks, lab ) 
            
            #   Merge with the colours
            x <- merge( 
                x     = x, 
                y     = convert, # [,c( "values", "colours" ) ]
                by    = "internalLabels", 
                all.x = TRUE, 
                sort  = FALSE 
            )   
            
            x <- x[ order( x[, "id" ] ), ] 
            
            #   Add colour for missing values
            x[ is.na( x[, "values" ] ), "col" ] <- naCol 
            
            # if( any( is.na( x[, "col" ] ) ) ){ 
                # warning( "Some values in 'x' are new. Colour set to NA" ) 
            # }   
            
            return( as.character( x[, "col" ] ) ) 
        }   
        
        #   Give the function a clean environment
        environment( out[[ "fill" ]] ) <- new.env() 
        
        #   Populate the environment
        assign( 
            x     = "convert", 
            value = convert[, c( "labels", "internalLabels", 
                "col", "from", "to" ) ], # , "groupLabels", "groups"
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign( 
            x     = "right",          
            value = right,          
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign( 
            x     = "include.lowest", 
            value = include.lowest, 
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign( 
            x     = "naCol",          
            value = naCol,          
            envir = environment( out[[ "fill" ]] ) ) 
        
        assign(   # Part of bug fix for colour order
            x     = "decreasing",          
            value = decreasing,          
            envir = environment( out[[ "fill" ]] ) ) 
        
        # if( hasNA ){ 
            # convert <- rbind( convert, NA ) 
            
            # nconvert <- nrow( convert ) 
            # convert[ nconvert, "col" ]    <- naCol 
            # convert[ nconvert, "labels" ] <- naLeg 
            
            # # convert[ nconvert, "groups" ] <- # No groups when no intermediate colours
                # # max( convert[, "groups" ], na.rm = TRUE ) + 1  
        # }   
        
        
        #   Prepare the legend function
        # if( decreasing ){ # Attempt to fix a bug. Not sure
            # formals( out[[ "legend" ]] )[[ "fill" ]] <- rev( convert2[, "col" ] ) 
        # }else{
            # formals( out[[ "legend" ]] )[[ "fill" ]] <- convert2[, "col" ] 
        # }   
        
        formals( out[[ "legend" ]] )[[ "fill" ]]   <- convert[, "col" ]
        formals( out[[ "legend" ]] )[[ "legend" ]] <- convert[, "labels" ] 
    }   
    
    formals( out[[ "legend" ]] )[[ "y.intersp" ]] <- y.intersp 
    
    out[[ "convert" ]]  <- convert
    
    return( out ) 
}   



#'@rdname setColourScale-methods
#'
#'@method setColourScale matrix
#'
#'@export 
#'
setColourScale.matrix <- function( 
 x, 
 ...
){  #   x dimentions
    dx <- dim( x )
    
    #   Matrix -> vector
    x <- as.vector( x ) 
    
    #   
    cs <- setColourScale(
        x     = x, 
        naCol = NA, 
        ...
    )   
    
    
    #   Find argument "decreasing"
    decreasing <- list(...)[[ "decreasing" ]]
    if( is.null( decreasing ) ){
        decreasing <- formals( setColourScale.default )[[ "decreasing" ]]
    }   
    
    
    #   New colour function
    if( "col" %in% names(cs) ){ 
        colOriginal <- cs[[ "col" ]] 
        
        cs[[ "col" ]] <- function(x){ 
            return( matrix( 
                data  = colOriginal( as.vector( x ) ), 
                nrow  = dx[1L], 
                ncol  = dx[2L], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( cs[[ "col" ]] ) <-  new.env() 
        assign( pos = environment( cs[[ "col" ]] ), x = "colOriginal", value = colOriginal,  ) 
        assign( pos = environment( cs[[ "col" ]] ), x = "dx",          value = dx ) 
        
        convert <- as.list( environment( colOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "from" ], decreasing = decreasing ), ] 
        
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
                nrow  = dx[1L], 
                ncol  = dx[2L], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( cs[[ "fill" ]] ) <-  new.env() 
        assign( pos = environment( cs[[ "fill" ]] ), x = "fillOriginal", value = fillOriginal,  ) 
        assign( pos = environment( cs[[ "fill" ]] ), x = "dx",           value = dx ) 
        
        convert <- as.list( environment( fillOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "from" ], decreasing = decreasing ), ] 
        
        isNotNA <- (!is.na( convert[, "from" ] )) & 
            (!is.na( convert[, "to" ] ))
        
        formals( cs[[ "legend" ]] )[[ "fill" ]][ !isNotNA ] <- 
            NA_character_ 
    }   
    
    
    return( cs )
}   



#'@rdname setColourScale-methods
#'
#'@method setColourScale RasterLayer
#'
#'@export 
#'
setColourScale.RasterLayer <- function( 
 x, 
 ...
){  
    if( !"raster" %in% rownames( installed.packages() ) ){ 
        stop( "setColourScale.RasterLayer requires the package 'raster' to be installed" ) 
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
    
    cs <- setColourScale(
        x = x, 
        ...
    )   
    
    
    #   New colour function
    if( "col" %in% names(cs) ){ 
        cs[[ "col" ]] <- function(x){ 
            stop( "Not implemented. Technically can't return a raster of colours (character strings)" )
        }   
    }   
    
    
    #   New fill function
    if( "fill" %in% names(cs) ){ 
        cs[[ "fill" ]] <- function(x){ 
            stop( "Not implemented. Technically can't return a raster of fill-colours (character strings)" )
        }   
    }   
    
    
    return( cs )
}   



# setColourRampScale =======================================

.addColorGradientLegend2 <- function( 
    l,          # output from legend, called a 1st time 
    fill,       # fill colours
    groups,     # fill groups
    #legend,    # labels for each break-point 
    border = "black" 
){  
    # browser()
    
    n       <- length( l$text$x ) 
    uGroups <- unique( groups ) 
    
    # browser() 
    
    if( length( uGroups ) != n ){ 
        stop( sprintf( 
            "Number of fill-groups (%s) should be equal to the number of original legend-labels (%s) (internal error)", 
            length( uGroups ), n 
        ) ) 
    }   
    
    
    if( length( groups ) != length( fill ) ){ 
        stop( "Length of 'fill' and 'groups' differ (internal error)" ) 
    }   
    
    
    # dx <- max( abs( diff(l$text$x) ) )
    dy <- max( abs( diff(l$text$y) ) ) 
    
    #   Find the width of the filled boxes
    fillWidth <- min(l$text$x) - l$rect$left 
    
    yy <- rep( NA_real_, n+1 )
    
    for( i in 1:n ){
        # if( i == 1 ){ 
            # ytop <- l$text$y[ i ] + dy/2 # + dy/4
        # }else{ 
            # ytop <- l$text$y[ i ] + dy/2
        # }   
        
        ytop <- l$text$y[ i ] + dy/2
        
        # if( i == n ){ 
            # ybottom <- l$text$y[i] - dy/2 # - dy/4
        # }else{ 
            # ybottom <- l$text$y[i] - dy/2
        # }   
        
        ybottom <- l$text$y[ i ] - dy/2
        
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
    # .addColorGradientLegend2( l = l, fill = fill, groups = rep( 1:n, each = 2 ) )



#' Prepare colour scale (ramp) and legend (fill) from continuous-numeric data.
#'
#' Prepare colour scale (ramp) and legend (fill) from 
#'  continuous-numeric data. \code{setColourScale} is a 
#'  partial rewriting of \code{\link{setColorScale}}.
#'
#'
#'@seealso \code{\link{setColourScale}}, 
#'  \code{\link{setFactorGraphics}} and 
#'  \code{\link[grDevices]{colorRampPalette}}.
#'
#'
#'@param x
#'  A vector of numerical values (continous).
#'
#'@param fill
#'  Logical value or vector of character strings representing 
#'  fill-colours. If \code{TRUE} or vector of colours, a 
#'  continuous fill-colour scale is defined for 'x'. There 
#'  must be as many fill-colours as \code{breaks}.
#'
#'@param breaks
#'  See \code{\link[base]{cut}}. There must be as many breaks 
#'  as colours (i.e. \code{length(fill) == length(breaks)}; 
#'  Notice that this is different from 
#'  \code{\link{setColourScale}}). If \code{NULL}, 
#'  \code{nBreaks} breaks will be generated internally (see 
#'  below). \code{breaks} should be ordered (no \code{NA}), 
#'  with no replica, and consistent with the argument 
#'  \code{decreasing}. See \code{naCol} and \code{naLeg} 
#'  regarding the handling of \code{NA}-values.
#'
#'@param int
#'  Single integer value. Number of colour intervals to be 
#'  defined in between the main fill-colours. \code{int} must 
#'  be \code{> 1}. Intermediate colours are generated with 
#'  \code{\link[grDevices]{colorRampPalette}}.
#'
#'@param brackets
#'  Vector of 3 characters. Used to generate customise 
#'  legend-labels, if \code{labels} is \code{NULL}. 
#'
#'@param labels
#'  See \code{\link[base]{cut}}.
#'
#'@param right
#'  See \code{\link[base]{cut}}. This parameter is important 
#'  as it will determine how \code{x}-values falling on a break 
#'  will be classified.
#'
#'@param include.lowest
#'  See \code{\link[base]{cut}}.
#'
#'@param digits
#'  See \code{\link[base]{format}}. Number formatting in the 
#'  legend.
#'
#'@param nsmall
#'  See \code{\link[base]{format}}. Number formatting in the 
#'  legend.
#'
#'@param naCol
#'  Single character string. Colour for \code{NA} (missing) 
#'  values. Notice that in the case of an 
#'  \code{\link[graphics]{image}} plot this parameter is 
#'  silently be ignored and replaced by \code{NA} as 
#'  the function displays missing values as transparent pixels.
#'  In the case of \code{raster::plot}, One should make sure 
#'  that the value of \code{naCol}-argument in 
#'  \code{setColourRampScale} is coherent with the value of 
#'  \code{naCol}-argument in \code{raster::plot}.
#'
#'@param naLeg
#'  Single character string. Legend (label) for \code{NA} 
#'  (missing) values.
#'
#'@param decreasing
#'  Logical value. If \code{TRUE} (default), the legend is 
#'  ordered (sorted) in decreasing order (i.e. low values on 
#'  the bottom of the colour scale and high values on the top 
#'  of the colour scale).
#'
#'@param y.intersp 
#'  Single numerical value. Character inter-spacing factor 
#'  for vertical (y) spacing of the colour legend. Passed to 
#'  \code{\link[graphics]{legend}}.
#'
#'@param nBreaks
#'  Single integer value. Number of breaks that must be 
#'  generated internally when \code{breaks} is not specified 
#'  (\code{NULL}). See argument \code{breaks} for details 
#'  on how \code{nBreaks} and \code{fill} are related.
#'
#'@param alpha
#'  Single logical value. Passed internally to 
#'  \code{\link[grDevices]{colorRampPalette}}
#'
#'@param hideMinMax
#'  Vector of two logical value. If \code{TRUE}, the min or 
#'  the max values in breaks (or both) will be replaced by 
#'  \code{> x} or \code{< y} where \code{x} is the 2nd highest 
#'  value in \code{breaks} and \code{y} is the 2nd smallest 
#'  value in \code{breaks}. \code{hideMinMax[1L]} is for the 
#'  min value and \code{hideMinMax[2L]} is for the max-value.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a list of 2 \code{\link[base]{function}}: 
#'  \code{$fill()}, a function that converts \code{x}-like values 
#'  into (fill)colours; \code{$legend()}, a function to draw a 
#'  legend on a plot with the correct (fill)colours and legend. 
#'  That function accepts \code{...} arguments, passed to the 
#'  original \code{\link[graphics]{legend}} function.
#'
#'
#'@example inst/examples/setColourRampScale-examples.R
#'
#'@importFrom grDevices colorRampPalette
#'
#'@rdname setColourRampScale-methods
#'
#'@export 
#'
setColourRampScale <- function(
 x, 
 ...
){  
    UseMethod( "setColourRampScale" )
}   


#'@rdname setColourRampScale-methods
#'
#'@method setColourRampScale default
#'
#'@export 
#'
setColourRampScale.default <- function( 
    x, 
    # col    = FALSE, 
    fill     = NULL, 
    breaks   = NULL, 
    int      = 4L, 
    naCol    = "lightgray", 
    naLeg    = "na", 
    brackets = c( "", " ", "" ), 
    labels   = NULL, 
    right    = FALSE,       # see cut()
    include.lowest = TRUE,  # see cut()
    digits   = 3L,          # see format()
    nsmall   = 3L,          # see format()
    decreasing = TRUE, 
    y.intersp = 1.5, 
    nBreaks = 5L, 
    alpha = FALSE, 
    hideMinMax = c( FALSE, FALSE ), 
    ...      # passed to format
){  
    #   Reset 'nBreak's if 'breaks' is supplied and check that 
    #   there are enough breaks
    if( !is.null( breaks ) ){
        # test that there are no NA in breaks
        if( any( is.na( breaks ) ) ){
            stop( "NA not allowed in breaks. Use arguments 'naLeg' and 'naCol' instead" )
        }   
        
        if( length( breaks ) < 2L ){
            stop( sprintf( 
                "When non-NULL, length(breaks) must be at least 2 (now %s)", 
                length(breaks)
            ) ) 
        }   
        
        # if( length( breaks[ is.finite( breaks ) ] ) < 1L ){
            # stop( sprintf( 
                # "The number of finite breaks must be at least 1 (now %s)", 
                # length( breaks[ is.finite( breaks ) ] ) 
            # ) ) 
        # }   
        
        nBreaks <- length( breaks ) 
    }else{
        if( nBreaks < 2 ){
            stop( sprintf( 
                "nBreaks < 2. nBreaks must be at least 2 (now %s)", 
                nBreaks
            ) ) 
        }   
        
    }   
    
    
    #   Case: a fill-legend should be generated
    #       and no colour are specified
    #       Generating a 5-colour range
    if( is.null( fill ) ){ 
        if( decreasing ){
            fill <- hsv( 
                h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
                s = seq( .8,  .20, length.out = nBreaks ), 
                v = seq( .3,  .95, length.out = nBreaks ) )
                #        dark light 
        }else{
            fill <- hsv( 
                h = 0.55, # 0.04 # 0.21 # 0.38 # 0.55
                s = seq( .20,  .8, length.out = nBreaks ), 
                v = seq( .95,  .3, length.out = nBreaks ) )
                #        light dark
        }   
        
    }   
    
    if( length(fill) != nBreaks ){
        stop( sprintf(
            "length(fill) must be equal to nBreaks or length(breaks). Now respectively %s, %s and %s", 
            length(fill), nBreaks, length(breaks) 
        ) ) 
    }   
    
    
    #   Check and sanitise 'int'
    int <- as.integer( round( int, 0 ) ) 
    int <- ifelse( int[1] < 1L, 1L, int[1] )
    
    if( (int%%2 == 1) & (int != 1) ){ 
        stop( "'int' must be an odd integer or 1" )
    }   
    
    
    # #   Find out how many colours were provided
    # n <- ifelse( sCol, length(col), length(fill) ) 
    
    
    #   Has x any NA values?
    hasNA <- any( is.na( x ) ) | any( is.nan( x ) ) 
    
    
    #   Define the breaks in x values
    if( is.null( breaks ) ){
        if( length( x ) == 0 ){
            stop( "length(x) (or number of values in 'x') is 0" ) 
        }   
        
        #   Unique x values:
        u_x <- unique( x ) 
        u_x <- u_x[ !is.na( u_x ) ] 
        u_x <- u_x[ !is.nan( u_x ) ] 
        u_x <- u_x[ !is.infinite( u_x ) ] 
        
        #   Handle single unique values
        if( length( u_x ) == 0 ){
            stop( "All values in 'x' are NA, NaN or infinite" ) 
            
        }else if( length( u_x ) == 1L ){
            if( u_x == 0 ){
                u_x <- c( -1e-3, u_x, 1e-3 )
                
            }else{
                u_x <- c( u_x - (u_x/1000), u_x, u_x + (u_x/1000) )
                
            }   
        }   
        
        if( decreasing ){ 
            breaks <- seq( 
                from = max( u_x, na.rm = hasNA ), 
                to   = min( u_x, na.rm = hasNA ), 
                length.out = nBreaks ) 
        }else{ 
            breaks <- seq( 
                from = min( u_x, na.rm = hasNA ), 
                to   = max( u_x, na.rm = hasNA ), 
                length.out = nBreaks ) 
        }   
        
        # breaksSanitized <- breaks
    }else{ 
        # Test that the breaks are sorted correctly
        test <- sort( breaks, na.last = TRUE, decreasing = decreasing )
        #   Note: na.last not needed in principle
        
        test <- any( test != breaks ) | 
                (length( unique( breaks ) ) != length( breaks )) 
        
        if( test ){ 
            stop( "'breaks' should be ordered, with no replica, and consistent with argument 'decreasing'" ) 
        }   
        rm( test )
        
        
        if( any( is.infinite( breaks ) ) ){
            stop( "Some values in breaks are infinite. Replace them by a high or low value and set 'hideMinMax' instead." )
        }   
        
        
        # #   breaks2 is a copy of breaks where infinite values
        # #   will be removed. Usefull for estimating intermediate
        # #   steps in the breaks
        # breaksSanitized <- breaks
        
        # #   Test for infinite values
        # if( any( is.infinite( breaks ) ) ){
            # #   Calculate the maximum non-infinite difference 
            # #   between the breaks (absolute value)
            # maxAbsDiff <- abs( diff( breaks ) ) 
            # maxAbsDiff <- maxAbsDiff[ is.finite( maxAbsDiff ) ] 
            # if( length( maxAbsDiff ) == 0 ){
                # #   Case: less than 2 finite breaks
                # maxAbsDiff <- 1
            # }else{
                # #   Case: at least 2 finite breaks
                # maxAbsDiff <- max( maxAbsDiff )
            # }   
            
            # if( decreasing ){
                # if( breaks[ 1L ] == +Inf ){
                    # # breaksSanitized[ 1L ] <- max( breaks[ is.finite( breaks ) ] ) + maxAbsDiff 
                    # breaksSanitized[ 1L ] <- +.Machine[[ "double.xmax" ]] 
                # }   
                
                # if( breaks[ length( breaks ) ] == -Inf ){
                    # # breaksSanitized[ length( breaks ) ] <- min( breaks[ is.finite( breaks ) ] ) - maxAbsDiff 
                    # breaksSanitized[ length( breaks ) ] <- -.Machine[[ "double.xmax" ]]
                # }   
            # }else{ # increasing
                # if( breaks[ 1L ] == -Inf ){
                    # # breaksSanitized[ 1L ] <- min( breaks[ is.finite( breaks ) ] ) - maxAbsDiff 
                    # breaksSanitized[ 1L ] <- -.Machine[[ "double.xmax" ]] 
                # }   
                
                # if( breaks[ length( breaks ) ] == +Inf ){
                    # # breaksSanitized[ length( breaks ) ] <- max( breaks[ is.finite( breaks ) ] ) + maxAbsDiff 
                    # breaksSanitized[ length( breaks ) ] <- +.Machine[[ "double.xmax" ]] 
                # }   
            # }   
        # }   
        
    }   
    
    
    #   Output list
    out <- list( 
        "legend" = function( 
            # Arguments that exists in legend()
            x, 
            y           = NULL, 
            legend, 
            col         = NULL, 
            fill        = NULL, 
            border      = "black", 
            cex         = 1, 
            text.col    = par( "col" ), 
            text.font   = NULL, 
            title.col   = par( "col" ), 
            horiz       = FALSE, 
            title       = NULL, 
            y.intersp   = y.intersp, 
            ..., 
            
            #   Arguments that do not exists in legend() (extra)
            groups      = NULL   
            # style     = 1L    # style == 1L means no intermediate colours. 
                                # style != 1L means with intermediate colours
            
        ){  
            # if( !exists( "style" ) ){ style <- 1L }
            
            if( !is.null( col ) ){
                stop( "$legend output-function of 'setColourRampScale' does not accept a 'col' argument (as 'fill' is set)" )
            }   
            
            if( horiz ){ stop( "'horiz' = TRUE not supported in easylegend::setColourRampScale" ) }
            
            #   Prepare a list (of arguments) that will be passed 
            #   to the function legend using do.call()
            arguments <- list( 
                "x"         = x, 
                "y"         = y, 
                "title.col" = title.col, 
                "title"     = title, 
                "y.intersp" = y.intersp ) 
                
            transpLeg <- legend[ order( nchar( legend ) ) ] 
            transpLeg <- transpLeg[ -1 ]
            
            arguments <- c( arguments, list( 
                "fill"     = "transparent", 
                "border"   = "transparent", 
                "legend"   = transpLeg, 
                "text.col" = "transparent" ) ) 
            
            arguments <- c( arguments, list(...) )
            rm( transpLeg ) 
            
            lRes <- do.call( 
                what = get( "legend", pos = "package:graphics" ), 
                args = arguments ) 
            
            yy <- .addColorGradientLegend2( 
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
            
            return( invisible( arguments ) ) 
        }   
    )    
    
    environment( out[[ "legend" ]] ) <- new.env() 
    
    class( out ) <- "numericGraphics"
    
    
    # breaks <- c(Inf, 1, 0, -1, -Inf); decreasing <- TRUE  
    # breaks <- c(-Inf, -1, 0, 1, Inf); decreasing <- FALSE  
    
    
    # #   Function to set the midpoint values for cases with infinite from or to
    # .sanitiseMid <- function( mid, from, to ){ 
        # browser()
        
        # dff <- diff( c( from, to[ length( to ) ] ) )/2
        
        # testInf <- from == +Inf
        # mid[ testInf ] <- to[ which( testInf ) ] - dff[ which( testInf ) + 1L ]
        # #   By definition from == +Inf implied that decreasing is TRUE
        
        # testInf <- from == -Inf
        # mid[ testInf ] <- to[ which( testInf ) ] - dff[ which( testInf ) + 1L ]
        # #   By definition from == -Inf implied that decreasing is FALSE
        
        # testInf <- to == -Inf
        # mid[ testInf ] <- from[ which( testInf ) ] + dff[ which( testInf ) - 1L ]
        # #   By definition to == -Inf implied that decreasing is TRUE
        
        # testInf <- to == +Inf
        # mid[ testInf ] <- from[ which( testInf ) ] + dff[ which( testInf ) - 1L ]
        # #   By definition to == +Inf implied that decreasing is FALSE
        
        # return( mid )
    # }   
    
    
    # #   Find out the mid-points between breaks and the 
    # #   new breakpoints (from and to)
    # mid <- breaksSanitized[ 1:(length(breaksSanitized)-1) ] + (diff( breaksSanitized )/2) # ifelse( decreasing, +1, -1 )
    
    # from <- c( breaks[ 1L ], mid )
    # to <- c( mid, breaks[ nBreaks ] ) 
    
    # # fromSanit <- c( breaksSanitized[ 1L ], mid )
    # # toSanit   <- c( mid, breaksSanitized[ nBreaks ] ) 
    
    # convert <- data.frame( 
        # "from"           = from, 
        # "to"             = to, 
        # # "fromSanit"      = fromSanit, 
        # # "toSanit"        = toSanit, 
        # "col"            = fill, 
        # "labels"         = NA_character_, 
        # "internalLabels" = NA_character_, 
        # stringsAsFactors = FALSE 
    # )   
    
    # # browser()
    
    # rm( breaks, from, to, mid ) # , fromSanit, toSanit
    
    
    #   Prepare the colour-class labels (including replacing 
    #   infinite with > 2nd last break or lower than 2nd break)
    .makeLabels <- function( 
        from, 
        to, 
        hideMinMax = hideMinMax, 
        brackets   = brackets, 
        digits     = digits, 
        nsmall     = nsmall, 
        ... 
    ){  
        labs <- paste0( 
            brackets[1], 
            format( from, digits = digits, nsmall = nsmall, ... ), 
            brackets[2], 
            format( to,   digits = digits, nsmall = nsmall, ... ), 
            brackets[3] ) 
        
        if( hideMinMax[ 1L ] ){ # Case: The min value should be hidden
            
            testFrom <- from == min( c( from, to ) )
            
            if( any( testFrom ) ){
                labs[ testFrom ] <- sprintf( 
                    "< %s", 
                    format( 
                        to[ testFrom ], 
                        digits = digits, 
                        nsmall = nsmall, 
                        ... 
                    ) 
                )    
            };  rm( testFrom ) 
            
            testTo <- to == min( c( from, to ) )
            
            if( any( testTo ) ){
                labs[ testTo ] <- sprintf( 
                    "< %s", 
                    format( 
                        from[ testTo ], 
                        digits = digits, 
                        nsmall = nsmall, 
                        ... 
                    ) 
                )    
            };  rm( testTo ) 
        }   
        
        if( hideMinMax[ 2L ] ){ # Case: The max value should be hidden
            testFrom <- from == max( c( from, to ) )
            if( any( testFrom ) ){
                labs[ testFrom ] <- sprintf( 
                    "> %s", 
                    format( 
                        to[ testFrom ], 
                        digits = digits, 
                        nsmall = nsmall, 
                        ... 
                    )   
                )    
            };  rm( testFrom ) 
            
            
            testTo <- to == max( c( from, to ) )
            if( any( testTo ) ){
                labs[ testTo ] <- sprintf( 
                    "> %s", 
                    format( 
                        from[ testTo ], 
                        digits = digits, 
                        nsmall = nsmall, 
                        ... 
                    )   
                )    
            };  rm( testTo ) 
        }   
        
        # if( any( testInf <- from == +Inf ) ){ 
            # labs[ testInf ] <- sprintf( 
                # "> %s", 
                # format( to[ testInf ], digits = digits, nsmall = nsmall, ... ) )       
        # }   
        
        # if( any( testInf <- from == -Inf ) ){ 
            # labs[ testInf ] <- sprintf( 
                # "< %s", 
                # format( to[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        # }   
        
        # if( any( testInf <- to == +Inf ) ){ 
            # labs[ testInf ] <- sprintf( 
                # "> %s", 
                # format( from[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        # }   
        
        # if( any( testInf <- to == -Inf ) ){ 
            # labs[ testInf ] <- sprintf( 
                # "< %s", 
                # format( from[ testInf ], digits = digits, nsmall = nsmall, ... ) )    
        # }   
        
        return( labs )
    }   
    
    # if( is.null( labels ) ){ 
        # convert[, "labels" ] <- .makeLabels( 
            # from     = convert[, "from" ], 
            # to       = convert[, "to" ], 
            # brackets = brackets, 
            # digits   = digits, 
            # nsmall   = nsmall, 
            # ... 
        # )   
        
    # }else{ 
        # convert[, "labels" ] <- labels 
        # rm( labels )
    # }   
    
    # #   Internal labels are necessary to account for the 
    # #   that rounding may accidentally creates the same 
    # #   labels for two different intervals
    # convert[, "internalLabels" ] <- .makeLabels( 
        # from     = convert[, "from" ], 
        # to       = convert[, "to" ], 
        # brackets = brackets, 
        # digits   = 16, 
        # nsmall   = 16, 
        # ... 
    # )   
    
    # nBreaks <- nrow( convert ) 
    
    # nConvert2 <- ((nBreaks - 1L) * int) + 1L
    
    # convert2 <- data.frame( 
        # "from"           = rep( NA_real_, nConvert2 ), 
        # "to"             = rep( NA_real_, nConvert2 ), 
        # # "fromSanit"      = rep( NA_real_, nConvert2 ), 
        # # "toSanit"        = rep( NA_real_, nConvert2 ), 
        # # "mid"          = rep( NA_real_, nConvert2 ), 
        # "col"            = rep( NA_character_, nConvert2 ), 
        # "labels"         = rep( NA_character_, nConvert2 ), 
        # "internalLabels" = rep( NA_character_, nConvert2 ), 
        # # "groups"       = as.integer( rep( 1:nBreaks, each = int ) ) , 
        # "groups"         = round( seq( 
            # from         = 1, 
            # to           = nBreaks, 
            # length.out   = nConvert2 ), 0 ), 
        # stringsAsFactors = FALSE 
    # )           
    
    # #   Interpolate colours, adding intermediate colours
    # cr  <- grDevices::colorRampPalette( 
        # colors = convert[, "col" ], 
        # bias   = 1, 
        # space  = "Lab", 
        # alpha  = alpha ) 
    # convert2[, "col" ]  <- cr( nConvert2 ) 
    
    convertTemplates <- data.frame( 
        "from"           = rep( NA_real_, int ), 
        "to"             = rep( NA_real_, int ), 
        "col"            = rep( NA_character_, int ), 
        "labels"         = rep( NA_character_, int ), 
        "internalLabels" = rep( NA_character_, int ), 
        "groups"         = rep( NA_integer_, int ), 
        stringsAsFactors = FALSE 
    )           
    
    convert <- do.call( what = "rbind", args = lapply( 
        X   = 1:(nBreaks - 1L), 
        FUN = function(i){
            out <- convertTemplates
            
            if( i == (nBreaks - 1L) ){  # Last group
                # Add one row to the output
                out <- rbind( out, out[ 1L, ] ) 
                
                #   Define the breaks in the group
                #   One more breaks than for the other groups
                newBreaks <- seq( 
                    from       = breaks[ i ],       # breaksSanitized[ i ], 
                    to         = breaks[ i + 1L ],  # breaksSanitized[ i + 1L ], 
                    length.out = nrow(out)+1L ) 
                
                #   Define the from-to ranges for each step 
                #   in the group
                out[, "from" ] <- newBreaks[ 1:(length(newBreaks)-1L) ] 
                out[, "to" ]   <- newBreaks[ 2:length(newBreaks) ] 
                
                cl <- grDevices::colorRampPalette( 
                    colors = fill[ c( i, i + 1L ) ], 
                    bias   = 1, 
                    space  = "Lab", 
                    alpha  = alpha ) 
                
                #   Generate one colours
                out[, "col" ] <- cl( nrow( out ) ) 
                
            }else{                      # Not the last group
                #   Define the breaks in the group
                newBreaks <- seq( 
                    from       = breaks[ i ],       # breaksSanitized[ i ], 
                    to         = breaks[ i + 1L ],  # breaksSanitized[ i + 1L ], 
                    length.out = nrow(out)+1L ) 
                
                #   Define the from-to ranges for each step 
                #   in the group
                out[, "from" ] <- newBreaks[ 1:(length(newBreaks)-1L) ] 
                out[, "to" ]   <- newBreaks[ 2:length(newBreaks) ] 
                
                cl <- grDevices::colorRampPalette( 
                    colors = fill[ c( i, i + 1L ) ], 
                    bias   = 1, 
                    space  = "Lab", 
                    alpha  = alpha ) 
                
                #   Generate one colour too much and discard 
                #   it (to avoid having twice the same colour 
                #   in different groups, on the boundary class)
                out[, "col" ] <- cl( nrow( out ) + 1L )[ 1:nrow( out ) ] 
            }   
            
            out[, "groups" ] <- i 
            
            return( out )
        }   
    ) ) 
    
    #   Only keeps unique values. Just in case 
    # convert2 <- unique( convert2 ) 
    
    rm( convertTemplates ) 
    
    nConvert <- nrow( convert ) 
    
    #   Order the values, in case the do.call-lapply 
    #   code above would mess the order
    convert <- convert[ order( convert[, "from" ], 
        decreasing = decreasing ), ]
    
    
    #   Add a pretty label will be necessary when 
    #   creating the fill function that will convert 
    #   numeric values into fill-colours (with gradients)
    convert[, "labels" ] <- .makeLabels( 
        from       = convert[, "from" ], 
        to         = convert[, "to" ], 
        hideMinMax = hideMinMax, 
        brackets   = brackets, 
        digits     = digits, 
        nsmall     = nsmall, 
        ... 
    )   
    
    #   Internal labels are necessary to account for the 
    #   that rounding may accidentally creates the same 
    #   labels for two different intervals
    convert[, "internalLabels" ] <- .makeLabels( 
        from       = convert[, "from" ], 
        to         = convert[, "to" ], 
        hideMinMax = c( FALSE, FALSE ), 
        brackets   = brackets, 
        digits     = 16, # See print.default() to see why this is relevant
        nsmall     = 16, 
        ... 
    )   
    
    #   Make a new legend for each break point
    # attr( convert, "legend" ) <- c( convert[, "from" ], 
        # convert[ nBreaks, "to" ] ) 
    
    attr( convert, "legend" ) <- breaks 
    #   Note: eventual NAs will be added below
    
    # isInfLeg <- is.infinite( attr( convert, "legend" ) ) 
    
    attr( convert, "legend" ) <- format( attr( convert, "legend" ), 
        digits = digits, nsmall = nsmall, ... ) 
    
    #   Hide min or max value?
    if( hideMinMax[ 1L ] ){
        testMin <- breaks == min( breaks )
        attr( convert, "legend" )[ which( testMin ) ] <- "" 
        rm( testMin ) 
    }   
    
    if( hideMinMax[ 2L ] ){
        testMax <- breaks == max( breaks )
        attr( convert, "legend" )[ which( testMax ) ] <- "" 
        rm( testMax )
    }   
    
    # # TO DO: Can this be improved??
    # attr( convert, "legend" )[ isInfLeg ] <- ""
    
    # rm( isInfLeg ) 
    
    
    if( decreasing ){ 
        out[[ "iCol" ]] <- out[[ "iFill" ]] <- rev( convert[, "col" ] ) #  !isNA
    }else{ 
        out[[ "iCol" ]] <- out[[ "iFill" ]] <- convert[, "col" ] #  !isNA
    }   
    
    out[[ "iBreaks" ]] <- sort( 
        unique( c( convert[, "from" ], convert[ nrow( convert ), "to" ] ) ), 
        na.last = TRUE,      # Useless?
        decreasing = FALSE ) # This is not a mistake
    
    
    # #   It is necessary to recalculate the breaks so that there 
    # #   is one more breaks than colour (for image() and raster 
    # #   plots)
    
    # #   Differences between breaks
    # bDiff <- diff( out[[ "iBreaks" ]] ) 
    
    # #   Handle the case of infinite values
    # bDiff[ bDiff == +Inf ] <- max( abs( bDiff[ is.finite( bDiff ) ] ) ) 
    # bDiff[ bDiff == -Inf ] <- -max( abs( bDiff[ is.finite( bDiff ) ] ) ) 
    
    # #   New, intermediate breaks (excluding first and last)
    # newBreaks <- 
        # out[[ "iBreaks" ]][ 1:(length(out[[ "iBreaks" ]]) - 1L) ] + 
        # bDiff 
    
    # #   Add first and last breaks
    # newBreaks <- c( 
        # out[[ "iBreaks" ]][ 1L ] - max( bDiff ), 
        # newBreaks, 
        # out[[ "iBreaks" ]][ length( out[[ "iBreaks" ]] ) ] + max( bDiff ) )
    
    # #   Set the new breaks
    # out[[ "iBreaks" ]] <- newBreaks
    # rm( newBreaks, bDiff )
    
    
    if( hasNA ){ 
        convert  <- rbind( convert,  NA ) 
        
        nConvert <- nrow( convert ) 
        convert[ nConvert, "col" ]    <- naCol 
        convert[ nConvert, "labels" ] <- naLeg 
        # convert[ nConvert, "internalLabels" ] <- naLeg # NO!
        convert[ nConvert, "groups" ] <- 
            max( convert[, "groups" ], na.rm = TRUE ) + 1L
        
        attr( convert, "legend" ) <- c( attr( convert, "legend" ), 
            naLeg ) 
        
        # rm( nConvert )
    }   
    
    
    # mi_x <- c( x, out[[ "iBreaks" ]] )
    # ma_x <- max( mi_x[ is.finite( mi_x ) ], na.rm = TRUE )
    # mi_x <- min( mi_x[ is.finite( mi_x ) ], na.rm = TRUE ) 
    
    # out[[ "iBreaks" ]][ out[[ "iBreaks" ]] == -Inf ] <- -.Machine[[ "double.xmax" ]]
    # out[[ "iBreaks" ]][ out[[ "iBreaks" ]] == +Inf ] <- +.Machine[[ "double.xmax" ]]
    
    
    
    out[[ "fill" ]] <- function(x){ 
        #   Find which rows are NA
        rowIsNa <- apply( 
            X      = convert[, c( "from", "to" ) ], 
            MARGIN = 1, # rows
            FUN    = function(x){ any( is.na( x ) ) } 
        )   
        
        #   Find the breaks and labels (and remove NA's)
        breaks <- c( 
            convert[ !rowIsNa, "from" ], 
            convert[ !rowIsNa, ][ nrow( convert[ !rowIsNa, ] ), "to" ] )
        lab    <- convert[ !rowIsNa, "internalLabels" ]
        
        if( decreasing ){
            breaks <- rev( breaks ) 
            lab    <- rev( lab ) 
        }   
        
        
        #   Test that no values in x is outside the 'breaks'
        #   bounds
        if( any( testHigh <- (na.omit( x ) > max( breaks )) ) ){
            warning( sprintf( 
                "Some values in x (%s) are higher than max(breaks). They may be displayed wrongly (or as NA)", 
                sum( testHigh )
            ) ) 
        }   
        
        if( any( testLow <- (na.omit( x ) < min( breaks )) ) ){
            warning( sprintf( 
                "Some values in x (%s) are lower than min(breaks). They may be displayed wrongly (or as NA)", 
                sum( testLow )
            ) ) 
        }   
        rm( testHigh, testLow )
        
        
        # #   Add NA's again # # NO! NA are handled by cut()
        # if( any( rowIsNa ) ){
            # breaks <- c( breaks, convert[ rowIsNa, "from" ][ 1L ] )
            # lab    <- c( lab, convert[ rowIsNa, "internalLabels" ][ 1L ] )
        # }   
        
        #   Cast x into a data.frame
        x <- data.frame( 
            "values"         = x, 
            "id"             = 1:length(x), 
            "internalLabels" = as.character( cut( 
                x      = x, 
                breaks = breaks, 
                labels = lab, # This is what makes sure the classes are matching
                right  = right, 
                include.lowest = include.lowest ) ), 
            stringsAsFactors = FALSE ) 
        
        rm( breaks, lab ) 
        
        #   Merge with the colours
        x <- merge( 
            x     = x, 
            y     = convert, # [,c( "values", "colours" ) ]
            by    = "internalLabels", 
            all.x = TRUE, 
            sort  = FALSE 
        )   
        
        x <- x[ order( x[, "id" ] ), ] 
        
        #   Add colour for missing values
        x[ is.na( x[, "values" ] ), "col" ] <- naCol 
        
        # if( any( is.na( x[, "col" ] ) ) ){ 
            # warning( "Some values in 'x' are new. Colour set to NA" ) 
        # }   
        
        return( as.character( x[, "col" ] ) ) 
    }   
    
    #   Give the function a clean environment
    environment( out[[ "fill" ]] ) <- new.env() 
    
    #   Populate the environment
    assign( 
        x     = "convert", 
        value = convert[, c( "labels", "internalLabels", 
            "col", "from", "to", "groups" ) ], 
        envir = environment( out[[ "fill" ]] ) ) 
    
    assign( 
        x     = "right",          
        value = right,          
        envir = environment( out[[ "fill" ]] ) ) 
    
    assign( 
        x     = "include.lowest", 
        value = include.lowest, 
        envir = environment( out[[ "fill" ]] ) ) 
    
    assign( 
        x     = "naCol",          
        value = naCol,          
        envir = environment( out[[ "fill" ]] ) ) 
    
    assign(   # Part of bug fix for colour order
        x     = "decreasing",          
        value = decreasing,          
        envir = environment( out[[ "fill" ]] ) ) 
    
    
    # if( hasNA ){ 
        # convert <- rbind( convert, NA ) 
        
        # nconvert <- nrow( convert ) 
        # convert[ nconvert, "col" ]    <- naCol 
        # # convert2[ nconvert2, "labels" ] <- naLeg 
        
        # convert[ nconvert, "groups" ] <- 
            # max( convert[, "groups" ], na.rm = TRUE ) + 1  
    # }   
    
    
    #   Prepare the legend function
    # if( decreasing ){ # Attempt to fix a bug. Not sure
        # formals( out[[ "legend" ]] )[[ "fill" ]] <- rev( convert2[, "col" ] ) 
    # }else{
        # formals( out[[ "legend" ]] )[[ "fill" ]] <- convert2[, "col" ] 
    # }   
    
    formals( out[[ "legend" ]] )[[ "fill" ]]      <- convert[, "col" ]
    formals( out[[ "legend" ]] )[[ "groups" ]]    <- convert[, "groups" ] 
    formals( out[[ "legend" ]] )[[ "legend" ]]    <- attr( convert, "legend" )    
    formals( out[[ "legend" ]] )[[ "y.intersp" ]] <- y.intersp 
    
    out[[ "convert" ]]  <- convert
    
    return( out ) 
}   



#'@rdname setColourRampScale-methods
#'
#'@method setColourRampScale matrix
#'
#'@export 
#'
setColourRampScale.matrix <- function( 
 x, 
 ...
){  #   x dimentions
    dx <- dim( x )
    
    #   Matrix -> vector
    x <- as.vector( x ) 
    
    #   
    cs <- setColourRampScale(
        x     = x, 
        naCol = NA, 
        ...
    )   
    
    
    #   Find argument "decreasing"
    decreasing <- list(...)[[ "decreasing" ]]
    if( is.null( decreasing ) ){
        decreasing <- formals( setColourRampScale.default )[[ "decreasing" ]]
    }   
    
    
    #   New colour function
    if( "col" %in% names(cs) ){ 
        colOriginal <- cs[[ "col" ]] 
        
        cs[[ "col" ]] <- function(x){ 
            return( matrix( 
                data  = colOriginal( as.vector( x ) ), 
                nrow  = dx[1L], 
                ncol  = dx[2L], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( cs[[ "col" ]] ) <-  new.env() 
        assign( pos = environment( cs[[ "col" ]] ), x = "colOriginal", value = colOriginal,  ) 
        assign( pos = environment( cs[[ "col" ]] ), x = "dx",          value = dx ) 
        
        convert <- as.list( environment( colOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "from" ], decreasing = decreasing ), ] 
        
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
                nrow  = dx[1L], 
                ncol  = dx[2L], 
                byrow = FALSE 
            ) ) 
        }   
        
        #   Clean and populate the function's environment
        environment( cs[[ "fill" ]] ) <-  new.env() 
        assign( pos = environment( cs[[ "fill" ]] ), x = "fillOriginal", value = fillOriginal,  ) 
        assign( pos = environment( cs[[ "fill" ]] ), x = "dx",           value = dx ) 
        
        convert <- as.list( environment( fillOriginal ) )[[ "convert" ]]
        convert <- convert[ order( convert[, "from" ], decreasing = decreasing ), ] 
        
        isNotNA <- (!is.na( convert[, "from" ] )) & 
            (!is.na( convert[, "to" ] ))
        
        formals( cs[[ "legend" ]] )[[ "fill" ]][ !isNotNA ] <- 
            NA_character_ 
    }   
    
    
    return( cs )
}   



#'@rdname setColourRampScale-methods
#'
#'@method setColourRampScale RasterLayer
#'
#'@export 
#'
setColourRampScale.RasterLayer <- function( 
 x, 
 ...
){  
    if( !"raster" %in% rownames( installed.packages() ) ){ 
        stop( "setColourRampScale.RasterLayer requires the package 'raster' to be installed" ) 
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
    
    cs <- setColourRampScale(
        x = x, 
        ...
    )   # Note regarding colNA: see ?raster::plot
    
    
    #   New colour function
    if( "col" %in% names(cs) ){ 
        cs[[ "col" ]] <- function(x){ 
            stop( "Not implemented. Technically can't return a raster of colours (character strings)" )
        }   
    }   
    
    
    #   New fill function
    if( "fill" %in% names(cs) ){ 
        cs[[ "fill" ]] <- function(x){ 
            stop( "Not implemented. Technically can't return a raster of fill-colours (character strings)" )
        }   
    }   
    
    
    return( cs )
}   





