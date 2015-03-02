library( "easylegend" ) 


# Example: x numerical values, points =======================
x <- sample( x = round(rnorm(10),2), size = 20, replace = TRUE )

asPch( x ) 
asPch( x, export = TRUE ) 
