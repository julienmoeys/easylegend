library( "easylegend" ) 


# Example: x numerical values, points =======================
x <- sample( x = round(rnorm(10),2), size = 20, replace = TRUE )

asCol( x ) 
asCol( x, export = TRUE ) 
