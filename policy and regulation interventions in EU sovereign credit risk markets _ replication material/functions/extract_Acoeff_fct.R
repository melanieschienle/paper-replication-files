extract_Acoeff_fct <- function ( varmod, p ){
  
    ctry    <- varmod$K
                   
    coeffs  <- coef( varmod )
    
    A       <- matrix(0,ctry,ctry)
    for( k in 1:ctry ){
      cc        <- coeffs[[k]][,1]
      pp        <- coeffs[[k]][,4]
      index     <- which( pp > p )
      cc[index] <- 0 
    
      A[k,]     <- cc
    }
  
    return( A ) 
}
