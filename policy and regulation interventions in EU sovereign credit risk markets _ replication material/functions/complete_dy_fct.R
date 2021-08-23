complete_dy_fct <- function (A, sigma, H) {
  
  ctry      <- dim(A)[1]
  
  T1Hij     <- matrix(0, ctry, ctry )
  T2Hij     <- matrix(0, ctry, ctry )
  sij       <- matrix(0, ctry, ctry )
      
  for( l in 0:(H-1) ){
    T1Hij   <- T1Hij + ( A%^%l %*% sigma ) * ( A%^%l %*% sigma )  # the * product means elementwise multiplication
    T2Hij   <- T2Hij + ( A%^%l %*% sigma %*% t(A%^%l) )
  }    
  
  for( i in 1:ctry ){         
    for( j in 1:ctry ) {     
      sij[i, j] <- 1/sigma[j,j] * T1Hij[i, j]/T2Hij[i, i]
    }
  }
  
  tilde_tot <- compute_tilde_tot_fct(sij)
  
  return( list( sij, tilde_tot[[1]], tilde_tot[[2]] ) )
}