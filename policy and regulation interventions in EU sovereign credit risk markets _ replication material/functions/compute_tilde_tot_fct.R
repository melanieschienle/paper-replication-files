compute_tilde_tot_fct <- function (mat) {
  
  ctry   <- dim(mat)[1]
  
  tilde  <- mat/rowSums( mat )
  
  mat    <- tilde - diag( diag( tilde ) )
  
  tot    <- 1/ctry * sum( mat ) 
  
  return( list( tilde, tot) )
}