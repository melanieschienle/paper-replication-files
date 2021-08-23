RCorr_fct <- function (data) {
  
  sij_d_2   <- cor(data)^2   # elementwise multiplication/squaring
  
  tilde_tot <- compute_tilde_tot_fct(sij_d_2)
  
  return( list( sij_d_2, tilde_tot[[1]], tilde_tot[[2]] ) )
}