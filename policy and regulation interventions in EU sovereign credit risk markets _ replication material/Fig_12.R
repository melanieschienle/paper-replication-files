rm(list = ls())

library("mlVAR")
library("vars")
library("expm")
library(stringr)
library("timeSeries")
library("readxl")
library(xts)

source("functions/plot_fct.r")
source("functions/extract_Acoeff_fct.r")
source("functions/compute_tilde_tot_fct.r")
source("functions/complete_dy_fct.r")
source("functions/RCorr_fct.r")

window    <- 360
load("data/mydatadiff_n.RData")
data      <- c14

ts_len    <- dim(data)[1]
ctry      <- dim(data)[2]

rho       <- matrix(0, nrow=ts_len+1-window, ncol=ctry^2)
dy        <- matrix(0, nrow=ts_len+1-window, ncol=ctry^2)
corr2     <- matrix(0, nrow=ts_len+1-window, ncol=ctry^2)
count     <- 1

TYPE      <- 3

for( k in 1:(ts_len + 1 - window) ){
  varmod  <- VAR( data[k:(k+window-1),], p=1, type = "none" )
  AA      <- extract_Acoeff_fct( varmod, 0.1 )
  resid   <- residuals( varmod )
  sig     <- cov(resid)
  rho_ij2 <- cor(resid)^2
  
  # computed Dibold Yilmaz for different H Eq 3
  rho_ij2 <- complete_dy_fct(AA, sig, 1)[[TYPE]]          # identical to rhoij2 <- cor(resid)^2 from hand made calculation 
  sij_1   <- complete_dy_fct(AA, sig, 6)[[TYPE]]        
  
  rho[count,]    <- as.vector(rho_ij2)
  dy[count,]     <- as.vector(sij_1)
  
  sij_d_2             <- RCorr_fct(data[k:(k+window-1),])[[TYPE]]
  corr2[count,]       <- as.vector( sij_d_2 )
  
  count          <- count + 1
}

# result for intraday
result_intra     <- c14[180:(dim(c14)[1]-180),1:2]
result_intra[,1] <- dy[,1]
result_intra[,2] <- corr2[,1]
colnames(result_intra)=c("DY", "Rcorr2")
write.zoo(result_intra,file="dy_vs_corr.csv", sep = ",")

plot( result_intra[,1], type="l", xlab="", ylab="", ylim = c(0,1) )
lines(result_intra[,2], col="blue")