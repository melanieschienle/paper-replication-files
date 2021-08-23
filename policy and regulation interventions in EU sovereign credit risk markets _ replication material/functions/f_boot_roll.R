

f_boot_roll2<-function(data,R=1000){
  n<-nrow(data)
  k<-ncol(data)
  # estimate VAR:
  VAR<-VAR.reb(data, type = "none", lag.max = 1, ic="AIC", exog=NULL)
  res<-scale(VAR$resid, center=T, scale=F)
  coef<-VAR$coef
  p<-VAR$p
  zmat<-VAR$zmat
  VDm<-f.VD.intra1(data,1)[[2]]
  ## with R=1000 replications:
  VD<-array(dim=c(k,k,R))
    for (j in 1:R){
    ind<-sample(1:nrow(res), nrow(res), replace=T)
    bres<-res[ind,]
    
    # compute bootstrap time series
    y0<-data[1:VAR$p,]
    ystar <- matrix(nrow=nrow(data),ncol=ncol(data))
    ystar[1:VAR$p,] <- y0 # Input the initial value
    for(i in (VAR$p+1): nrow(ystar)){ #switch order of ystar to have xt-1,xt-2,...
      ystar[i,] <- coef%*% as.vector(ystar[(i-1):(i-VAR$p),]) + bres[i-VAR$p,]
    }
    
    # use new bootstrap time series to calculate statistic
    try(VD[,,j]<-f.VD.intra1(ystar,1)[[2]], silent=T)
  }
  VD0<-array(apply(VD, 3, function(x){y<-x; diag(y)<-0; return(y)}),dim=c(k,k,1000))
  diag(VDm)<-0
  CIm<-mean(colSums(VDm))#-diag(VDm))
  VDtot<-apply(VD0, 3, function(x){mean(colSums(x))})
  #CI<-quantile(VDtot, probs=c(0.05,0.95),type=8)
  CI<-sort(VDtot)[c(50,950)]
  CI.<-c(CI[1],CIm,CI[2])
  return(CI.)
}


f_boot_roll3<-function(data,R=1000,n.ahead){
  n<-nrow(data)
  k<-ncol(data)
  # estimate VAR:
  VAR<-VAR.reb(data, type = "none", lag.max = 1, ic="AIC", exog=NULL)
  res<-scale(VAR$resid, center=T, scale=F)
  coef<-VAR$coef
  p<-VAR$p
  zmat<-VAR$zmat
  VDm<-f.VD.intra1(data,n.ahead)[[2]]
  ## with R=1000 replications:
  VD<-array(dim=c(k,k,R))
  for (j in 1:R){
    ind<-sample(1:nrow(res), nrow(res), replace=T)
    bres<-res[ind,]
    
    # compute bootstrap time series
    y0<-data[1:VAR$p,]
    ystar <- matrix(nrow=nrow(data),ncol=ncol(data))
    ystar[1:VAR$p,] <- y0 # Input the initial value
    for(i in (VAR$p+1): nrow(ystar)){ #switch order of ystar to have xt-1,xt-2,...
      ystar[i,] <- coef%*% as.vector(ystar[(i-1):(i-VAR$p),]) + bres[i-VAR$p,]
    }
    
    # use new bootstrap time series to calculate statistic
    try(VD[,,j]<-f.VD.intra1(ystar,n.ahead)[[2]], silent=T)
  }
  VD0<-array(apply(VD, 3, function(x){y<-x; diag(y)<-0; return(y)}),dim=c(k,k,1000))
  diag(VDm)<-0
  CIm<-mean(colSums(VDm))#-diag(VDm))
  VDtot<-apply(VD0, 3, function(x){mean(colSums(x))})
  #CI<-quantile(VDtot, probs=c(0.05,0.95),type=8)
  CI<-sort(VDtot)[c(50,950)]
  CI.<-c(CI[1],CIm,CI[2])
  return(CI.)
}
