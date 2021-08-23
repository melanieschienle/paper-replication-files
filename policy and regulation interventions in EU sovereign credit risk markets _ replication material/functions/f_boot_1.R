# f_boot_1<-function(data,R=1000){
#   n<-nrow(data)
#   k<-ncol(data)
#   # estimate VAR:
#   VAR<-VAR.reb(data, type = "none", lag.max = 3, ic="AIC", exog=NULL)
#   res<-scale(VAR$resid, center=T, scale=F)
#   coef<-VAR$coef
#   ## with R=1000 replications:
#   VD<-array(dim=c(k,k,R))
#   for (j in 1:R){
#     bres<-apply(res,2,function(x){sample(x, length(x), replace=T)}) # resample each column seperately
#     
#     # compute bootstrap time series
#     y0<-data[1:VAR$p,]
#     ystar <- matrix(nrow=nrow(data),ncol=ncol(data))
#     ystar[1,] <- y0<-data[1:VAR$p,]# Input the initial value
#     for(i in 2: nrow(ystar)){
#       ystar[i,] <- coef%*%ystar[i-1,] + bres[i-1,]
#     }
#     
#     # use new bootstrap time series to calculate statistic
#     VD[,,j]<-f.VD.intra1(ystar,1)[[2]]
#   }
#   CI<-apply(VD, c(1,2), function(x){quantile(x,probs=c(0.05,0.95))})
#   return(list(CI[1,,],CI[2,,]))
#   #return(CI)
# }
# 
# f_boot_2<-function(data,R=1000){
#   n<-nrow(data)
#   k<-ncol(data)
#   # estimate VAR:
#   VAR<-VAR.reb(data, type = "none", lag.max = 3, ic="AIC", exog=NULL)
#   res<-scale(VAR$resid, center=T, scale=F)
#   #res<-VAR$resid
#   coef<-VAR$coef
#   p<-VAR$p
#   zmat<-VAR$zmat
#   ## with R=1000 replications:
#   VD<-array(dim=c(k,k,R))
#   for (j in 1:R){
#     ind<-sample(1:nrow(res), nrow(res), replace=T)
#     bres<-res[ind,]
#     
#     # compute bootstrap time series
#     y0<-data[1:p,]
#     ystar <- matrix(nrow=n,ncol=k)
#     ystar[1:p,] <- y0 # Input the initial value
#     
#     ystar[(p+1): nrow(ystar),] <- t(coef %*% zmat) + bres
#     # use new bootstrap time series to calculate statistic
#     VD[,,j]<-f.VD.intra1(ystar,1)[[1]]
#   }
#   VDm<-f.VD.intra1(data,1)[[1]]
#   #CI<-apply(VD, c(1,2), function(x){quantile(x,probs=c(0.05,0.95))})
#   CI<-apply(VD, c(1,2), function(x){quantile(x,probs=c(0.33,0.67))})
#   return(list(CI[1,,],CI[2,,]))
#   #return(CI)
# }
# 
# 
# f_boot_3<-function(data,R=1000){
#   n<-nrow(data)
#   k<-ncol(data)
#   # estimate VAR:
#   VAR<-VAR.reb(data, type = "none", lag.max = 3, ic="AIC", exog=NULL)
#   res<-scale(VAR$resid, center=T, scale=F)
#   #res<-VAR$resid
#   coef<-VAR$coef
#   p<-VAR$p
#   zmat<-VAR$zmat
#   ## with R=1000 replications:
#   VD<-array(dim=c(k,k,R))
#   for (j in 1:R){
#     ind<-sample(1:nrow(res), nrow(res), replace=T)
#     bres<-res[ind,]
#     
#     # compute bootstrap time series
#     y0<-data[1:p,]
#     ystar <- matrix(nrow=n,ncol=k)
#     ystar[1:p,] <- y0 # Input the initial value
#     
#     ystar[(p+1): nrow(ystar),] <- t(coef %*% zmat) + bres
#     # use new bootstrap time series to calculate statistic
#     VD[,,j]<-f.VD.intra1(ystar,1)[[2]]
#   }
#   VDm<-f.VD.intra1(data,1)[[2]] # normalized VD: Dtilde
#   #CI<-apply(VD, c(1,2), function(x){quantile(x,probs=c(0.05,0.95))})
#   CI<-apply(VD, c(1,2), function(x){quantile(x,probs=c(0.33,0.67))})
#   return(list(lo=CI[1,,],est=VDm, hi=CI[2,,]))
#   #return(CI)
# }
# 
# f_boot_4<-function(data,R=1000){
#   n<-nrow(data)
#   k<-ncol(data)
#   # estimate VAR:
#   VAR<-VAR.reb(data, type = "none", lag.max = 3, ic="AIC", exog=NULL)
#   res<-scale(VAR$resid, center=T, scale=F)
#   #res<-VAR$resid
#   coef<-VAR$coef
#   p<-VAR$p
#   zmat<-VAR$zmat
#   ## with R=1000 replications:
#   VD<-array(dim=c(k,k,R))
#   for (j in 1:R){
#     ind<-sample(1:nrow(res), nrow(res), replace=T)
#     bres<-res[ind,]
#     
#     # compute bootstrap time series
#     y0<-data[1:p,]
#     ystar <- matrix(nrow=n,ncol=k)
#     ystar[1:p,] <- y0 # Input the initial value
#     
#     ystar[(p+1): nrow(ystar),] <- t(coef %*% zmat) + bres
#     # use new bootstrap time series to calculate statistic
#     VD[,,j]<-f.VD.intra1(ystar,1)[[2]]
#   }
#   VDm<-f.VD.intra1(data,1)[[2]] # normalized VD: Dtilde
#   CI<-apply(VD, c(1,2), function(x){quantile(x,probs=c(0.05,0.95))})
#   #CI<-apply(VD, c(1,2), function(x){quantile(x,probs=c(0.33,0.67))})
#   return(list(lo=CI[1,,],est=VDm, hi=CI[2,,]))
#   #return(CI)
# }



f_boot_4<-function(data,R=1000){
  n<-nrow(data)
  k<-ncol(data)
  # estimate VAR:
  VAR<-VAR.reb(data, type = "none", lag.max = 1, ic="AIC", exog=NULL)
  res<-scale(VAR$resid, center=T, scale=F)
  #res<-VAR$resid
  coef<-VAR$coef
  p<-VAR$p
  zmat<-VAR$zmat
  ## with R=1000 replications:
  VD<-array(dim=c(k,k,R))
  for (j in 1:R){
    ind<-sample(1:nrow(res), nrow(res), replace=T)
    bres<-res[ind,]

    # compute bootstrap time series
    y0<-data[1:p,]
    ystar <- matrix(nrow=n,ncol=k)
    ystar[1:p,] <- y0 # Input the initial value

    ystar[(p+1): nrow(ystar),] <- t(coef %*% zmat) + bres
    # use new bootstrap time series to calculate statistic
    VD[,,j]<-f.VD.intra1(ystar,1)[[2]]
  }
  VD0<-array(apply(VD, 3, function(x){y<-x; diag(y)<-0; return(y)}),dim=c(k,k,1000))
  VDm<-f.VD.intra1(data,1)[[2]] # normalized VD: Dtilde
  diag(VDm)<-0
  CI<-apply(VD0, c(1,2), function(x){quantile(x,probs=c(0.05,0.95),type=8)})
  #CI<-apply(VD0, c(1,2), function(x){sort(x)})[c(50,950),1:ncol(data),]
  return(list(lo=CI[1,,],est=VDm, hi=CI[2,,]))
}

f_boot_5<-function(data,R=1000){ # construct bootstrap ts recursively and order by tot conn
  n<-nrow(data)
  k<-ncol(data)
  # estimate VAR:
  VAR<-VAR.reb(data, type = "none", lag.max = 1, ic="AIC", exog=NULL)
  res<-scale(VAR$resid, center=T, scale=F)
  #res<-VAR$resid
  coef<-VAR$coef
  p<-VAR$p
  zmat<-VAR$zmat
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
    VD[,,j]<-f.VD.intra1(ystar,1)[[2]]
  }
  VD0<-array(apply(VD, 3, function(x){y<-x; diag(y)<-0; return(y)}),dim=c(k,k,1000))
  VDm<-f.VD.intra1(data,1)[[2]] # normalized VD: Dtilde
  diag(VDm)<-0
  VDtot<-apply(VD0, 3, function(x){mean(colSums(x))})
  names(VDtot)<-dimnames(VD0)[[3]]<-1:1000
  VDsort<-sort(VDtot)
  VD0sort<-VD0[,,names(VDsort)]
  CI<-VD0sort[,,c(50,950)]
  return(list(lo=CI[,,1],est=VDm, hi=CI[,,2]))
}
#test<-f_boot_4(all.d$post_Gr2_by)
#data<-all.d$post_Gr2_by


f_boot_rel<-function(data,R=1000){ # construct bootstrap ts recursively and order by tot conn
  n<-nrow(data)
  k<-ncol(data)
  # estimate VAR:
  VAR<-VAR.reb(data, type = "none", lag.max = 1, ic="AIC", exog=NULL)
  res<-scale(VAR$resid, center=T, scale=F)
  #res<-VAR$resid
  coef<-VAR$coef
  p<-VAR$p
  zmat<-VAR$zmat
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
    VD[,,j]<-f.VD.intra.rel(ystar,1)
  }
  VD0<-array(apply(VD, 3, function(x){y<-x; diag(y)<-0; return(y)}),dim=c(k,k,1000))
  VDm<-f.VD.intra.rel(data,1) # normalized VD: Dtilde
  diag(VDm)<-0
  VDtot<-apply(VD0, 3, function(x){mean(colSums(x))})
  names(VDtot)<-dimnames(VD0)[[3]]<-1:1000
  VDsort<-sort(VDtot)
  VD0sort<-VD0[,,names(VDsort)]
  CI<-VD0sort[,,c(50,950)]
  return(list(lo=CI[,,1],est=VDm, hi=CI[,,2]))
}
