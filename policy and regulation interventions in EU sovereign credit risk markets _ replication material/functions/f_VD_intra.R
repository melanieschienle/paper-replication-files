f.VD.intra1<-function(data,n.ahead){
  K<-ncol(data)
  model<-VAR.reb(data,type="none",lag.max=3,ic="AIC",exog=NULL)
  sigu<-model$sigu
  phi<-Phi.reb(model, nstep=n.ahead) 
  
  # numerator matrix#
  f1<- function(x) x%*%sigu # multiply phi by sigma for h=0,...,H.
  phisigma<- alply(phi, 3, f1)
  f2<- function(x) x^2 # take square of each element of matrix phisigma
  phisigma2<- lapply(phisigma, f2)
  numerator<- Reduce("+", phisigma2)# sum of phisigma2's for h=0,...H
  
  # denominator matrix #
  f3<- function(x) x%*%sigu%*%t(x) # multiply phi sigma phi-transposed for h=0,...,H
  phisigmaphi<- alply(phi, 3, f3)
  denominator<- Reduce("+", phisigmaphi) # sum of phisigmaphi's
  
  rm(f1, f2, f3, phisigma, phisigma2, phisigmaphi)
  
  # variance decomposition matrix #
  D2<-array(,dim=c(K,K))
  for(i in 1:K){
    for(j in 1:K){
      D2[i,j]<-numerator[i,j]/(denominator[i,i]*sigu[j,j])
    }
  }
  rm(numerator, denominator)
  
  # Dtilde2 #
  sum<- rowSums(D2) # compute sum of each row
  Dtilde2<-array(,dim=c(K,K))# for each row i, divide each element by sum[i]
  for(i in 1:K){
    Dtilde2[i,]<-apply(t(as.matrix(D2[i,])),2,function(x){x/sum[i]})
  }
  rm(sum)

  return(list(D2,Dtilde2))
}

f.VD.intra.rel<-function(data,n.ahead){
  K<-ncol(data)
  model<-VAR.reb(data,type="none",lag.max=3,ic="AIC",exog=NULL)
  sigu<-model$sigu
  phi<-Phi.reb(model, nstep=n.ahead) 
  
  # numerator matrix#
  f1<- function(x) x%*%sigu # multiply phi by sigma for h=0,...,H.
  phisigma<- alply(phi, 3, f1)
  f2<- function(x) x^2 # take square of each element of matrix phisigma
  phisigma2<- lapply(phisigma, f2)
  numerator<- Reduce("+", phisigma2)# sum of phisigma2's for h=0,...H
  
  # denominator matrix #
  f3<- function(x) x%*%sigu%*%t(x) # multiply phi sigma phi-transposed for h=0,...,H
  phisigmaphi<- alply(phi, 3, f3)
  denominator<- Reduce("+", phisigmaphi) # sum of phisigmaphi's
  
  rm(f1, f2, f3, phisigma, phisigma2, phisigmaphi)
  
  # variance decomposition matrix #
  D2<-array(,dim=c(K,K))
  for(i in 1:K){
    for(j in 1:K){
      D2[i,j]<-numerator[i,j]/(denominator[i,i]*sigu[j,j])
    }
  }
  rm(numerator, denominator)
  
  # Dtilde2 #
  sum<- rowSums(D2) # compute sum of each row
  Dtilde2<-array(,dim=c(K,K))# for each row i, divide each element by sum[i]
  for(i in 1:K){
    Dtilde2[i,]<-apply(t(as.matrix(D2[i,])),2,function(x){x/sum[i]})
  }
  rm(sum)
  
  diag(Dtilde2)<-0
  tot<-sum(Dtilde2)
  Dtilderel<-Dtilde2/tot
  
  return(Dtilderel)
}

