### bootstrap for rolling window.

rm(list = ls())
library(vars)
library(expm)
library(plyr)
library(xts)
library(network)
library(tseries)
library(imputeTS)
library(boot)
setwd("//econ-stat-file2.econ.kit.edu/home$/Gaetjen/R_no2/R git 2")
load("mydatadiff_n.RData")
source("functions/f_VD_intra.R")
source("functions/f_boot_roll.R")
source("functions/f_eval.R")


######### CDS #####
days<-unique(.indexday(c14))
days2<-unique(as.Date(time(c14)))
idays<-as.POSIXct(days2, format="%Y-%m-%d")
nc<-ndays(c14)

Cdata.10<-list()
samplesize.10<-c()
for(i in 1:(nc-9)){
  d10<-first(c14, "10 days")
  Cdata.10[[i]]<-d10
  samplesize.10[i]<-nrow(d10)
  day1<-nrow(first(c14,"1 day"))
  c14<-c14[-c(1:day1),]
}
N<-length(Cdata.10)

CI.c910<-matrix(nrow=N,ncol=3)
set.seed(3)
for(i in 1:N){
  CI.c910[i,]<- f_boot_roll3(Cdata.10[[i]],R=1000,n.ahead=5)
} 

CI.c.10<-as.xts(CI.c910, order.by = idays[10:length(idays)])
ssize.c.10<-as.xts(samplesize.10, order.by = idays[10:length(idays)])

pdf(file="C:/Users/pb3661/Dropbox/R_no2/tex1/timeseries_bootstrap/CI90_CDS_10d.pdf",width=8,height=5)
old.par <- par( no.readonly = TRUE )
par( oma = c( 0, 0, 3, 0 ) )
plot(CI.c.10[,2],ylim=range(CI.c.10, na.rm = T), major.ticks="years",minor.ticks=F, major.format= "%Y", main="CDS")
lines(CI.c.10[,1], col="grey")
lines(CI.c.10[,3], col="grey")
par( old.par )
dev.off()

pdf(file="C:/Users/pb3661/Dropbox/R_no2/tex1/timeseries_bootstrap/ssize_CDS_10d.pdf",width=8,height=5)
old.par <- par( no.readonly = TRUE )
par( oma = c( 0, 0, 3, 0 ) )
plot(ssize.c.10, major.ticks="years",minor.ticks=F, major.format= "%Y", main="sample size CDS")
par( old.par )
dev.off()

######### bond yields #####
by14<-a14
by4<-a4
byGr<-aGr
byG<-aG

days<-unique(.indexday(by14))
days2<-unique(as.Date(time(by14)))
idays<-as.POSIXct(days2, format="%Y-%m-%d")
nby<-ndays(by14)

bydata.20<-list()
samplesize.20<-c()
for(i in 1:(nby-19)){
  d20<-first(by14, "20 days")
  bydata.20[[i]]<-d20
  samplesize.20[i]<-nrow(d20)
  day1<-nrow(first(by14,"1 day"))
  by14<-by14[-c(1:day1),]
}
N<-length(bydata.20)

CI.by910<-matrix(nrow=N,ncol=3)
set.seed(3)
for(i in 1:N){
  CI.by910[i,]<- f_boot_roll3(bydata.20[[i]],R=1000,n.ahead=5)
} 

CI.by.20<-as.xts(CI.by910, order.by = idays[20:length(idays)])
ssize.by.20<-as.xts(samplesize.20, order.by = idays[20:length(idays)])

pdf(file="C:/Users/pb3661/Dropbox/R_no2/tex1/timeseries_bootstrap/CI90_ASW_20d.pdf",width=8,height=5)
old.par <- par( no.readonly = TRUE )
par( oma = c( 0, 0, 3, 0 ) )
plot(CI.by.20[,2],ylim=range(CI.by.20), major.ticks="years",minor.ticks=F, major.format= "%Y", main="ASW")
lines(CI.by.20[,1], col="grey")
lines(CI.by.20[,3], col="grey")
polygon(CI.by.20[,1],CI.by.20[,3], col="grey")
par( old.par )
dev.off()

pdf(file="C:/Users/pb3661/Dropbox/R_no2/tex1/timeseries_bootstrap/ssize_BY_20d.pdf",width=8,height=5)
old.par <- par( no.readonly = TRUE )
par( oma = c( 0, 0, 3, 0 ) )
plot(ssize.by.20, major.ticks="years",minor.ticks=F, major.format= "%Y", main="sample size bond yields")
par( old.par )
dev.off()

CI.a.20<-CI.by.20
ssize.a.20<-ssize.by.20
# load("final_results_exper_20d.RData")
# setwd("C:/Users/pb3661/Dropbox/R_no2")
# save(CI.c.20, ssize.c.20, CI.by.20, ssize.by.20, CI.a.20, ssize.a.20, file="final_results_exper_20d.RData")

setwd("//econ-stat-file2.econ.kit.edu/share-alle/Rebekka boot/results")
save(CI.c.20, ssize.c.20, CI.a.20, ssize.a.20, file="CI_longts_20d_5ahead.RData")
write.zoo(CI.c.20,"CI.c.20.5ahead.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(CI.by.20,"CI.a.20.5ahead.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

#########

#install.packages("doParallel")
library(doParallel)

c1<-makeCluster(10)
registerDoParallel(c1)

fr.by11<-foreach(i = 1:(nby11-Te+1),.packages=c("vars","expm","plyr","xts","network",
                                              "tseries","imputeTS","boot"))%dopar%{ 
                                                f_boot_roll2(by11[i:(Te+i-1),])}
CI.by11<-matrix(unlist(fr.by11),nrow=nby11-Te+1,ncol=5, byrow=TRUE)

save(CI.by11,CI.by12, file="results_1112_b.RData")



