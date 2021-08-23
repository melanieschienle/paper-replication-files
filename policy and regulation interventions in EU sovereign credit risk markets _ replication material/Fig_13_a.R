rm(list = ls())
setwd("C:/Users/pb3661/Dropbox/R_no2")
load("C:/Users/pb3661/Dropbox/R_no2/myintrad.RData") #from R no2/ data_intrad_new

Grcol<-which(colnames(mid.cds)=="GR")
cds14<-mid.cds[,-Grcol]

# remove rows in which there are 5 NAs or more
nC4<-which(apply(cds14, 1, function(x){sum(is.na(x))>=5}))
cds14<-cds14[-nC4,]

# remove rows in which there is NA at 08:30:00
nC4<-which(apply(cds14, 1, function(x){sum(is.na(x))>=1}) & 
             (substring(index(cds14),12,16)=="08:30") )
#nC4b<-which(apply(cds14, 1, function(x){sum(is.na(x))>=1}) & 
#              (substring(index(cds14),12,16)=="08:30" |
#                 substring(index(cds14),12,16)=="09:00"))
nC4bis<-which(apply(cds14, 1, function(x){sum(is.na(x))>=1}) & 
                (substring(index(cds14),12,16)=="08:30" |
                   substring(index(cds14),12,16)=="09:00"|
                   substring(index(cds14),12,16)=="09:30" ))
nC4cis<-nC4bis[which(nC4bis %in% nC4 == FALSE)]-1
nC4dis<-nC4bis[which(nC4bis %in% nC4cis)+1]
nC4e<-sort(c(nC4,nC4dis))
cds14<-cds14[-nC4e,]


# last obs missing? no.
sum(is.na(cds14[nrow(cds14),]))>=1
# first obs is missing? yes. do next-obs-carried-backward
sum(is.na(cds14[1]))>=1
cds14[1:3,]<-na.locf(coredata(cds14[1:3,]), option="nocb")

# kalman smoothing
f.impute.k<-function(x){
  p<-na.kalman(coredata(x))
  #p<-na.interpolation(coredata(x))
  q<-as.xts(p, order.by=index(x))
  return(q)
}
c4<-f.impute.k(cds14)

# first differences
c14<-diff(c4)[-1,]

rm(BAS.asw,BAS.cds,cds14,mid.asw,mid.cds,Grcol,nC4,nC4bis,nC4cis,nC4dis,nC4e,f.impute.k)


##################
## 10 days #######
##################
setwd("//econ-stat-file2.econ.kit.edu/share-alle/Rebekka boot")
source("f_VD_intra.R")
source("f_boot_roll.R")
source("fnct_eval.R")
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

#pdf(file="C:/Users/pb3661/Dropbox/R_no2/tex1/timeseries_bootstrap/CI90_CDS_10d.pdf",width=8,height=5)
old.par <- par( no.readonly = TRUE )
par( oma = c( 0, 0, 3, 0 ) )
plot(CI.c.10[,2],ylim=range(CI.c.10, na.rm = T), major.ticks="years",minor.ticks=F, major.format= "%Y", main="CDS")
lines(CI.c.10[,1], col="grey")
lines(CI.c.10[,3], col="grey")
par( old.par )
dev.off()

setwd("//econ-stat-file2.econ.kit.edu/share-alle/Rebekka boot/results")
write.zoo(CI.c.10,"CI_robust_countries.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")







