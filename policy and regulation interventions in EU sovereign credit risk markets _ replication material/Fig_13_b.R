# simulate data behaviour to test if it affects connectedness measure
rm(list = ls())
library(xts)
load("C:/Users/pb3661/Dropbox/R_no2/mydatadiff_n.RData")
rm(a14,a4,aG,aGr,mid.asw)

# do less obs lead to lower connectedness?
# use pattern of number of observations from 2014-07-01 until 2014-12-31
# and apply it to 2010-07-01 until 2010-12-31

## find timestamps that are missing ####

Grcol<-which(colnames(mid.cds)=="GR")
cds14<-mid.cds[,-Grcol]

# remove rows in which there are 4 NAs or more
nC4na4<-which(apply(cds14, 1, function(x){sum(is.na(x))>=4}))
i1<-grep("2014-07",index(cds14[nC4na4,]))[1] #this is the first index in July 2014 that is missing
# so we are interested in these missing values:
del<-index(cds14)[nC4na4[i1:length(nC4na4)]]
# now we want to change 2014 to 2010, so that we know what to delete.
del10<-gsub("2014", "2010",del)

# now delete these observations to get to the next step.
cds14<-cds14[-nC4na4,]

# remove rows in which there is NA at 08:30:00
nC4<-which(apply(cds14, 1, function(x){sum(is.na(x))>=1}) & 
             (substring(index(cds14),12,16)=="08:30") )
nC4bis<-which(apply(cds14, 1, function(x){sum(is.na(x))>=1}) & 
                (substring(index(cds14),12,16)=="08:30" |
                   substring(index(cds14),12,16)=="09:00"|
                   substring(index(cds14),12,16)=="09:30" ))
nC4cis<-nC4bis[which(nC4bis %in% nC4 == FALSE)]-1
nC4dis<-nC4bis[which(nC4bis %in% nC4cis)+1]
nC4e<-sort(c(nC4,nC4dis))
# same procedure: get dates from 2014 and change 2014 to 2010.
i2<-grep("2014-07",index(cds14[nC4e,]))[1] #this is the first index in July 2014 that is missing
# so we are interested in these missing values:
del2<-index(cds14)[nC4e[i2:length(nC4e)]]
# now we want to change 2014 to 2010, so that we know what to delete.
del10.2<-gsub("2014","2010",del2)

# transform del10 and del10.2 to POSIX.ct
del10<-as.POSIXct(del10, format="%Y-%m-%d %H:%M:%S") #missing obs for 4 countries or more
del10.2<-as.POSIXct(del10.2, format="%Y-%m-%d %H:%M:%S") #missing obs at beginnig of day

# remove froms cds14
cds14<-cds14[-nC4e,]

rm(i1,i2,del,del2,nC4,nC4bis,nC4cis,nC4dis,nC4e,nC4na4,Grcol)

# match indices from del10 and del10.2 to indices in c14 (differenced data)
c.10<-c14["2010-07-01/2010-12-31"] #original dataset from 2010
which(del10.2 %in% index(c.10))
which(index(c.10) %in% del10.2)
d.2<-match(del10.2, index(c.10))
d.1<-match(del10, index(c.10))
c.10d<-c.10[-na.omit(c(d.1,d.2)),] #2010 dataset with missing obs of 2014 deleted (d)

# now compare VD if we use c.10 or c.10d
setwd("C:/Users/pb3661/Dropbox/R_no2/R git 2/")
source("f_VD_inroll.R")
source("fnct_eval.R")
library(vars)
library(plyr)
VD10<-f.VDinroll.T(c.10,n.ahead=1,Te=180)$Dtilde
VD10d<-f.VDinroll.T(c.10d,n.ahead=1,Te=180)$Dtilde

tot10<-f.sums(VD10)$totals
tot10d<-f.sums(VD10d)$totals
Te<-180
tot10.<-as.xts(tot10,order.by=index(c.10))
tot10d.<-as.xts(tot10d,order.by=index(c.10d))
plot(tot10.)
lines(tot10d., col=2)

lessobs<-merge(tot10.,tot10d.)
colnames(lessobs)<-c("original data 2010", "values deleted as in 2014")
setwd("C:/Users/pb3661/Dropbox/R_no2/R git 2/results")
write.zoo(lessobs,"rob.lessobs.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

