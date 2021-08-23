# country-wise VD and total VD with CI for small windows around events

rm(list = ls())
library(vars)
library(expm)
library(plyr)
library(xts)
setwd("C:/Users/pb3661/Dropbox/R_no2")
load("mydatadiff_n.RData")
load("myeventwindows_play.RData")
source("functions/f_VD_intra.R")
source("functions/f_boot_roll.R")
source("functions/f_eval.R")


##########################################################
f.CI<-function(data,Te){
  n<-nrow(data)
  CI<-matrix(nrow=n-Te+1,ncol=3)
  for(i in 1:(n-Te+1)){
    CI[i,]<- f_boot_roll2(data[i:(Te+i-1),])
  }
  return(CI)
}
##########################################################
set.seed(3)
CI.GrS1<-f.CI(all.d$GrSMP1_cds, 90)
CI.GrS2<-f.CI(all.d$GrSMP2_cds, 90)
CI.EsDrgO<-f.CI(all.d$EsDrgO_cds, 90)
CI.Ie<-f.CI(all.d$Ie_cds, 90)
CI.Pt<-f.CI(all.d$Pt_cds, 90)
set.seed(42)
CI.14<-f.CI(all.d$down2014_cds, 90) # does not run
set.seed(42)
CI.GD<-f.CI(all.d$GD_cds, 90)

set.seed(3)
CI.GrS1<-f.CI(all.d$GrSMP1_cds, 180)
CI.GrS2<-f.CI(all.d$GrSMP2_cds, 180)
CI.EsDrgO<-f.CI(all.d$EsDrgO_cds, 180)
CI.Ie<-f.CI(all.d$Ie_cds, 180)
CI.Pt<-f.CI(all.d$Pt_cds, 180)
set.seed(42)
CI.14<-f.CI(all.d$down2014_cds, 180) # runs!
set.seed(42)
CI.ban<-f.CI(all.d$ban_cds, 180) 
set.seed(42)
CI.ban.by<-f.CI(bondban.d, 180) 


CI.GrS1.a.DP<-f.CI(all.d$GrSMP1_asw[,c(1,6)], 180)
CI.GrS1.a.DE<-f.CI(all.d$GrSMP1_asw[,c(7,6)], 180)
CI.GrS1.a.PE<-f.CI(all.d$GrSMP1_asw[,c(1,7)], 180)

set.seed(3)
CI.GrS1.a<-f.CI(all.d$GrSMP1_asw, 180) # does not run on 90
CI.GrS2.a<-f.CI(all.d$GrSMP2_asw, 180) # does not run on 90, 120
CI.EsDrgO.a<-f.CI(all.d$EsDrgO_asw, 180) # does not run on 90
CI.Ie.a<-f.CI(all.d$Ie_asw, 180) # not covered by CI for 90
CI.Pt.a<-f.CI(all.d$Pt_asw, 180) # not covered by CI for 90
CI.14.a<-f.CI(all.d$down2014_asw, 180) # does not run for 90, 180

plot(ban.b[,2],ylim=range(ban.b, na.rm = T), major.ticks="months",minor.ticks=F, major.format= "%m", main="bond yields, w/ all")
lines(ban.b[,1], col="grey")
lines(ban.b[,3], col="grey")


#############################################################
Te<-90
GS1.<-as.xts(CI.GrS1,order.by=index(all.d$GrSMP1_cds[Te:nrow(all.d$GrSMP1_cds),]))
Ie.<-as.xts(CI.Ie,order.by=index(all.d$Ie_cds[Te:nrow(all.d$Ie_cds),]))
Pt.<-as.xts(CI.Pt,order.by=index(all.d$Pt_cds[Te:nrow(all.d$Pt_cds),]))
GS2.<-as.xts(CI.GrS2,order.by=index(all.d$GrSMP2_cds[Te:nrow(all.d$GrSMP2_cds),]))
EDO.<-as.xts(CI.EsDrgO,order.by=index(all.d$EsDrgO_cds[Te:nrow(all.d$EsDrgO_cds),]))
GD.<-as.xts(CI.GD,order.by=index(all.d$GD_cds[Te:nrow(all.d$GD_cds),]))
Te<-180
down.<-as.xts(CI.14,order.by=index(all.d$down2014_cds[Te:nrow(all.d$down2014_cds),]))
ban.<-as.xts(CI.ban,order.by=index(all.d$ban_cds[Te:nrow(all.d$ban_cds),]))
############################################################
GS1.a<-as.xts(CI.GrS1.a,order.by=index(all.d$GrSMP1_asw[Te:nrow(all.d$GrSMP1_asw),]))
Ie.a<-as.xts(CI.Ie.a,order.by=index(all.d$Ie_asw[Te:nrow(all.d$Ie_asw),]))
Pt.a<-as.xts(CI.Pt.a,order.by=index(all.d$Pt_asw[Te:nrow(all.d$Pt_asw),]))
GS2.a<-as.xts(CI.GrS2.a,order.by=index(all.d$GrSMP2_asw[Te:nrow(all.d$GrSMP2_asw),]))
EDO.a<-as.xts(CI.EsDrgO.a,order.by=index(all.d$EsDrgO_asw[Te:nrow(all.d$EsDrgO_asw),]))
down.a<-as.xts(CI.14.a,order.by=index(all.d$down2014_asw[Te:nrow(all.d$down2014_asw),]))

GS1.a.DF<-as.xts(CI.GrS1.a.DF,order.by=index(all.d$GrSMP1_asw[Te:nrow(all.d$GrSMP1_asw),]))
GS1.a.DE<-as.xts(CI.GrS1.a.DE,order.by=index(all.d$GrSMP1_asw[Te:nrow(all.d$GrSMP1_asw),]))
GS1.a.PE<-as.xts(CI.GrS1.a.PE,order.by=index(all.d$GrSMP1_asw[Te:nrow(all.d$GrSMP1_asw),]))
Te<-180
ban.b<-as.xts(CI.ban.by.,order.by=index(bondban.d[Te:nrow(bondban.d),]))
ban.bPI<-as.xts(CI.ban.by.PI,order.by=index(bondban.d[Te:nrow(bondban.d),]))
ban.bP<-as.xts(CI.ban.by.P,order.by=index(bondban.d[Te:nrow(bondban.d),]))
save(ban.b,ban.bP,ban.bPI, file="bondban_CI.RData")


colnames(GS1.)<-colnames(Ie.)<-colnames(Pt.)<-colnames(GS2.)<-colnames(EDO.)<-colnames(EDO.)<-colnames(down.)<-
  colnames(GS1.a)<-colnames(Ie.a)<-colnames(Pt.a)<-colnames(GS2.a)<-colnames(EDO.a)<-colnames(down.a)<-c("CI05","est","CI95")
#setwd("//econ-stat-file2.econ.kit.edu/share-alle/Rebekka boot/results/csv")
setwd("C:/Users/pb3661/Dropbox/R_no2/R git 2/results/csv")
save(GS1.,GS1.a,GS2.a,GS2.,Ie.,Ie.a,Pt.a,Pt.,EDO.a,EDO.,down.a,down. , file="ts_events_90d.RData")
write.zoo(GS1.,"ts.GS1.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(Ie.,"ts.Ie.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(Pt.,"ts.Pt.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(GS2.,"ts.GS2.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(EDO.,"ts.EDO.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(down.,"ts.down14.long.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(ban.,"ts.ban.long.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(GD.,"ts.GD.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

write.zoo(GS1.a,"ts.GS1.IPPS.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(Ie.a,"ts.Ie.a.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(Pt.a,"ts.Pt.a.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(GS2.a,"ts.GS2.a.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(EDO.a,"ts.EDO.a.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(down.a,"ts.down14.a.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
##########################################################
write.zoo(GS1.a.DF,"ts.GS1.DF.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(GS1.a.DE,"ts.GS1.DE.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(GS1.a.PE,"ts.GS1.PE.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

d<-which(index(all.d$GrSMP1_asw)=="2010-05-05 9:30")
data<-all.d$GrSMP1_asw[(d-180):d,c(7,6)]# De Es
VAR<-VAR.reb(data, type = "none", lag.max = 1, ic="AIC", exog=NULL)
(res<-scale(VAR$resid, center=T, scale=F))
data<-all.d$GrSMP1_asw[(d-180):d,c(1,7)]# Pt Es
VAR<-VAR.reb(data, type = "none", lag.max = 1, ic="AIC", exog=NULL)
(res<-scale(VAR$resid, center=T, scale=F))

d<-which(index(all.d$GD_cds)=="2012-03-01 12:00")
index(all.d$GD_cds[d+90,])
d<-which(index(all.d$GD_cds)=="2012-03-09 12:00")
index(all.d$GD_cds[d+90,])

####### asw ban

ban.asw<-as.xts(CI.ban.asw,order.by=index(all.d$ban_asw[Te:nrow(all.d$ban_asw),]))

write.zoo(ban.asw,"ts.ban.asw.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

########### country-wise for ban and rules

source("f_VD_inroll.R")

VD.14<-f.VDinroll.T(all.d$down2014_cds, n.ahead = 1, 180)
VD.ban<-f.VDinroll.T(all.d$ban_cds, n.ahead = 1,180)

ctry.14<-t(f.sums(VD.14$Dtilde)$tos)
ctry.ban<-t(f.sums(VD.ban$Dtilde)$tos) 

Te<-180
down..<-as.xts(ctry.14,order.by=index(all.d$down2014_cds))[Te:nrow(all.d$down2014_cds),]
ban..<-as.xts(ctry.ban,order.by=index(all.d$ban_cds))[Te:nrow(all.d$ban_cds),]
colnames(ban..)<-colnames(down..)<-colnames(all.d$down2014_cds)

#setwd("//econ-stat-file2.econ.kit.edu/share-alle/Rebekka boot/results/csv")
setwd("C:/Users/pb3661/Dropbox/R_no2/R git 2/results/csv")
write.zoo(down..,"ts.down14.ctry.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(ban..,"ts.ban.ctry.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

### ban, Ie, Pt with 90 d rolling window
VD.ban<-f.VDinroll.T(all.d$ban_cds, n.ahead = 1,90)
ctry.ban<-f.sums(VD.ban$Dtilde)$totals
VD.Ie<-f.VDinroll.T(all.d$Ie_cds, n.ahead = 1,90)
ctry.Ie<-f.sums(VD.Ie$Dtilde)$totals
VD.Pt<-f.VDinroll.T(all.d$Pt_cds, n.ahead = 1,90)
ctry.Pt<-f.sums(VD.Pt$Dtilde)$totals
Te<-90
ban..<-as.xts(ctry.ban,order.by=index(all.d$ban_cds))[Te:nrow(all.d$ban_cds),]
Ie..<-as.xts(ctry.Ie,order.by=index(all.d$Ie_cds))[Te:nrow(all.d$Ie_cds),]
Pt..<-as.xts(ctry.Pt,order.by=index(all.d$Pt_cds))[Te:nrow(all.d$Pt_cds),]
write.zoo(ban..,"ts.ban.tot.90d.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(Ie..,"ts.Ie.tot.90d.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(Pt..,"ts.Pt.tot.90d.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

######### total with 180 days for all.d$GrSMP1_cds, all.d$GrSMP2_cds, all.d$EsDrgO_cds
VD.GS1.old<-f.VDinroll.T(all.d$GrSMP1_cds, n.ahead = 1, 180)
ctry.GS1.old<-f.sums(VD.GS1.old$Dtilde)$totals
GS1..old<-as.xts(ctry.GS1.old,order.by=index(all.d$GrSMP1_cds))[Te:nrow(all.d$GrSMP1_cds),]

VD.GS1<-f.VDinroll.T(G12D_90.d$GrSMP1_cds, n.ahead = 1, 180)
VD.GS2<-f.VDinroll.T(G12D_90.d$GrSMP2_cds, n.ahead = 1, 180)
VD.EDO<-f.VDinroll.T(G12D_90.d$EsDrgO_cds, n.ahead = 1, 180)

ctry.GS1<-t(f.sums(VD.GS1$Dtilde)$tos)
ctry.GS2<-t(f.sums(VD.GS2$Dtilde)$tos)
ctry.EDO<-t(f.sums(VD.EDO$Dtilde)$tos) 

ctry.GS1<-f.sums(VD.GS1$Dtilde)$totals
ctry.GS2<-f.sums(VD.GS2$Dtilde)$totals
ctry.EDO<-f.sums(VD.EDO$Dtilde)$totals

Te<-180
GS1..<-as.xts(ctry.GS1,order.by=index(G12D_90.d$GrSMP1_cds))[Te:nrow(G12D_90.d$GrSMP1_cds),]
GS2..<-as.xts(ctry.GS2,order.by=index(G12D_90.d$GrSMP2_cds))[Te:nrow(G12D_90.d$GrSMP2_cds),]
EDO..<-as.xts(ctry.EDO,order.by=index(G12D_90.d$EsDrgO_cds))[Te:nrow(G12D_90.d$EsDrgO_cds),]
colnames(GS1..)<-colnames(GS2..)<-colnames(all.d$GrSMP1_cds)
colnames(EDO..)<-colnames(all.d$EsDrgO_cds)

setwd("C:/Users/pb3661/Dropbox/BIS_KIT/policy_regulation/R_for_revision/ctry_180d")
write.zoo(GS1..,"ts.GS1.ctry.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(GS2..,"ts.GS2.ctry.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(EDO..,"ts.EDO.ctry.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

write.zoo(GS1..,"ts.GS1.tot2.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(GS2..,"ts.GS2.tot2.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")
write.zoo(EDO..,"ts.EDO.tot2.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")


# bond yield ban:
setwd("C:/Users/pb3661/Dropbox/R_no2")
load("mybondbanwindow.RData")
VD.ban.by<-f.VDinroll.T(all.d$ban_by, n.ahead = 1,180)
ctry.ban.by<-t(f.sums(VD.ban.by$Dtilde)$tos) 

Te<-180
ban.by.<-as.xts(ctry.ban.by,order.by=index(all.d$ban_by))[Te:nrow(all.d$ban_by),]
colnames(ban.by.)<-colnames(all.d$ban_by)
#setwd("//econ-stat-file2.econ.kit.edu/share-alle/Rebekka boot/results/csv")
setwd("C:/Users/pb3661/Dropbox/R_no2/R git 2/results/csv")
write.zoo(ban.by.,"ts.ban.ctry.by.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")

# bond yield 2014:

VD.by.14<-f.VDinroll.T(bond14.d, n.ahead = 1,180)
tot.by.14<-f.sums(VD.by.14$Dtilde)$totals 

Te<-180
by.14<-as.xts(tot.by.14,order.by=index(bond14.d))[Te:nrow(bond14.d),]
plot(by.14)
setwd("C:/Users/pb3661/Dropbox/R_no2/R git 2/results/csv")
write.zoo(by.14,"ts.14.by.txt",row.names=F,col.names = T ,sep="\t", eol = "\n")


