rm(list = ls())

library("mlVAR")
library("vars")
library("expm")
library(stringr)
library("timeSeries")
library("readxl")
library(xts)
library(gap)
library(broom)

source("functions/extract_Acoeff_fct.r")
source("functions/compute_tilde_tot_fct.r")
source("functions/complete_dy_fct.r")
source("functions/RCorr_fct.r")

window    <- 130    # approx. half of a year
start     <- 1045   # start 01.01.2012
end       <- 2349 + 40   # a bit more than 2016 - until 24.02.2017
data      <- read_excel("data/SOV_CDS_daily_R2.xlsx", sheet="diff")
#x axis
date      <- data[start:end,1]
data      <- data[(start-window/2):(end-1+window/2),2:7]

ts_len    <- dim(date)[1]
ctry      <- dim(data)[2]

rho       <- matrix(0, nrow=ts_len, ncol=ctry^2)
dy        <- matrix(0, nrow=ts_len, ncol=ctry^2)
corr2     <- matrix(0, nrow=ts_len, ncol=ctry^2)
count     <- 1

TYPE      <- 3

for( k in 1:ts_len ){
  varmod  <- VAR( data[k:(k-1+window),], p=1, type = "none" )
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

dy <- dy[,1]

faq         <- 667  # 01.11.2012   implementation of ban
mean_before <- mean(dy[1:faq])
imp         <- 726  # implementation ISDA rules
mean_after  <- mean(dy[(imp+1):ts_len])

F <- 927  # 27.06.2015/30.06.2015 GR gov. stops negotiations/default  
G <- 966  # 2. August IMF pulls out of negotiations
H <- 1009 # 22.Oct. ECB unveils renewed QE
I <- 1109 # 10. March 2016 ECB extends QE

# evaluation area boundary for n=60
b1 <- faq-window/2 - 80 +2
b2 <- faq-window/2 + 2
a1 <- imp+window/2 - 2 
a2 <- imp+window/2 -2 + 80


rr     <- 2 # rounding up to 2 digits
alt = "two.sided"
mat = matrix(0,5,6)

before <- dy[1:(faq-window/2)]
after  <- dy[(imp+window/2):length(dy)]

n   <- 20

bef <- data.frame(bf = tail(before, n), xaxes = 1:n)
aft <- data.frame(af = head(after,n), xaxes = 1:n)

outp1   <- t.test(bef$bf, aft$af, alternative = alt, paired = FALSE, var.equal = FALSE)

mean_bef <- mean(bef$bf)
mean_aft <- mean(aft$af)

lm1         <- lm(bf ~ xaxes, data = bef)
pv1         <- glance(lm1)$p.value
lm2         <- lm(af ~ xaxes, data = aft)
pv2         <- glance(lm2)$p.value
chow.test.r <- chow.test(bef$bf,1:n,aft$af,1:n)

mat[1,] <- c(round(mean_bef,rr), round(mean_aft,rr), round((mean_aft-mean_bef)/mean_bef, rr),
             round(outp1$p.value, rr), round(as.numeric(chow.test.r[1]),rr), round(as.numeric(chow.test.r[4],rr)))

n   <- 40

bef <- data.frame(bf = tail(before, n), xaxes = 1:n)
aft <- data.frame(af = head(after,n), xaxes = 1:n)

outp1   <- t.test(bef$bf, aft$af, alternative = alt, paired = FALSE, var.equal = FALSE)

mean_bef <- mean(bef$bf)
mean_aft <- mean(aft$af)

lm1         <- lm(bf ~ xaxes, data = bef)
pv1         <- glance(lm1)$p.value
lm2         <- lm(af ~ xaxes, data = aft)
pv2         <- glance(lm2)$p.value
chow.test.r <- chow.test(bef$bf,1:n,aft$af,1:n)

mat[2,] <- c(round(mean_bef,rr), round(mean_aft,rr), round((mean_aft-mean_bef)/mean_bef, rr),
             round(outp1$p.value, rr), round(as.numeric(chow.test.r[1]),rr), round(as.numeric(chow.test.r[4],rr)))

n   <- 60 

bef <- data.frame(bf = tail(before, n), xaxes = 1:n)
aft <- data.frame(af = head(after,n), xaxes = 1:n)

outp1   <- t.test(bef$bf, aft$af, alternative = alt, paired = FALSE, var.equal = FALSE)

mean_bef <- mean(bef$bf)
mean_aft <- mean(aft$af)

lm1         <- lm(bf ~ xaxes, data = bef)
pv1         <- glance(lm1)$p.value
lm2         <- lm(af ~ xaxes, data = aft)
pv2         <- glance(lm2)$p.value
chow.test.r <- chow.test(bef$bf,1:n,aft$af,1:n)

mat[3,] <- c(round(mean_bef,rr), round(mean_aft,rr), round((mean_aft-mean_bef)/mean_bef, rr),
             round(outp1$p.value, rr), round(as.numeric(chow.test.r[1]),rr), round(as.numeric(chow.test.r[4],rr)))

n   <- 80 # maximum n, because for larger n the impact of event F is influencing the results

bef <- data.frame(bf = tail(before, n), xaxes = 1:n)
aft <- data.frame(af = head(after,n), xaxes = 1:n)

outp1   <- t.test(bef$bf, aft$af, alternative = alt, paired = FALSE, var.equal = FALSE)

mean_bef <- mean(bef$bf)
mean_aft <- mean(aft$af)

lm1         <- lm(bf ~ xaxes, data = bef)
pv1         <- glance(lm1)$p.value
lm2         <- lm(af ~ xaxes, data = aft)
pv2         <- glance(lm2)$p.value
chow.test.r <- chow.test(bef$bf,1:n,aft$af,1:n)

mat[4,] <- c(round(mean_bef,rr), round(mean_aft,rr), round((mean_aft-mean_bef)/mean_bef, rr),
             round(outp1$p.value, rr), round(as.numeric(chow.test.r[1]),rr), round(as.numeric(chow.test.r[4],rr)))


n   <- max # full data set

bef <- data.frame(bf = before, xaxes = 1:length(before))
aft <- data.frame(af = after, xaxes = 1:length(after))

outp1   <- t.test(bef$bf, aft$af, alternative = alt, paired = FALSE, var.equal = FALSE)

mean_bef <- mean(bef$bf)
mean_aft <- mean(aft$af)

lm1         <- lm(bf ~ xaxes, data = bef)
pv1         <- glance(lm1)$p.value
lm2         <- lm(af ~ xaxes, data = aft)
pv2         <- glance(lm2)$p.value
chow.test.r <- chow.test(bef$bf,1:length(before),aft$af,1:length(after))

mat[5,] <- c(round(mean_bef,rr), round(mean_aft,rr), round((mean_aft-mean_bef)/mean_bef, rr),
             round(outp1$p.value, rr), round(as.numeric(chow.test.r[1]),rr), round(as.numeric(chow.test.r[4],rr)))

print(mat)
