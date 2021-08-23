rm(list=ls())

library("readxl")

setwd("C:/Users/urbjoe00/Dropbox/BIS_KIT/t_statistics_RefRep/")

alt = "two.sided"  # "less", "greater"

# redirect output
file.remove("data_stat_tests_table3_ctry.txt")
Sys.sleep(1)
sink(file = "data_stat_tests_table3_ctry.txt", append = TRUE, type = c("output", "message"), split = FALSE)

f.removeNA <-function(data){
  data <- data[!is.na(data)]
  return(data)
}

n        = 90 
n_window = 90 

cat("countrywise CDS")
cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_left_EAP")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_left_SMP")

cat("1-2 GR1-SMP1   ")
cat("\n")

for (i in c("CDS_DE", "CDS_ES", "CDS_FR", "CDS_GR", "CDS_IE", "CDS_IT", "CDS_PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- head(f.removeNA(my_data2$", i, "_a), n)", sep="") ))

  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

  cat(substr(i,5,6))
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}

cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_left_CDS")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_left_CDS")

cat("3 IE   ")
cat("\n")

for (i in c("CDS_DE", "CDS_ES", "CDS_FR", "CDS_GR", "CDS_IE", "CDS_IT", "CDS_PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- head(f.removeNA(my_data2$", i, "_a), n)", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(substr(i,5,6))
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}

cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_right_CDS")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_right_CDS")

cat("4 PT   ")
cat("\n")

for (i in c("CDS_DE", "CDS_ES", "CDS_FR", "CDS_GR", "CDS_IE", "CDS_IT", "CDS_PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- head(f.removeNA(my_data2$", i, "_a), n)", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(substr(i,5,6))
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}

cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_left_EAP")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_left_SMP")

cat("5-6 GR2-SMP2   ")
cat("\n")

for (i in c("CDS_DE", "CDS_ES", "CDS_FR", "CDS_GR", "CDS_IE", "CDS_IT", "CDS_PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- head(f.removeNA(my_data2$", i, "_a), n)", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(substr(i,5,6))
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}

cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_left_EAP")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_left_Draghi")

cat("8-9 ES-Draghi ")
cat("\n")

for (i in c("CDS_DE", "CDS_ES", "CDS_FR", "CDS_IE", "CDS_IT", "CDS_PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- head(f.removeNA(my_data2$", i, "_a), n)", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(substr(i,5,6))
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}


cat("\n")
cat("\n")
cat("countrywise Connectedness")
cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_right_EAP")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_right_SMP")

cat("1-2 GR1-SMP1   ")
cat("\n")

for (i in c("DE", "ES", "FR", "GR", "IE", "IT", "PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- tail(head(f.removeNA(my_data2$", i, "_a), n+n_window), n )", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(i)
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}

cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_left")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_left")

cat("3 IE   ")
cat("\n")

for (i in c("DE", "ES", "FR", "GR", "IE", "IT", "PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- tail(head(f.removeNA(my_data2$", i, "_a), n+n_window), n )", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(i)
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}

cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_right")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_right")

cat("4 PT   ")
cat("\n")

for (i in c("DE", "ES", "FR", "GR", "IE", "IT", "PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- tail(head(f.removeNA(my_data2$", i, "_a), n+n_window), n )", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(i)
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}

cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_right_EAP")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_right_SMP")

cat("5-6 GR2-SMP2   ")
cat("\n")

for (i in c("DE", "ES", "FR", "GR", "IE", "IT", "PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- tail(head(f.removeNA(my_data2$", i, "_a), n+n_window), n )", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(i)
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}

cat("\n")

#######################################################################################################

my_data1 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_right_EAP")
my_data2 <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_right_Draghi")

cat("8-9 ES-Draghi ")
cat("\n")

for (i in c("DE", "ES", "FR", "IE", "IT", "PT")) {
  eval(parse(text=paste("before  <- tail(f.removeNA(my_data1$", i, "_b), n)", sep="") ))
  eval(parse(text=paste("after   <- tail(head(f.removeNA(my_data2$", i, "_a), n+n_window), n )", sep="") ))
  
  outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)
  
  cat(i)
  cat("  ")
  toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2) )
  cat(sprintf("%.2f\t", toprint))
  cat("\n")
}


sink()