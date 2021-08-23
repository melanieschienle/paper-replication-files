library("readxl")

setwd("C:/Users/urbjoe00/Dropbox/BIS_KIT/t_statistics_RefRep/")

alt = "two.sided"  # "less", "greater"

# redirect output
file.remove("data_stat_tests_table3.txt")
Sys.sleep(1)
sink(file = "data_stat_tests_table3.txt", append = TRUE, type = c("output", "message"), split = FALSE)

f.removeNA <-function(data){
  data <- data[!is.na(data)]
  return(data)
}

n        = 90 
n_window = 90 

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_right_EAP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- tail(head(f.removeNA(my_data$est_after), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("1 GR1    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_right_SMP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- tail(head(f.removeNA(my_data$est_after), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_right_EAP")

before1 <- tail(f.removeNA(my_data$est_before), n)
outp1   <- t.test(before1, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("2 SMP1   ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2), outp1$p.value, 100*( round(mean(after),2)-round(mean(before1),2) )/round(mean(before1),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

########################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_left")

before  <- tail(f.removeNA(my_data$IE_before), 200)
after   <- tail(head(f.removeNA(my_data$IE_after), 200+n_window), 200 )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("3 IE     ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_right")

before  <- tail(f.removeNA(my_data$PT_before), 200)
after   <- tail(head(f.removeNA(my_data$PT_after), 200+n_window), 200 )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("4 PT     ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_right_EAP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- tail(head(f.removeNA(my_data$est_after), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("5 GR2    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_right_SMP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- tail(head(f.removeNA(my_data$est_after), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_right_EAP")

before1 <- tail(f.removeNA(my_data$est_before), n)
outp1   <- t.test(before1, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("6 SMP2   ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2), outp1$p.value, 100*( round(mean(after),2)-round(mean(before1),2) )/round(mean(before1),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_right_EAP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- tail(head(f.removeNA(my_data$est_after), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("8 ES     ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_right_Draghi")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- tail(head(f.removeNA(my_data$est_after), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_right_EAP")

before1 <- tail(f.removeNA(my_data$est_before), n)
outp1  <- t.test(before1, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("9 Draghi ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2), outp1$p.value, 100*( round(mean(after),2)-round(mean(before1),2) )/round(mean(before1),2)   )
cat(sprintf("%.2f\t", toprint))
cat("\n")
cat("\n")
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_left_EAP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- head(f.removeNA(my_data$est_after), n)
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("1 GR1    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_left_SMP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- head(f.removeNA(my_data$est_after), n)
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_8_left_EAP")

before1 <- tail(f.removeNA(my_data$est_before), n)
outp1   <- t.test(before1, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("2 SMP1   ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2), outp1$p.value, 100*( round(mean(after),2)-round(mean(before1),2) )/round(mean(before1),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

########################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_left_CDS")

before  <- tail(f.removeNA(my_data$IE_before), 200)
after   <- head(f.removeNA(my_data$IE_after), 200)
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("3 IE     ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_7_right_CDS")

before  <- tail(f.removeNA(my_data$PT_before), 200)
after   <- head(f.removeNA(my_data$PT_after), 200)
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("4 PT     ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_left_EAP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- head(f.removeNA(my_data$est_after), n)
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("5 GR2    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_left_SMP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- head(f.removeNA(my_data$est_after), n)
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_9_left_EAP")

before1 <- tail(f.removeNA(my_data$est_before), n)
outp1   <- t.test(before1, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("6 SMP2   ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2), outp1$p.value, 100*( round(mean(after),2)-round(mean(before1),2) )/round(mean(before1),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_left_EAP")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- head(f.removeNA(my_data$est_after), n)
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("8 ES     ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

#######################################################################################################

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_left_Draghi")

before  <- tail(f.removeNA(my_data$est_before), n)
after   <- head(f.removeNA(my_data$est_after), n)
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

my_data <- read_excel("data_stat_tests_table3.xls", sheet = "Fig_10_left_EAP")

before1 <- tail(f.removeNA(my_data$est_before), n)
outp1  <- t.test(before1, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("9 Draghi ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2), outp1$p.value, 100*( round(mean(after),2)-round(mean(before1),2) )/round(mean(before1),2)   )
cat(sprintf("%.2f\t", toprint))
cat("\n")

sink()