rm(list=ls())

library("readxl")

alt = "two.sided"

# redirect output
file.remove("output_Table1.txt")
sink(file = "output_Table1.txt", append = TRUE, type = c("output", "message"), split = FALSE)

f.removeNA <-function(data){
  data <- data[!is.na(data)]
  return(data)
}

n        <- 200
n_window <- 180   # shift, meaning that the post event window starts 180 obs after the event

########################################################################################################

my_data <- read_excel("data_Table1_8.xls", sheet = "tot conn ban")

before  <- tail(f.removeNA(my_data$est_A), n)
after   <- tail(head(f.removeNA(my_data$est_C), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("total ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

########################################################################################################

my_data <- read_excel("data_Table1_8.xls", sheet = "ctry conn ban")

before  <- tail(f.removeNA(my_data$DE_A), n)
after   <- tail(head(f.removeNA(my_data$DE_C), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("DE    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$ES_A), n)
after   <- tail(head(f.removeNA(my_data$ES_C), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("ES    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$FR_A), n)
after   <- tail(head(f.removeNA(my_data$FR_C), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("FR    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$IE_A), n)
after   <- tail(head(f.removeNA(my_data$IE_C), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("IE    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$IT_A), n)
after   <- tail(head(f.removeNA(my_data$IT_C), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("IT    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$PT_A), n)
after   <- tail(head(f.removeNA(my_data$PT_C), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("PT    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

########################################################################################################

my_data <- read_excel("data_Table1_8.xls", sheet = "tot conn ban")

before  <- tail(f.removeNA(my_data$est_by_A), n)
after   <- tail(head(f.removeNA(my_data$est_by_C), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("bonds ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")
cat("\n")

########################################################################################################

my_data <- read_excel("data_Table1_8.xls", sheet = "tot conn ISDA")

before  <- tail(f.removeNA(my_data$est_D), n)
after   <- tail(head(f.removeNA(my_data$est_E), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("total ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

########################################################################################################

my_data <- read_excel("data_Table1_8.xls", sheet = "ctry conn ISDA")

before  <- tail(f.removeNA(my_data$DE_D), n)
after   <- tail(head(f.removeNA(my_data$DE_E), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("DE    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$ES_D), n)
after   <- tail(head(f.removeNA(my_data$ES_E), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("ES    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$FR_D), n)
after   <- tail(head(f.removeNA(my_data$FR_E), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("FR    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$IE_D), n)
after   <- tail(head(f.removeNA(my_data$IE_E), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("IE    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$IT_D), n)
after   <- tail(head(f.removeNA(my_data$IT_E), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("IT    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

before  <- tail(f.removeNA(my_data$PT_D), n)
after   <- tail(head(f.removeNA(my_data$PT_E), n+n_window), n )
outp    <- t.test(before, after, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("PT    ")
toprint <- c(mean(before), sd(before), mean(after), sd(after), outp$p.value, 100*( round(mean(after),2)-round(mean(before),2) )/round(mean(before),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

sink()