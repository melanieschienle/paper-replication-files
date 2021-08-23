library("readxl")

alt = "two.sided"

# redirect output
file.remove("data_Table2.txt")
Sys.sleep(1)
sink(file = "data_Table2.txt", append = TRUE, type = c("output", "message"), split = FALSE)

f.removeNA <-function(data){
  data <- data[!is.na(data)]
  return(data)
}

n=200
n_window =180

########################################################################################################

my_data <- read_excel("data_Table2.xls", sheet = "total_con_ban")

# event A

before1 <- tail(f.removeNA(my_data$est_before_A), n)
after1  <- tail(head(f.removeNA(my_data$est_before_B), n+n_window), n )
outp1   <- t.test(before1, after1, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("A Agreement       ")
toprint <- c(mean(before1), sd(before1), mean(after1), sd(after1), outp1$p.value, 100*( round(mean(after1),2)-round(mean(before1),2) )/round(mean(before1),2)  )
cat(sprintf("%.2f\t", toprint))
cat("\n")

# event B

before1 <- tail(f.removeNA(my_data$est_before_B), n)
after1  <- tail(head(f.removeNA(my_data$est_before_9), n+n_window), n )
outp1   <- t.test(before1, after1, alternative = alt, paired = FALSE, var.equal = FALSE)

# event AB

before2 <- tail(f.removeNA(my_data$est_before_A), n)
after2  <- tail(head(f.removeNA(my_data$est_before_9), n+n_window), n )
outp2   <- t.test(before2, after2, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("B Announcement    ")
toprint <- c(mean(before1), sd(before1), mean(after1), sd(after1), outp1$p.value, 100*( round(mean(after1),2)-round(mean(before1),2) )/round(mean(before1),2), outp2$p.value, 100*( round(mean(after2),2)-round(mean(before2),2) )/round(mean(before2),2)    )
cat(sprintf("%.2f\t", toprint))
cat("\n")

# event 9

before1 <- tail(f.removeNA(my_data$est_before_9), n)
after1  <- tail(head(f.removeNA(my_data$est_before_C), n+n_window), n )
outp1   <- t.test(before1, after1, alternative = alt, paired = FALSE, var.equal = FALSE)

# event A9

before2 <- tail(f.removeNA(my_data$est_before_A), n)
after2  <- tail(head(f.removeNA(my_data$est_before_C), n+n_window), n )
outp2   <- t.test(before2, after2, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("9 Draghi          ")
toprint <- c(mean(before1), sd(before1), mean(after1), sd(after1), outp1$p.value, 100*( round(mean(after1),2)-round(mean(before1),2) )/round(mean(before1),2), outp2$p.value, 100*( round(mean(after2),2)-round(mean(before2),2) )/round(mean(before2),2)      )
cat(sprintf("%.2f\t", toprint))
cat("\n")

# event C

before1 <- tail(f.removeNA(my_data$est_before_C), n)
after1  <- tail(head(f.removeNA(my_data$est_after_C), n+n_window), n )
outp1   <- t.test(before1, after1, alternative = alt, paired = FALSE, var.equal = FALSE)

# event AC

before2 <- tail(f.removeNA(my_data$est_before_A), n)
after2  <- tail(head(f.removeNA(my_data$est_after_C), n+n_window), n )
outp2   <- t.test(before2, after2, alternative = alt, paired = FALSE, var.equal = FALSE)

cat("C Implementation  ")
toprint <- c(mean(before1), sd(before1), mean(after1), sd(after1), outp1$p.value, 100*( round(mean(after1),2)-round(mean(before1),2) )/round(mean(before1),2), outp2$p.value, 100*( round(mean(after2),2)-round(mean(before2),2) )/round(mean(before2),2)      )
cat(sprintf("%.2f\t", toprint))
cat("\n")
cat("\n")

sink()