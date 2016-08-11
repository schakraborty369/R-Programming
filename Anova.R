My_Anova <- function(Alpha) {
  
  x <- read.csv("Anova.csv")
  df <- lapply(x, function(x) x[!is.na(x)])
  mean_vec <- sapply(df, mean)
  var_vec <- sapply(df, var)
  Grand_mean <- mean(mean_vec)
  count_vec <- sapply(df, length)
  sumofSq_vec <- var_vec * (count_vec -1)
  mean_minus_grandmean_square_vec <- sapply(mean_vec, function(y) (y - Grand_mean)^2)
  BSS_vec <- mean_minus_grandmean_square_vec * count_vec
  BSS <- sum(BSS_vec)
  WSS <- sum(sumofSq_vec)
  df_treatment <- ncol(x) - 1
  N <- sum(sapply(df, length))
  df_error <- N - ncol(x)
  Mean_sq_BSS <- BSS / df_treatment
  Mean_sq_WSS <- WSS / df_error
  Statistic <- Mean_sq_BSS / Mean_sq_WSS
  CV <- 1 - pf(Statistic, df_treatment, df_error)
  
  if (CV > Alpha) {cat("---------------Accept Null Hypothesis---------------\n")}
  else {cat("--------------Reject Null Hypothesis-----------------\n")}
  
  cat ("-------------------------------------------------------\n")
  cat ("BSS = ", BSS, " | WSS = ", WSS, " | TSS = ", BSS+WSS, "\n")
  cat ("-------------------------------------------------------\n")
  cat ("MeanSq_BSS = ", Mean_sq_BSS, " | MeanSq_WSS = ", Mean_sq_WSS, "\n")
  cat ("-------------------------------------------------------\n")
  cat ("p_value =", CV)
  
}


