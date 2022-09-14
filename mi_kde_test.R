library(mpmi)
library(dplyr)

mi_kde_test <- function(x, y, R = 1000) {
  all_data <- cbind(x, y)
  mi_coeff <- cminjk(all_data)[1, 2]
  perm_vals <- numeric(R)
  n <- nrow(all_data)
  for (i in 1:R) {
    permutation <- sample(n, n, replace = F)
    perm_data <- cbind(x, y[permutation])
    perm_vals[i] <- cminjk(perm_data)[1, 2]
  }
  p_value <- mean(mi_coeff >= perm_vals | near(perm_vals, mi_coeff))
  return(p_value)
}
