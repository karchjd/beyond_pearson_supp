library(HHG)

hhg_kde_test <- function(x, y, R = 1000) {
  dist_x <- as.matrix(dist(x, diag = TRUE, upper = TRUE))
  dist_y <- as.matrix(dist(y, diag = TRUE, upper = TRUE))
  res <- hhg.test(dist_x, dist_y, nr.perm = R)
  p_value_hhg <- res$perm.pval.hhg.sl
  p_value_pearson <- cor.test(x, y, method = "pearson")$p.value
  p_value <- min(p_value_hhg, p_value_pearson) * 2
  return(p_value)
}
