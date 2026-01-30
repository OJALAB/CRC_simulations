library(maxLik)
library(extraDistr)

logLik_ZTCP <- function(beta, y, X, status) {
  lambda <- as.vector(exp(X %*% beta))
  
  lambda <- pmax(lambda, 1e-10)
  lambda <- pmin(lambda, 1e100)
  
  ll_exact <- dtpois(y, lambda, a = 0, log = TRUE)
  ll_cens <- ptpois(y - 1, lambda, a = 0, lower.tail = FALSE, log.p = TRUE)
  
  loglik_vec <- status * ll_exact + (1 - status) * ll_cens
  loglik_vec[!is.finite(loglik_vec)] <- -1e100
  
  sum(loglik_vec)
}