library(data.table)
library(extraDistr)

simulation_norm <- function(N, p_1, p_2) {
  
  values <- data.table(
    value = rnorm(n = N, mean = 175, sd = 10)
  )
  values[, id := .I]
  
  indices_1 <- rbinom(n = N, size = 1, prob = p_1)
  indices_2 <- rbinom(n = N, size = 1, prob = p_2)
  A <- values[as.logical(indices_1)]
  B <- values[as.logical(indices_2)]
  inter <- values[id %in% intersect(A[["id"]], B[["id"]])]
  
  est <- sum(A[["value"]]) * sum(B[["value"]]) / sum(inter[["value"]])
  
  list(est = est, sum = sum(values[["value"]]))
  
}

simulation_ztpois <- function(N, p_1, p_2) {
  
  values <- data.table(
    value = rtpois(n = N, lambda = 5, a = 0)
  )
  values[, id := .I]
  
  indices_1 <- rbinom(n = N, size = 1, prob = p_1)
  indices_2 <- rbinom(n = N, size = 1, prob = p_2)
  A <- values[as.logical(indices_1)]
  B <- values[as.logical(indices_2)]
  inter <- values[id %in% intersect(A[["id"]], B[["id"]])]
  
  est <- sum(A[["value"]]) * sum(B[["value"]]) / sum(inter[["value"]])
  
  list(est = est, sum = sum(values[["value"]]))
  
}

simulation_ztpois_out <- function(N, p_1, p_2) {
  
  values <- data.table(
    value = rtpois(n = N, lambda = 10, a = 0)
  )
  values[, id := .I]
  is_outlier <- runif(N) < 0.05
  values[["value"]][is_outlier] <- rtpois(n = sum(is_outlier), lambda = 1000, a = 0) 

  indices_1 <- rbinom(n = N, size = 1, prob = p_1)
  indices_2 <- rbinom(n = N, size = 1, prob = p_2)
  A <- values[as.logical(indices_1)]
  B <- values[as.logical(indices_2)]
  inter <- values[id %in% intersect(A[["id"]], B[["id"]])]
  
  est <- sum(A[["value"]]) * sum(B[["value"]]) / sum(inter[["value"]])
  
  list(est = est, sum = sum(values[["value"]]))
  
}
