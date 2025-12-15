library(data.table)

simulation <- function(N, p_1, p_2) {
  
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
  
  return(est)
  
}



