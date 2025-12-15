source("codes/functions.R")
set.seed(1)

N_s <- c(1000, 10000, 100000)
p_s <- list(c(0.1, 0.1), c(0.1, 0.25), c(0.1, 0.5), c(0.1, 0.75), c(0.25, 0.25),
            c(0.25, 0.5), c(0.25, 0.75), c(0.5, 0.5), c(0.5, 0.75), c(0.75, 0.75))

for (N in N_s) {
  
  results_final <- list()
  
  for (i in 1: 100) {
    
    results <- data.frame(p_1 = c(), p_2 = c(), est = c(), mean = c())
    for (p in p_s) {
      sim <- simulation_norm(N, p[1], p[2])
      result <- sim$est
      true_sum <- sim$sum
      results <- rbind(results, c(p[1],  p[2], result, true_sum))
    }
    colnames(results) <- c("p_1", "p_2", "est", "sum")
    results_final[[i]] <- results
    
  }
  
  ests <- Reduce(`+`, lapply(results_final, function(df) df[["est"]])) / 100
  true_sums <- Reduce(`+`, lapply(results_final, function(df) df[["sum"]])) / 100
  assign(paste0("table_", as.integer(N), "_norm"), 
         data.table(
           p_1 = results_final[[1]][["p_1"]],
           p_2 = results_final[[1]][["p_2"]],
           est_sum = ests,
           true_sum = true_sums)
         )
  
}

for (N in N_s) {
  
  results_final <- list()
  
  for (i in 1: 100) {
    
    results <- data.frame(p_1 = c(), p_2 = c(), est = c(), mean = c())
    for (p in p_s) {
      sim <- simulation_ztpois(N, p[1], p[2])
      result <- sim$est
      true_sum <- sim$sum
      results <- rbind(results, c(p[1],  p[2], result, true_sum))
    }
    colnames(results) <- c("p_1", "p_2", "est", "sum")
    results_final[[i]] <- results
    
  }
  
  ests <- Reduce(`+`, lapply(results_final, function(df) df[["est"]])) / 100
  true_sums <- Reduce(`+`, lapply(results_final, function(df) df[["sum"]])) / 100
  assign(paste0("table_", as.integer(N), "_ztpois"), 
         data.table(
           p_1 = results_final[[1]][["p_1"]],
           p_2 = results_final[[1]][["p_2"]],
           est_sum = ests,
           true_sum = true_sums)
  )
  
}

for (N in N_s) {
  
  results_final <- list()
  
  for (i in 1: 100) {
    
    results <- data.frame(p_1 = c(), p_2 = c(), est = c(), mean = c())
    for (p in p_s) {
      sim <- simulation_ztpois_out(N, p[1], p[2])
      result <- sim$est
      true_sum <- sim$sum
      results <- rbind(results, c(p[1],  p[2], result, true_sum))
    }
    colnames(results) <- c("p_1", "p_2", "est", "sum")
    results_final[[i]] <- results
    
  }
  
  ests <- Reduce(`+`, lapply(results_final, function(df) df[["est"]])) / 100
  true_sums <- Reduce(`+`, lapply(results_final, function(df) df[["sum"]])) / 100
  assign(paste0("table_", as.integer(N), "_ztpois_out"), 
         data.table(
           p_1 = results_final[[1]][["p_1"]],
           p_2 = results_final[[1]][["p_2"]],
           est_sum = ests,
           true_sum = true_sums)
  )
  
}
