source("codes/functions.R")
set.seed(1)

N_s <- c(1000, 10000, 100000)
p_s <- list(c(0.1, 0.1), c(0.1, 0.25), c(0.1, 0.5), c(0.1, 0.75), c(0.25, 0.25),
            c(0.25, 0.5), c(0.25, 0.75), c(0.5, 0.5), c(0.5, 0.75), c(0.75, 0.75))

for (N in N_s) {
  
  results_final <- list()
  
  for (i in 1: 100) {
    
    results <- data.frame(p_1 = c(), p_2 = c(), est = c())
    for (p in p_s) {
      result <- simulation(N, p[1], p[2])
      results <- rbind(results, c(p[1],  p[2], result))
    }
    colnames(results) <- c("p_1", "p_2", "est")
    results_final[[i]] <- results
    
  }
  
  ests <- Reduce(`+`, lapply(results_final, function(df) df[["est"]])) / 100
  assign(paste0("table_", as.integer(N)), 
         data.table(
           p_1 = results_final[[1]][["p_1"]],
           p_2 = results_final[[1]][["p_2"]],
           est_mean = ests / N)
         )
  
}
