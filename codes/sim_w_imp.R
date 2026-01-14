source("codes/functions.R")
set.seed(123)

p_1_s <- c(0.3, 0.4, 0.5, 0.4, 0.3, 0.4, 0.5, 0.4, 0.3)
p_2_s <- c(0.4, 0.3, 0.4, 0.5, 0.4, 0.3, 0.4, 0.5, 0.4)

results_1 <- data.table(true_total_Z = numeric(),
                        est_total_Z_true = numeric(),
                        est_total_Z_imp = numeric(),
                        est_total_Z_from_count = numeric(),
                        est_total_Z_from_count_all = numeric())

for (i in 1:1000) {
  res <- simulation_w_imp(10000, p_1_s, p_2_s)
  results_1 <- rbind(results_1, res)
}

result_imp_1 <- calculate_metrics(results_1)

p_1_s <- p_1_s - 0.2
p_2_s <- p_2_s - 0.2

results_2 <- data.table(true_total_Z = numeric(),
                        est_total_Z_true = numeric(),
                        est_total_Z_imp = numeric(),
                        est_total_Z_from_count = numeric(),
                        est_total_Z_from_count_all = numeric())

for (i in 1:1000) {
  res <- simulation_w_imp(10000, p_1_s, p_2_s)
  results_2 <- rbind(results_2, res)
}

result_imp_2 <- calculate_metrics(results_2)
