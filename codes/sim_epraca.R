source("codes/functions.R")
set.seed(123)

load("data-raw/data.RData")

data <- data[str_sub(poz_kodZawodu, 1, 1) != "0"]
data <- data[, c("poz_kodZawodu", "poz_lWolnychMiejsc")]
data[, poz_kodZawodu := as.factor(str_sub(poz_kodZawodu, 1, 1))]
setnames(data, old = c("poz_kodZawodu", "poz_lWolnychMiejsc"), new = c("kod", "wakaty"))

true_wakaty <- sum(data[["wakaty"]])

p_1_s <- c(0.4, 0.5, 0.4, 0.3, 0.4, 0.5, 0.4, 0.3, 0.4)
p_2_s <- c(0.3, 0.4, 0.5, 0.4, 0.3, 0.4, 0.5, 0.4, 0.3)

results_1 <- data.table(
  true_total_wakaty = numeric(),
  est_total_wakaty = numeric(),
  est_total_wakaty_all = numeric(),
  est_total_wakaty_brglm = numeric(),
  est_total_wakaty_all_brglm = numeric()
)

for (i in 1:1000) {
  res <- sim_epraca(data, p_1_s, p_2_s, 0.25)
  results_1 <- rbind(results_1, res)
}

result_epraca_1 <- calculate_metrics_epraca(results_1)

# Scenario II

p_1_s <- p_1_s - 0.2
p_2_s <- p_2_s - 0.2

results_2 <- data.table(
  true_total_wakaty = numeric(),
  est_total_wakaty = numeric(),
  est_total_wakaty_all = numeric(),
  est_total_wakaty_brglm = numeric(),
  est_total_wakaty_all_brglm = numeric()
)

for (i in 1:1000) {
  res <- sim_epraca(data, p_1_s, p_2_s, 0.25)
  results_2 <- rbind(results_2, res)
}

result_epraca_2 <- calculate_metrics_epraca(results_2)
