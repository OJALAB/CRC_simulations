library(foreach)
library(doParallel)

source("codes/functions.R")
set.seed(123)

load("data-raw/data.RData")

data <- data[str_sub(poz_kodZawodu, 1, 1) != "0"]
data <- data[, c("poz_kodZawodu", "poz_lWolnychMiejsc", "prac_kodWojewodztwa")]
data[, poz_kodZawodu := as.factor(str_sub(poz_kodZawodu, 1, 1))]
data[, prac_kodWojewodztwa := as.factor(prac_kodWojewodztwa)]
setnames(data, old = c("poz_kodZawodu", "poz_lWolnychMiejsc", "prac_kodWojewodztwa"), new = c("kod", "wakaty", "woj"))

true_wakaty <- sum(data[["wakaty"]])

p_1_s <- c(0.4, 0.5, 0.4, 0.3, 0.4, 0.5, 0.4, 0.3, 0.4)
p_2_s <- c(0.3, 0.4, 0.5, 0.4, 0.3, 0.4, 0.5, 0.4, 0.3)

cl <- makeCluster(10)
registerDoParallel(cl)

clusterEvalQ(cl, {
  source("codes/functions.R")
})

clusterSetRNGStream(cl, 123)

# Scenario I

# results_1 <- data.table(
#   true_total_wakaty = numeric(),
#   est_total_wakaty = numeric(),
#   est_total_wakaty_brglm = numeric(),
#   est_total_wakaty_reg = numeric(),
#   est_total_wakaty_brglm_reg = numeric()
# )

# for (i in 1:1000) {
#   res <- sim_epraca_reg(data, p_1_s, p_2_s, 0.1)
#   results_1 <- rbind(results_1, res)
# }

results_1 <- foreach(i = 1:1000, .combine = rbind, .packages = "data.table") %dopar% {
  sim_epraca_reg(data, p_1_s, p_2_s, 0.1)
}

result_epraca_reg_1 <- calculate_metrics_epraca_reg(results_1)

# Scenario 2

# results_2 <- data.table(
#   true_total_wakaty = numeric(),
#   est_total_wakaty = numeric(),
#   est_total_wakaty_brglm = numeric(),
#   est_total_wakaty_reg = numeric(),
#   est_total_wakaty_brglm_reg = numeric()
# )
# 
# for (i in 1:1000) {
#   res <- sim_epraca_reg(data, p_1_s, p_2_s, 0.25)
#   results_2 <- rbind(results_2, res)
# }

results_2 <- foreach(i = 1:1000, .combine = rbind, .packages = "data.table") %dopar% {
  sim_epraca_reg(data, p_1_s, p_2_s, 0.25)
}

result_epraca_reg_2 <- calculate_metrics_epraca_reg(results_2)

stopCluster(cl)
