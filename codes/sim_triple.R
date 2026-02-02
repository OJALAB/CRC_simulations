library(foreach)
library(doParallel)

source("codes/functions.R")
source("codes/functions_triple.R")

load("data-raw/data.RData")

data <- data[str_sub(poz_kodZawodu, 1, 1) != "0"]
data <- data[, c("poz_kodZawodu", "poz_lWolnychMiejsc", "prac_kodWojewodztwa")]
data[, poz_kodZawodu := as.factor(str_sub(poz_kodZawodu, 1, 1))]
data[, prac_kodWojewodztwa := as.factor(prac_kodWojewodztwa)]
setnames(data, old = c("poz_kodZawodu", "poz_lWolnychMiejsc", "prac_kodWojewodztwa"), new = c("kod", "wakaty", "woj"))

true_wakaty <- sum(data[["wakaty"]])

p_1_s <- c(0.2, 0.1, 0.2, 0.3, 0.2, 0.1, 0.2, 0.3, 0.2)
p_2_s <- c(0.4, 0.5, 0.4, 0.3, 0.4, 0.5, 0.4, 0.3, 0.4) - 0.1
p_3_s <- c(0.3, 0.4, 0.5, 0.4, 0.3, 0.4, 0.5, 0.4, 0.3) - 0.1

cl <- makeCluster(10)
registerDoParallel(cl)

clusterEvalQ(cl, {
  source("codes/functions.R")
  source("codes/functions_triple.R")
})

clusterSetRNGStream(cl, 123)

# Scenario I

results_1 <- foreach(i = 1:1000, .combine = rbind, .packages = "data.table") %dopar% {
  sim_triple(data, p_1_s, p_2_s, p_3_s, 0.1)
}

result_epraca_triple_1 <- calculate_metrics_epraca_reg(results_1)

# Scenario 2

results_2 <- foreach(i = 1:1000, .combine = rbind, .packages = "data.table") %dopar% {
  sim_triple(data, p_1_s, p_2_s, p_3_s, 0.25)
}

result_epraca_triple_2 <- calculate_metrics_epraca_reg(results_2)

stopCluster(cl)
