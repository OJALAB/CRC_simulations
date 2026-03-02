library(stringr)
library(foreach)
library(doParallel)

source("codes/functions_triple.R")

load("data-raw/data.RData")

data <- data[str_sub(poz_kodZawodu, 1, 1) != "0"]
data <- data[, c("poz_kodZawodu", "poz_lWolnychMiejsc", "prac_kodWojewodztwa")]
data[, poz_kodZawodu := as.factor(str_sub(poz_kodZawodu, 1, 1))]
data[, prac_kodWojewodztwa := as.factor(prac_kodWojewodztwa)]
setnames(data, old = c("poz_kodZawodu", "poz_lWolnychMiejsc", "prac_kodWojewodztwa"), new = c("kod", "wakaty", "woj"))

true_wakaty <- sum(data[["wakaty"]])

Pi <- matrix(0.01, nrow = 9, ncol = 9)
rownames(Pi) <- as.character(1:9)
colnames(Pi) <- as.character(1:9)
diag(Pi) <- c(0.90, 0.85, 0.70, 0.75, 0.95, 0.80, 0.80, 0.60, 0.95)
Pi["3", "4"] <- 0.20
Pi["4", "3"] <- 0.15
Pi["8", "7"] <- 0.15
Pi["8", "9"] <- 0.15
Pi["6", "7"] <- 0.10
Pi <- Pi / rowSums(Pi)

cl <- makeCluster(10)
registerDoParallel(cl)

clusterEvalQ(cl, {
  source("codes/functions.R")
  source("codes/functions_triple.R")
})

clusterSetRNGStream(cl, 123)

# Scenario I

results_1 <- foreach(i = 1:1000, .combine = rbind, .packages = "data.table") %dopar% {
  sim_triple(data,
             p_easy = c(0.8, 0.2, 0.1),
             p_hard = c(0.1, 0.8, 0.6),
             prob_hard_vec = c(0.4, 0.3, 0.2, 0.3, 0.4, 0.3, 0.2, 0.3, 0.4),
             cens_frac = 0.1,
             Pi = Pi,
             val_sample_size = 1000)
}

result_epraca_triple_1 <- calculate_metrics_triple(results_1)

# Scenario 2

results_2 <- foreach(i = 1:1000, .combine = rbind, .packages = "data.table") %dopar% {
  sim_triple(data,
             p_easy = c(0.8, 0.2, 0.1),
             p_hard = c(0.1, 0.8, 0.6),
             prob_hard_vec = c(0.4, 0.3, 0.2, 0.3, 0.4, 0.3, 0.2, 0.3, 0.4),
             cens_frac = 0.25,
             Pi = Pi,
             val_sample_size = 1000)
}

result_epraca_triple_2 <- calculate_metrics_triple(results_2)

stopCluster(cl)
