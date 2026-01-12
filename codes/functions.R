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

simulation_w_imp <- function(N, p_1_s, p_2_s) {
  
  df <- data.table(X = sample(c("a", "b", "c"), size = N, replace = TRUE),
                   Y = sample(c("e", "f", "g"), size = N, replace = TRUE))
  params <- data.table(
    X = rep(c("a", "b", "c"), each = 3),
    Y = rep(c("e", "f", "g"), times = 3),
    lambda_val = c(2.5, 3, 3.5, 4, 4.5, 5, 10, 50, 100)
  )
  df <- merge(df, params, by = c("X", "Y"), sort = FALSE)
  df[, Z := rtpois(n = N, lambda = lambda_val, a = 0)]
  df[, lambda_val := NULL]
  
  params_probs <- data.table(
    X = rep(c("a", "b", "c"), each = 3),
    Y = rep(c("e", "f", "g"), times = 3),
    prob_1 = p_1_s, 
    prob_2 = p_2_s
  )
  df <- merge(df, params_probs, by = c("X", "Y"), sort = FALSE)
  df[, I1 := rbinom(n = .N, size = 1, prob = prob_1)]
  df[, I2 := rbinom(n = .N, size = 1, prob = prob_2)]
  df[, prob_1 := NULL]
  df[, prob_2 := NULL]
  
  df[, Z_true := Z]
  to_remove <- sample(1:N, size = floor(0.25 * N))
  df[to_remove, Z := NA]
  observed_means <- df[I1 == 1 | I2 == 1, .(mean_obs = mean(Z, na.rm = TRUE)), by = .(X, Y)]
  df <- merge(df, observed_means, by = c("X", "Y"), all.x = TRUE, sort = FALSE)
  df[is.na(Z) & (I1 == 1 | I2 == 1), Z := mean_obs]
  df[, mean_obs := NULL]

  true_total_Z <- sum(df[["Z_true"]])
  
  df_observed <- df[I1 == 1 | I2 == 1]
  df_agg <- df_observed[, .(n_imp = sum(Z), n_true = sum(Z_true), count = .N), by = .(X, Y, I1, I2)]
  df_agg[, I1 := as.factor(I1)]
  df_agg[, I2 := as.factor(I2)]
  df_hidden <- data.table(expand.grid(
    X = c("a", "b", "c"),
    Y = c("e", "f", "g"),
    I1 = factor("0", levels = c("0", "1")),
    I2 = factor("0", levels = c("0", "1"))
  ))
  
  model_imp <- glm(n_imp ~ (I1 + I2) * (X + Y), 
                   family = quasipoisson(), 
                   data = df_agg)
  model_true <- glm(n_true ~ (I1 + I2) * (X + Y), 
                    family = quasipoisson(), 
                    data = df_agg)
  model_count <- glm(count ~ (I1 + I2) * (X + Y),
                     family = quasipoisson(),
                     data = df_agg)
  
  df_hidden[, Z_pred_imp := predict(model_imp, newdata = df_hidden, type = "response")]
  df_hidden[, Z_pred_true := predict(model_true, newdata = df_hidden, type = "response")]
  df_hidden[, n_pred := predict(model_count, newdata = df_hidden, type = "response")]
  
  df_hidden <- merge(df_hidden, observed_means, by = c("X", "Y"), all.x = TRUE, sort = FALSE)
  df_hidden[, Z_pred_from_count := n_pred * mean_obs]
  
  est_total_Z_imp <- sum(df_observed[["Z"]]) + sum(df_hidden[["Z_pred_imp"]])
  est_total_Z_true <- sum(df_observed[["Z_true"]]) + sum(df_hidden[["Z_pred_true"]])
  est_total_Z_from_count <- sum(df_observed[["Z"]]) + sum(df_hidden[["Z_pred_from_count"]])
  
  data.table(true_total_Z = true_total_Z,
             est_total_Z_true = est_total_Z_true,
             est_total_Z_imp = est_total_Z_imp,
             est_total_Z_from_count = est_total_Z_from_count)
  
}

calculate_metrics <- function(df_results) {
  
  metrics_list <- list()
  
  for (est in c("est_total_Z_true", "est_total_Z_imp", "est_total_Z_from_count")) {
    error <- df_results[[est]] - df_results[["true_total_Z"]]
    
    bias <- mean(error)
    rel_bias <- mean(error / df_results[["true_total_Z"]])
    rmse <- sqrt(mean(error^2))
    mean_value <- mean(df_results[[est]])
    
    metrics_list[[est]] <- data.table(
      method = est,
      mean_value = mean_value,
      mean_bias = bias,
      mean_rel_bias = rel_bias * 100,
      rmse = rmse
    )
  }
  
  metrics_list[["true_total_Z"]] <- data.table(
    method = "true_total_z",
    mean_value = mean(df_results[["true_total_Z"]]),
    mean_bias = 0,
    mean_rel_bias = 0,
    rmse = 0
  )
  
  rbindlist(metrics_list)
  
}
