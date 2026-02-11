sim_triple <- function(df, p_1_s, p_2_s, p_3_s, cens_frac) {
  
  df[, kod := relevel(kod, ref = "2")]
  df[, woj := relevel(woj, ref = "22")]
  
  params_probs <- data.table(
    kod = as.factor(1:9),
    prob_1 = p_1_s,
    prob_2 = p_2_s,
    prob_3 = p_3_s
  )
  
  df <- merge(df, params_probs, by = "kod", sort = FALSE)
  df[, I1 := rbinom(n = .N, size = 1, prob = prob_1)]
  df[I1 == 0, prob_2 := pmin(prob_2 + 0.2, 1)]
  df[, I2 := rbinom(n = .N, size = 1, prob = prob_2)]
  df[I1 == 0, prob_3 := pmin(prob_3 + 0.2, 1)]
  df[, I3 := rbinom(n = .N, size = 1, prob = prob_3)]
  df[, c("prob_1", "prob_2", "prob_3") := NULL]
  
  df[, wakaty := as.numeric(wakaty)]
  df[, wakaty_true := wakaty]
  # candidates_idx <- df[, .I[wakaty >= 2]]
  # candidates_values <- df[wakaty >= 2, wakaty]
  # to_be_censored <- sample(candidates_idx,
  #                          size = floor(cens_frac * NROW(df[wakaty >= 2])),
  #                          prob = 1 / candidates_values)
  to_be_censored <- sample(df[, .I[wakaty >= 2]], size = floor(cens_frac * NROW(df[wakaty >= 2, ])))
  df[, status := 1]
  df[to_be_censored, status := 0]
  df[to_be_censored, wakaty := 2]
  observed_means <- df[(I1 == 1 | I2 == 1 | I3 == 1) & status == 1 & wakaty >= 2, .(mean_obs = mean(wakaty)), by = .(kod)]
  observed_means[is.nan(mean_obs), mean_obs := mean(df[(I1 == 1 | I2 == 1 | I3 == 1) & status == 1 & wakaty >= 2, wakaty])]
  df <- merge(df, observed_means, by = "kod", all.x = TRUE, sort = FALSE)
  df[, wakaty_mean := wakaty]
  df[status == 0, wakaty_mean := mean_obs]
  df[, mean_obs := NULL]
  
  true_total_wakaty <- sum(df[["wakaty_true"]])
  
  all_combinations <- CJ(
    kod = factor(1:9, levels = levels(df$kod)), 
    I1 = factor(c("0", "1"), levels = c("0", "1")),
    I2 = factor(c("0", "1"), levels = c("0", "1")),
    I3 = factor(c("0", "1"), levels = c("0", "1"))
  )
  all_combinations <- all_combinations[!(I1 == "0" & I2 == "0" & I3 == "0")]
  
  df_observed <- df[I1 == 1 | I2 == 1 | I3 == 1]
  df_agg <- df_observed[, .(count = .N), by = .(kod, I1 = as.factor(I1), I2 = as.factor(I2), I3 = as.factor(I3))]
  df_hidden <- data.table(expand.grid(
    kod = as.factor(1:9),
    I1 = factor("0", levels = c("0", "1")),
    I2 = factor("0", levels = c("0", "1")),
    I3 = factor("0", levels = c("0", "1"))
  ))
  df_agg <- df_agg[all_combinations, on = .(kod, I1, I2, I3)]
  df_agg[is.na(count), count := 0]
  
  model_count <- glm(count ~ (I1 + I2 + I3) * kod + I1:I2 + I1:I3,
                     family = quasipoisson(),
                     data = df_agg)
  model_count_brglm <- glm(count ~ (I1 + I2 + I3) * kod + I1:I2 + I1:I3,
                           family = poisson(),
                           data = df_agg,
                           method = "brglmFit")
  
  df_hidden[, n_pred := predict(model_count, newdata = df_hidden, type = "response")]
  df_hidden[, n_pred_brglm := predict(model_count_brglm, newdata = df_hidden, type = "response")]
  
  # model_wakaty <- vglm(SurvS4(wakaty, status, type = "right") ~ kod + woj,
  #                      family = cens.poisson,
  #                      data = df_observed)
  
  # model_wakaty <- vglm(SurvS4(wakaty, status, type = "right") ~ kod,
  #                      family = cens.poisson,
  #                      data = df_observed)
  
  # gen.cens(NBI, type = "right")
  eval(expression(gen.cens(NBI, type = "right")), envir = .GlobalEnv)
  
  model_wakaty_nb <- gamlss(Surv(wakaty, status) ~ kod + woj,
                            family = get("NBIrc", envir = .GlobalEnv),
                            data = df_observed,
                            trace = FALSE)
  
  mu_pred <- predict(model_wakaty_nb, newdata = df_observed[status == 0], data = df_observed, type = "response", what = "mu")
  sigma_pred <- predict(model_wakaty_nb, newdata = df_observed[status == 0], data = df_observed, type = "response", what = "sigma")
  
  p_0 <- dNBI(0, mu = mu_pred, sigma = sigma_pred)
  p_1 <- dNBI(1, mu = mu_pred, sigma = sigma_pred)
  p_ge_2 <- 1 - (p_0 + p_1)
  df_observed[status == 0, wakaty := (mu_pred - p_1) / p_ge_2]
  
  # lambda_pred <- predict(model_wakaty, newdata = df_observed[status == 0], type = "response")
  # p_ge_1 <- ppois(0, lambda_pred, lower.tail = FALSE)
  # p_ge_2 <- ppois(1, lambda_pred, lower.tail = FALSE)
  # df_observed[status == 0, wakaty := lambda_pred * p_ge_1 / p_ge_2]
  
  predicted_means <- df_observed[, .(mean_pred = mean(wakaty)), by =.(kod)]
  predicted_means[is.nan(mean_pred), mean_pred := mean(df_observed[["wakaty"]])]
  
  observed_means <- df_observed[, .(mean_obs = mean(wakaty_mean)), by = .(kod)]
  observed_means[is.nan(mean_obs), mean_obs := mean(df_observed[["wakaty_mean"]])]
  
  df_hidden <- merge(df_hidden, predicted_means, by = "kod", all.x = TRUE, sort = FALSE)
  df_hidden[is.na(mean_pred), mean_pred := mean(df_observed[["wakaty"]])]
  
  df_hidden <- merge(df_hidden, observed_means, by = "kod", all.x = TRUE, sort = FALSE)
  df_hidden[is.na(mean_obs), mean_obs := mean(df_observed[["wakaty_mean"]])]
  
  df_hidden[, wakaty_pred_reg := n_pred * mean_pred]
  df_hidden[, wakaty_pred_brglm_reg := n_pred_brglm * mean_pred]
  df_hidden[, wakaty_pred := n_pred * mean_obs]
  df_hidden[, wakaty_pred_brglm := n_pred_brglm * mean_obs]
  
  est_total_wakaty_reg <- sum(df_observed[["wakaty"]]) + sum(df_hidden[["wakaty_pred_reg"]])
  est_total_wakaty_brglm_reg <- sum(df_observed[["wakaty"]]) + sum(df_hidden[["wakaty_pred_brglm_reg"]])
  est_total_wakaty <- sum(df_observed[["wakaty_mean"]]) + sum(df_hidden[["wakaty_pred"]])
  est_total_wakaty_brglm <- sum(df_observed[["wakaty_mean"]]) + sum(df_hidden[["wakaty_pred_brglm"]])
  
  data.table(true_total_wakaty = true_total_wakaty,
             est_total_wakaty = est_total_wakaty,
             est_total_wakaty_brglm = est_total_wakaty_brglm,
             est_total_wakaty_reg = est_total_wakaty_reg,
             est_total_wakaty_brglm_reg = est_total_wakaty_brglm_reg)
  
}