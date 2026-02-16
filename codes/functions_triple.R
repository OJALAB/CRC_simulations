library(data.table)
library(brglm2)

sim_triple <- function(df, p_easy, p_hard, prob_hard_vec, cens_frac) {
  
  # relevel kod
  df[, kod := relevel(kod, ref = "2")]
  df[, woj := relevel(woj, ref = "22")]
  
  # assign probs of membership in the second class
  params_probs <- data.table(
    kod = as.factor(1:9),
    prob_is_hard = prob_hard_vec
  )
  
  # assign records to classes and set base capture probabilites
  df <- merge(df, params_probs, by = "kod", sort = FALSE)
  df[, is_hard := rbinom(.N, 1, prob_is_hard)]
  df[, `:=`(
    prob_1 = ifelse(is_hard == 1, p_hard[1], p_easy[1]),
    prob_2 = ifelse(is_hard == 1, p_hard[2], p_easy[2]),
    prob_3 = ifelse(is_hard == 1, p_hard[3], p_easy[3])
  )]
  
  # simulate the first capture occasion
  df[, I1 := rbinom(.N, 1, prob_1)]
  
  # adjust the second capture probabilities and simulate the second capture
  df[, prob_2 := pmax(prob_2 - (I1 * 0.05), 0)]
  df[, I2 := rbinom(.N, 1, prob_2)]
  
  # adjust the third capture probabilities and simulate the third capture
  df[, prob_3 := pmax(prob_3 - (I1 * 0.05), 0)]
  df[, I3 := rbinom(.N, 1, prob_3)]
  
  # remove unnecessary columns and copy true wakaty values
  df[, c("prob_is_hard", "is_hard", "prob_1", "prob_2", "prob_3") := NULL]
  df[, wakaty := as.numeric(wakaty)]
  df[, wakaty_true := wakaty]

  # randomly censor a fraction of records with wakaty >= 2
  to_be_censored <- sample(df[, .I[wakaty >= 2]], size = floor(cens_frac * NROW(df[wakaty >= 2, ])))
  df[, status := 1]
  df[to_be_censored, status := 0]
  df[to_be_censored, wakaty := 2]
  
  # calculate observed means for the observed and uncensored data where wakaty >= 2,
  # separately for each value of kod
  observed_means <- df[(I1 == 1 | I2 == 1 | I3 == 1) & status == 1 & wakaty >= 2, .(mean_obs = mean(wakaty)), by = .(kod)]
  observed_means[is.nan(mean_obs), mean_obs := mean(df[(I1 == 1 | I2 == 1 | I3 == 1) & status == 1 & wakaty >= 2, wakaty])]
  df <- merge(df, observed_means, by = "kod", all.x = TRUE, sort = FALSE)
  df[, wakaty_mean := wakaty]
  df[status == 0, wakaty_mean := mean_obs]
  df[, mean_obs := NULL]
  
  # calculate the actual number of vacancies
  true_total_wakaty <- sum(df[["wakaty_true"]])
  
  # prepare all combinations of kod values and observed capture histories
  all_combinations <- CJ(
    kod = factor(1:9, levels = levels(df$kod)), 
    I1 = factor(c("0", "1"), levels = c("0", "1")),
    I2 = factor(c("0", "1"), levels = c("0", "1")),
    I3 = factor(c("0", "1"), levels = c("0", "1"))
  )
  all_combinations <- all_combinations[!(I1 == "0" & I2 == "0" & I3 == "0")]
  
  # aggregate the observed data
  df_observed <- df[I1 == 1 | I2 == 1 | I3 == 1]
  df_agg <- df_observed[, .(count = .N), by = .(kod, I1 = as.factor(I1), I2 = as.factor(I2), I3 = as.factor(I3))]
  df_agg <- df_agg[all_combinations, on = .(kod, I1, I2, I3)]
  df_agg[is.na(count), count := 0]
  
  # prepare a data frame for the unobserved data
  df_hidden <- data.table(expand.grid(
    kod = as.factor(1:9),
    I1 = factor("0", levels = c("0", "1")),
    I2 = factor("0", levels = c("0", "1")),
    I3 = factor("0", levels = c("0", "1"))
  ))
  
  # fit a standard GLM
  model_count <- glm(count ~ (I1 + I2 + I3) * kod + I1:I2 + I1:I3 + I2:I3,
                     family = quasipoisson(),
                     data = df_agg)
  
  # fit a GLM with BRGLM (bias reduction)
  model_count_brglm <- glm(count ~ (I1 + I2 + I3) * kod + I1:I2 + I1:I3 + I2:I3,
                           family = poisson(),
                           data = df_agg,
                           method = "brglmFit")
  
  # perform an EM algorithm with latent classes
  pred_lca <- baffour_em(df_agg, latent_classes = 2)
  
  # obtain predictions for the hidden populations
  df_hidden[, n_pred := predict(model_count, newdata = df_hidden, type = "response")]
  df_hidden[, n_pred_brglm := predict(model_count_brglm, newdata = df_hidden, type = "response")]
  df_hidden <- merge(df_hidden, pred_lca, by = "kod", all.x = TRUE, sort = FALSE)
  
  # fit a negative binomial GLM to censored wakaty data
  eval(expression(gen.cens(NBI, type = "right")), envir = .GlobalEnv)
  model_wakaty_nb <- gamlss(Surv(wakaty, status) ~ kod + woj,
                            family = get("NBIrc", envir = .GlobalEnv),
                            data = df_observed,
                            trace = FALSE)
  
  # obtain predictions of wakaty from the negative binomial GLM
  mu_pred <- predict(model_wakaty_nb, newdata = df_observed[status == 0], data = df_observed, type = "response", what = "mu")
  sigma_pred <- predict(model_wakaty_nb, newdata = df_observed[status == 0], data = df_observed, type = "response", what = "sigma")
  
  # impute censored values using conditional expectation
  p_0 <- dNBI(0, mu = mu_pred, sigma = sigma_pred)
  p_1 <- dNBI(1, mu = mu_pred, sigma = sigma_pred)
  p_ge_2 <- 1 - (p_0 + p_1)
  df_observed[status == 0, wakaty := (mu_pred - p_1) / p_ge_2]

  # calculate means of wakaty in the groups after imputation
  predicted_means <- df_observed[, .(mean_pred = mean(wakaty)), by =.(kod)]
  predicted_means[is.nan(mean_pred), mean_pred := mean(df_observed[["wakaty"]])]
  
  # calculate means of wakaty in the groups based only on the observed data
  observed_means <- df_observed[, .(mean_obs = mean(wakaty_mean)), by = .(kod)]
  observed_means[is.nan(mean_obs), mean_obs := mean(df_observed[["wakaty_mean"]])]
  
  # merge the means with unobserved data
  df_hidden <- merge(df_hidden, predicted_means, by = "kod", all.x = TRUE, sort = FALSE)
  df_hidden[is.na(mean_pred), mean_pred := mean(df_observed[["wakaty"]])]
  df_hidden <- merge(df_hidden, observed_means, by = "kod", all.x = TRUE, sort = FALSE)
  df_hidden[is.na(mean_obs), mean_obs := mean(df_observed[["wakaty_mean"]])]
  
  # obtain predictions for the number of vacancies in the unobserved population
  df_hidden[, wakaty_pred_reg := n_pred * mean_pred]
  df_hidden[, wakaty_pred_brglm_reg := n_pred_brglm * mean_pred]
  df_hidden[, wakaty_pred := n_pred * mean_obs]
  df_hidden[, wakaty_pred_brglm := n_pred_brglm * mean_obs]
  df_hidden[, wakaty_pred_lca := n_pred_lca * mean_obs]
  df_hidden[, wakaty_pred_lca_reg := n_pred_lca * mean_pred]
  
  # obtain predictions for the total number of vacancies
  est_total_wakaty_reg <- sum(df_observed[["wakaty"]]) + sum(df_hidden[["wakaty_pred_reg"]])
  est_total_wakaty_brglm_reg <- sum(df_observed[["wakaty"]]) + sum(df_hidden[["wakaty_pred_brglm_reg"]])
  est_total_wakaty <- sum(df_observed[["wakaty_mean"]]) + sum(df_hidden[["wakaty_pred"]])
  est_total_wakaty_brglm <- sum(df_observed[["wakaty_mean"]]) + sum(df_hidden[["wakaty_pred_brglm"]])
  est_total_wakaty_lca <- sum(df_observed[["wakaty_mean"]]) + sum(df_hidden[["wakaty_pred_lca"]])
  est_total_wakaty_lca_reg <- sum(df_observed[["wakaty"]]) + sum(df_hidden[["wakaty_pred_lca_reg"]])
  
  data.table(true_total_wakaty = true_total_wakaty,
             est_total_wakaty = est_total_wakaty,
             est_total_wakaty_brglm = est_total_wakaty_brglm,
             est_total_wakaty_reg = est_total_wakaty_reg,
             est_total_wakaty_brglm_reg = est_total_wakaty_brglm_reg,
             est_total_wakaty_lca = est_total_wakaty_lca,
             est_total_wakaty_lca_reg = est_total_wakaty_lca_reg)
  
}

baffour_em <- function(df, latent_classes = 2, tol = 1e-6, max_iter = 1000) {
  
  # prepare all combinations of kod values and observed capture histories
  grid <- CJ(
    kod = unique(df[["kod"]]),
    I1 = factor(c("0", "1"), levels = c("0", "1")),
    I2 = factor(c("0", "1"), levels = c("0", "1")),
    I3 = factor(c("0", "1"), levels = c("0", "1"))
  )
  
  # merge the grid with the observed data
  data_full <- merge(grid, df[, .(kod, I1, I2, I3, count)], 
                     by = c("kod", "I1", "I2", "I3"), 
                     all.x = TRUE)
  
  # handle missing counts and identify the unobserved history
  data_full[is.na(count), count := 0]
  data_full[, is_missing_cell := (I1 == "0" & I2 == "0" & I3 == "0")]
  
  # duplicate rows for each class
  data_em <- data_full[rep(1:.N, each = latent_classes)]
  data_em[, X := factor(rep(1:latent_classes, times = nrow(data_full)))]
  
  # randomly split observed counts between the classes
  random_split <- runif(nrow(data_em)/latent_classes, 0.45, 0.55)
  data_em[X == "1", em_count := count * random_split]
  data_em[X == "2", em_count := count * (1 - random_split)]
  
  # set start values for the unobserved data
  data_em[is_missing_cell == TRUE, em_count := 1]
  
  # set technical parameters
  last_coef <- NULL
  converged <- FALSE
  iter <- 0

  # run the EM algorithm loop
  while (!converged && iter < max_iter) {
    
    iter <- iter + 1
    
    # M-step
    
    # fit a GLM
    model <- glm(em_count ~ (I1 + I2 + I3) * X + kod * X + I1:I2 + I1:I3,
                 family = quasipoisson(),
                 data = data_em)
    
    # obtain predictions from the GLM
    data_em[, mu_pred := fitted(model)]
    current_coef <- coef(model)
    
    # check convergence
    if (!is.null(last_coef)) {
      diff <- max(abs(current_coef - last_coef))
      if (diff < tol) converged <- TRUE
    }
    last_coef <- current_coef
    
    # E-step
    
    # aggregate the data
    data_em[, mu_total := sum(mu_pred), by = .(kod, I1, I2, I3)]
    
    # update the cell frequencies 
    data_em[is_missing_cell == FALSE, em_count := count / mu_total * mu_pred]
    data_em[is_missing_cell == TRUE, em_count := mu_pred]
    
  }
  
  # obtain predictions for the number of vacancies in the unobserved population
  missing_preds <- data_em[is_missing_cell == TRUE,
                           .(n_pred_lca = sum(em_count)),
                           by = .(kod)]
  
  missing_preds

}

calculate_metrics_triple <- function(df_results) {
  
  metrics_list <- list()
  
  for (est in c("est_total_wakaty",
                "est_total_wakaty_brglm",
                "est_total_wakaty_reg",
                "est_total_wakaty_brglm_reg",
                "est_total_wakaty_lca",
                "est_total_wakaty_lca_reg")) {
    error <- df_results[[est]] - df_results[["true_total_wakaty"]]
    
    bias <- mean(error)
    rel_bias <- mean(error / df_results[["true_total_wakaty"]])
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
  
  rbindlist(metrics_list)
  
}
