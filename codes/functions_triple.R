library(data.table)
library(brglm2)
library(gamlss)
library(gamlss.cens)
library(gamlss.dist)
library(ranger)
library(xgboost)

sim_triple <- function(df, p_easy, p_hard, prob_hard_vec, cens_frac, Pi, val_sample_size) {
  
  # relevel kod
  df[, kod := relevel(kod, ref = "2")]
  df[, woj := relevel(woj, ref = "22")]
  
  # generate ML labels
  df <- generate_labels(df, Pi)
  
  # calculate empirical distribution of kod
  kod_dist <- prop.table(table(df[["kod"]]))
  
  # generate validation sample
  df_val <- data.table(
    kod = factor(
      sample(
        x = names(kod_dist),
        size = val_sample_size,
        replace = TRUE,
        prob = as.numeric(kod_dist)
      ),
      levels = levels(df[["kod"]])
    )
  )
  df_val <- generate_labels(df_val, Pi)
  
  # estimate Pi using the validation sample
  Pi_est <- estimate_Pi(df_val)
  
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
  
  # prepare all combinations of kod values and observed capture histories for the EM algorithm
  all_combinations_ml <- CJ(
    kod_ml = factor(levels(df[["kod"]]), levels = levels(df[["kod"]])), 
    I1 = factor(c("0", "1"), levels = c("0", "1")),
    I2 = factor(c("0", "1"), levels = c("0", "1")),
    I3 = factor(c("0", "1"), levels = c("0", "1"))
  )
  all_combinations_ml <- all_combinations_ml[!(I1 == "0" & I2 == "0" & I3 == "0")]
  df_agg_ml <- df_observed[, .(count = .N), by = .(kod_ml, I1 = as.factor(I1), I2 = as.factor(I2), I3 = as.factor(I3))]
  df_agg_ml <- df_agg_ml[all_combinations_ml, on = .(kod_ml, I1, I2, I3)]
  df_agg_ml[is.na(count), count := 0]
  
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
  pred_lca <- baffour_em(df_agg)
  
  # perform a double EM algorithm with known confusion matrix
  pred_lca_misclass_known <- baffour_misclass_em(df_agg_ml, Pi)
  
  # perform a double EM algorithm with estimated confusion matrix
  pred_lca_misclass_est <- baffour_misclass_em(df_agg_ml, Pi_est)
  
  # change colnames
  setnames(pred_lca_misclass_known, "n_pred_lca", "n_pred_total_misclass_known")
  setnames(pred_lca_misclass_est, "n_pred_lca", "n_pred_total_misclass_est")
  
  # obtain predictions for the hidden populations
  df_hidden[, n_pred := predict(model_count, newdata = df_hidden, type = "response")]
  df_hidden[, n_pred_brglm := predict(model_count_brglm, newdata = df_hidden, type = "response")]
  df_hidden <- merge(df_hidden, pred_lca, by = "kod", all.x = TRUE, sort = FALSE)
  df_hidden <- merge(df_hidden, pred_lca_misclass_known, by = "kod", all.x = TRUE, sort = FALSE)
  df_hidden <- merge(df_hidden, pred_lca_misclass_est, by = "kod", all.x = TRUE, sort = FALSE)

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
  df_hidden[, wakaty_pred_lca_misclass_known := n_pred_total_misclass_known * mean_obs]
  df_hidden[, wakaty_pred_lca_misclass_known_reg := n_pred_total_misclass_known * mean_pred]
  df_hidden[, wakaty_pred_lca_misclass_est := n_pred_total_misclass_est * mean_obs]
  df_hidden[, wakaty_pred_lca_misclass_est_reg := n_pred_total_misclass_est * mean_pred]
  est_total_wakaty_lca_misclass_known <- sum(df_hidden[["wakaty_pred_lca_misclass_known"]])
  est_total_wakaty_lca_misclass_known_reg <- sum(df_hidden[["wakaty_pred_lca_misclass_known_reg"]])
  est_total_wakaty_lca_misclass_est <- sum(df_hidden[["wakaty_pred_lca_misclass_est"]])
  est_total_wakaty_lca_misclass_est_reg <- sum(df_hidden[["wakaty_pred_lca_misclass_est_reg"]])
  
  # calculate the true population size
  true_n <- NROW(df)
  
  # calculate the observed population size
  obs_n <- NROW(df_observed)
  
  # calculate the estimated population size
  est_n <- obs_n + sum(df_hidden[["n_pred"]])
  est_n_brglm <- obs_n + sum(df_hidden[["n_pred_brglm"]])
  est_n_lca <- obs_n + sum(df_hidden[["n_pred_lca"]])
  est_n_lca_misclass_known <- sum(df_hidden[["n_pred_total_misclass_known"]])
  est_n_lca_misclass_est <- sum(df_hidden[["n_pred_total_misclass_est"]])
  
  data.table(true_total_wakaty = true_total_wakaty,
             est_total_wakaty = est_total_wakaty,
             est_total_wakaty_brglm = est_total_wakaty_brglm,
             est_total_wakaty_reg = est_total_wakaty_reg,
             est_total_wakaty_brglm_reg = est_total_wakaty_brglm_reg,
             est_total_wakaty_lca = est_total_wakaty_lca,
             est_total_wakaty_lca_reg = est_total_wakaty_lca_reg,
             est_total_wakaty_lca_misclass_known = est_total_wakaty_lca_misclass_known,
             est_total_wakaty_lca_misclass_known_reg = est_total_wakaty_lca_misclass_known_reg,
             est_total_wakaty_lca_misclass_est = est_total_wakaty_lca_misclass_est,
             est_total_wakaty_lca_misclass_est_reg = est_total_wakaty_lca_misclass_est_reg,
             true_n = true_n,
             est_n = est_n,
             est_n_brglm = est_n_brglm,
             est_n_lca = est_n_lca,
             est_n_lca_misclass_known = est_n_lca_misclass_known,
             est_n_lca_misclass_est = est_n_lca_misclass_est)
  
}

generate_labels <- function(df, Pi) {
  
  df[, kod_ml := {
    g <- as.character(kod[1])
    as.character(
      sample(
        x = colnames(Pi),
        size = .N,
        replace = TRUE,
        prob = Pi[g, ]
      )
    )
  }, by = .(kod)]
  
  df[, kod_ml := factor(kod_ml, levels = levels(df[["kod"]]))]
  
  df
  
}

estimate_Pi <- function(df) {
  
  count_table <- table(df[["kod"]], df[["kod_ml"]])
  Pi_hat <- prop.table(count_table, margin = 1)
  as.matrix(Pi_hat)
  
}

baffour_em <- function(df, latent_classes = 2, tol = 1e-4, max_iter = 1000, use_ML = FALSE) {
  
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
  if (use_ML) {
    X_matrix <- model.matrix(~ I1 + I2 + I3 + kod + X - 1, data = data_em)
    dtrain <- xgb.DMatrix(data = X_matrix, label = data_em$em_count)
    params <- list(
      objective = "count:poisson",
      eval_metric = "poisson-nloglik",
      min_child_weight = 0,
      eta = 0.01,
      max_depth = 5,
      nthread = 1,
      tree_method = "hist"
    )
  }
  last_coef <- NULL
  last_mu <- NULL
  converged <- FALSE
  iter <- 0

  # run the EM algorithm loop
  while (!converged && iter < max_iter) {
    
    iter <- iter + 1
    
    # M-step
    
    if (use_ML) {
      
      # train an XGBoost model
      setinfo(dtrain, "label", data_em$em_count)
      model_xgb <- xgb.train(params = params, data = dtrain, nrounds = 50, verbose = 0)
      
      # obtain predictions from the XGBoost model
      data_em[, mu_pred := predict(model_xgb, newdata = X_matrix)]
      
      # check convergence
      current_mu <- data_em$mu_pred
      if (!is.null(last_mu)) {
        diff <- max(abs(current_mu - last_mu))
        if (diff < tol) converged <- TRUE
      }
      last_mu <- current_mu
      
    } else {
      
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
      
    }
    
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

baffour_misclass_em <- function(df, Pi, latent_classes = 2, tol = 1e-4, max_iter = 2000) {
  
  Pi_df <- as.data.table(as.table(Pi))
  setnames(Pi_df, c("kod", "kod_ml", "pi_prob"))
  Pi_df[, kod := as.factor(kod)]
  Pi_df[, kod_ml := as.factor(kod_ml)]
  
  obs_data <- df[!(I1 == "0" & I2 == "0" & I3 == "0"), .(kod_ml, I1, I2, I3, count)]
  
  data_em <- CJ(
    kod = factor(levels(Pi_df[["kod"]]), levels = levels(Pi_df[["kod"]])),
    I1 = factor(c("0", "1"), levels = c("0", "1")),
    I2 = factor(c("0", "1"), levels = c("0", "1")),
    I3 = factor(c("0", "1"), levels = c("0", "1")),
    X = factor(1:latent_classes)
  )
  data_em[, is_missing_cell := (I1 == "0" & I2 == "0" & I3 == "0")]
  
  true_grid <- CJ(
    kod = factor(levels(Pi_df[["kod"]]), levels = levels(Pi_df[["kod"]])),
    X = factor(1:latent_classes)
  )
  
  obs_data[, temp := 1]
  true_grid[, temp := 1]
  E_matrix <- merge(obs_data, true_grid, by = "temp", allow.cartesian = TRUE)
  E_matrix[, temp := NULL]
  
  E_matrix <- merge(E_matrix, Pi_df, by = c("kod", "kod_ml"), all.x = TRUE)
  
  E_matrix[, base_n_hat := count * pi_prob]
  
  init_counts <- E_matrix[, .(base_count = sum(base_n_hat)), by = .(kod, I1, I2, I3, X)]
  data_em <- merge(data_em, init_counts, by = c("kod", "I1", "I2", "I3", "X"), all.x = TRUE)
  data_em[is.na(base_count), base_count := 0]
  
  random_split <- runif(nrow(data_em)/latent_classes, 0.45, 0.55)
  data_em[X == "1", em_count := base_count * random_split]
  data_em[X == "2", em_count := base_count * (1 - random_split)]
  
  data_em[is_missing_cell == TRUE, em_count := 1]
  
  data_em[, base_count := NULL]
  E_matrix[, base_n_hat := NULL]
  
  last_coef <- NULL
  converged <- FALSE
  iter <- 0
  
  while (!converged && iter < max_iter) {
    
    iter <- iter + 1
    
    model <- glm(em_count ~ (I1 + I2 + I3) * X + kod * X + I1:I2 + I1:I3,
                 family = quasipoisson(),
                 data = data_em)
    
    data_em[, mu_pred := fitted(model)]
    current_coef <- coef(model)
    
    if(!is.null(last_coef)) {
      diff <- max(abs(current_coef - last_coef))
      if (diff < tol) converged <- TRUE
    }
    last_coef <- current_coef
    
    if (converged) break
    
    if ("mu_pred" %in% names(E_matrix)) E_matrix[, mu_pred := NULL]
    mu_to_merge <- data_em[, .(I1, I2, I3, kod, X, mu_pred)]
    E_matrix <- merge(E_matrix, mu_to_merge, by = c("I1", "I2", "I3", "kod", "X"), sort = FALSE)
    
    E_matrix[, num := pi_prob * mu_pred]
    E_matrix[, den := sum(num), by = .(I1, I2, I3, kod_ml)]
    E_matrix[, tau := ifelse(den > 0, num / den, 0)]
    
    E_matrix[, n_hat := count * tau]
    
    agg_n_hat <- E_matrix[, .(new_em_count = sum(n_hat)), by = .(I1, I2, I3, kod, X)]
    
    data_em <- merge(data_em, agg_n_hat, by = c("I1", "I2", "I3", "kod", "X"), all.x = TRUE)
    
    data_em[!is.na(new_em_count), em_count := new_em_count]
    data_em[, new_em_count := NULL]
    
    data_em[is_missing_cell == TRUE, em_count := mu_pred]
    
  }
  
  group_preds <- data_em[,
                         .(n_pred_lca = sum(em_count)),
                         by = .(kod)]
  
  group_preds
  
}

calculate_metrics_triple <- function(df_results) {
  
  metrics_list <- list()
  
  for (est in c("est_total_wakaty",
                "est_total_wakaty_brglm",
                "est_total_wakaty_reg",
                "est_total_wakaty_brglm_reg",
                "est_total_wakaty_lca",
                "est_total_wakaty_lca_reg",
                "est_total_wakaty_lca_misclass_known",
                "est_total_wakaty_lca_misclass_known_reg",
                "est_total_wakaty_lca_misclass_est",
                "est_total_wakaty_lca_misclass_est_reg")) {
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
  
  metrics_wakaty <- rbindlist(metrics_list)
  
  metrics_list <- list()
  
  for (est in c("est_n",
                "est_n_brglm",
                "est_n_lca",
                "est_n_lca_misclass_known",
                "est_n_lca_misclass_est")) {
    error <- df_results[[est]] - df_results[["true_n"]]
    
    bias <- mean(error)
    rel_bias <- mean(error / df_results[["true_n"]])
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
  
  metrics_n <- rbindlist(metrics_list)
  
  list(metrics_wakaty, metrics_n)  
}
