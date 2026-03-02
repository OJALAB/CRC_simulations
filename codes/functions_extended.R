library(data.table)

source("codes/functions_triple.R")

sim_vac <- function(df,
                    p_easy,
                    p_hard,
                    prob_hard_vec,
                    cens_frac,
                    missing_frac,
                    Pi,
                    val_sample_size,
                    C = 2) {
  
  # relevel kod
  df[, kod := relevel(kod, ref = "2")]
  
  # generate ML labels
  df <- generate_labels(df, Pi)
  
  # calculate the empirical distribution of kod
  kod_dist <- prop.table(table(df[["kod"]]))
  
  # generate a validation sample
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
  
  # filter the observed data
  df_obs <- copy(df[I1 == 1 | I2 == 1 | I3 == 1])
  
  # assign the observed kod values
  df_obs[, kod_obs := kod_ml]
  df_obs[I1 == 1, kod_obs := kod]
  
  # copy true wakaty values
  df_obs[, wakaty := as.numeric(wakaty)]
  df_obs[, wakaty_true := wakaty]
  df_obs[, v_status := "exact"]
  
  # generate missing data
  df_obs[I1 == 0, rand_val := runif(.N)]
  df_obs[I1 == 0 & rand_val <= missing_frac, v_status := "missing"]
  df_obs[v_status == "missing", wakaty := NA]
  
  # generate censored data
  df_obs[I1 == 0 & wakaty_true >= C &
           rand_val > missing_frac & rand_val <= (missing_frac + cens_frac),
         v_status := "censored"]
  df_obs[v_status == "censored", wakaty := C]
  
  # remove the unnecessary column
  df_obs[, rand_val := NULL]
  
  # prepare all combinations of kod values and observed capture histories
  # for the naive model
  all_combinations_naive <- CJ(
    kod_obs = factor(levels(df[["kod"]]), levels = levels(df[["kod"]])), 
    I1 = factor(c("0", "1"), levels = c("0", "1")),
    I2 = factor(c("0", "1"), levels = c("0", "1")),
    I3 = factor(c("0", "1"), levels = c("0", "1"))
  )
  all_combinations_naive <- all_combinations_naive[!(I1 == "0" & I2 == "0" & I3 == "0")]
  
  # aggregate the observed data for the naive model
  df_agg_naive <- df_obs[, .(count = .N), by = .(kod_obs, I1 = as.factor(I1), I2 = as.factor(I2), I3 = as.factor(I3))]
  df_agg_naive <- df_agg_naive[all_combinations_naive, on = .(kod_obs, I1, I2, I3)]
  df_agg_naive[is.na(count), count := 0]
  setnames(df_agg_naive, "kod_obs", "kod")
  
  # prepare all combinations of kod values and observed capture histories
  # for the improved model
  all_combinations_em <- CJ(
    kod_obs = factor(levels(df[["kod"]]), levels = levels(df[["kod"]])), 
    I1 = factor(c("0", "1"), levels = c("0", "1")),
    I2 = factor(c("0", "1"), levels = c("0", "1")),
    I3 = factor(c("0", "1"), levels = c("0", "1"))
  )
  all_combinations_em <- all_combinations_em[!(I1 == "0" & I2 == "0" & I3 == "0")]
  
  # aggregate the observed data for the improved model
  df_agg_em <- df_obs[, .(count = .N), by = .(kod_obs, I1 = as.factor(I1), I2 = as.factor(I2), I3 = as.factor(I3))]
  df_agg_em <- df_agg_em[all_combinations_em, on = .(kod_obs, I1, I2, I3)]
  df_agg_em[is.na(count), count := 0]
  
  # prepare a data frame for the unobserved data
  df_hidden <- CJ(
    kod = factor(levels(df[["kod"]]), levels = levels(df[["kod"]])),
    I1 = factor("0", levels = c("0", "1")),
    I2 = factor("0", levels = c("0", "1")),
    I3 = factor("0", levels = c("0", "1"))
  )
  
}
  
  