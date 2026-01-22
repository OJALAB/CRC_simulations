library(data.table)
library(extraDistr)
library(lubridate)
library(RcppSimdJson)
library(stringr)
library(brglm2)

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
  observed_means <- df[I1 == 1 | I2 == 1, .(mean_obs = mean(Z, na.rm = TRUE), mean_obs_all = mean(Z_true)), by = .(X, Y)]
  df <- merge(df, observed_means, by = c("X", "Y"), all.x = TRUE, sort = FALSE)
  df[is.na(Z) & (I1 == 1 | I2 == 1), Z := mean_obs]
  df[, mean_obs := NULL]
  df[, mean_obs_all := NULL]
  
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
  df_hidden[, Z_pred_from_count_all := n_pred * mean_obs_all]
  
  est_total_Z_imp <- sum(df_observed[["Z"]]) + sum(df_hidden[["Z_pred_imp"]])
  est_total_Z_true <- sum(df_observed[["Z_true"]]) + sum(df_hidden[["Z_pred_true"]])
  est_total_Z_from_count <- sum(df_observed[["Z"]]) + sum(df_hidden[["Z_pred_from_count"]])
  est_total_Z_from_count_all <- sum(df_observed[["Z_true"]]) + sum(df_hidden[["Z_pred_from_count_all"]])
  
  data.table(true_total_Z = true_total_Z,
             est_total_Z_true = est_total_Z_true,
             est_total_Z_imp = est_total_Z_imp,
             est_total_Z_from_count = est_total_Z_from_count,
             est_total_Z_from_count_all = est_total_Z_from_count_all)
  
}

calculate_metrics <- function(df_results) {
  
  metrics_list <- list()
  
  for (est in c("est_total_Z_true", "est_total_Z_imp", "est_total_Z_from_count", "est_total_Z_from_count_all")) {
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

read_cbop <- function(file, qdata, verbose = TRUE) {
  
  ## helper functions
  regon_check <- function(x, last, digits = 9) {
    
    if (digits == 9) {
      regon_weights <- c(8,9,2,3,4,5,6,7) ## mod 11
      splitted <- as.numeric(str_split(x, "", simplify = T)[1:8])
      mod <- sum(splitted*regon_weights) %% 11
      if (mod == 10) mod <- 0
      test <- mod %% 11 == last
    } else {
      regon_weights <- c(2,4,8,5,0,9,7,3,6,1,2,4,8) ## mod 11
      splitted <- as.numeric(str_split(x, "", simplify = T)[1:13])
      mod <- sum(splitted*regon_weights) %% 11
      if (mod == 10) mod <- 0
      test <- mod %% 11 == last
    }
    return(test)
  }
  
  regon_check_vec <- Vectorize(regon_check, vectorize.args = c("x", "last"))
  
  nip_check <- function(x, last) {
    nip_weights <- c(6, 5, 7, 2, 3, 4, 5, 6, 7) ## mod 11
    splitted <- as.numeric(str_split(x, "", simplify = T)[1:9])
    mod <- sum(splitted*nip_weights) %% 11
    test <- mod %% 11 == last
    return(test)
  }
  
  nip_check_vec <- Vectorize(nip_check, vectorize.args = c("x", "last"))
  
  
  cbop_file <- readLines(file)
  cbop_file <- RcppSimdJson::fparse(json = cbop_file[1])
  
  prac_list <- list()
  for (i in 1:length(cbop_file)) {
    ## employeer information
    prac <- cbop_file[[i]]$danePracodawcy
    names(prac) <- cbop_file[[i]]$hash
    prac_df <-  rbindlist(prac, idcol = "hash")
    setnames(prac_df, names(prac_df)[-1], paste0("prac_", names(prac_df)[-1]))
    
    ## other information
    pozostaleDane <- rbindlist(cbop_file[[i]]$pozostaleDane)
    setnames(pozostaleDane, names(pozostaleDane), paste0("poz_", names(pozostaleDane)))
    
    ## Working and pay conditions
    warunkiPracyIPlacy <- lapply(cbop_file[[i]]$warunkiPracyIPlacy, as.data.table)
    warunkiPracyIPlacy <- lapply(warunkiPracyIPlacy, \(x) x[1,])
    warunkiPracyIPlacy <- rbindlist(warunkiPracyIPlacy, fill = T)
    setnames(warunkiPracyIPlacy, names(warunkiPracyIPlacy), paste0("war_", names(warunkiPracyIPlacy)))
    
    ### requirements
    wymagania <- lapply(cbop_file[[i]]$wymagania, as.data.table)
    wymagania <- lapply(wymagania, \(x) x[1,])
    wymagania <- rbindlist(wymagania, fill = T)
    wym_cols <- names(wymagania)[grepl('czyPodanoWymagania|inneWymagania|stazWymagOgol', names(wymagania))]
    wymagania <- wymagania[, ..wym_cols]
    
    ## requred 
    wymagania_kon <- lapply(cbop_file[[i]]$wymagania, "[[", "wymaganiaKonieczne")
    wymagania_kon <- lapply(wymagania_kon, \(x) as.data.table(x[c("jezyki", "uprawnienia", "wyksztalcenia", "staze")]))
    wymagania_kon <- lapply(wymagania_kon, \(x) x[1,])
    null_dfs <- which(lengths(wymagania_kon) == 0)
    for (l in null_dfs) wymagania_kon[[l]] <-  data.table(jezyki = NA)
    wymagania_kon <- rbindlist(wymagania_kon, fill = T)
    
    setnames(wymagania_kon, names(wymagania_kon), paste0("war_", names(wymagania_kon)))
    setnames(wymagania, names(wymagania), paste0("war_", names(wymagania)))
    
    ### 
    
    prac_df <- cbind(prac_df, pozostaleDane, warunkiPracyIPlacy, wymagania, wymagania_kon)
    
    prac_df[, ":="(typOferty = cbop_file[[i]]$typOferty,
                   typOfertyNaglowek = cbop_file[[i]]$typOfertyNaglowek,
                   zagranicznaEures = cbop_file[[i]]$zagranicznaEures,
                   kodJezyka =  cbop_file[[i]]$kodJezyka,
                   czyWazna = cbop_file[[i]]$czyWazna,
                   statusOferty = cbop_file[[i]]$statusOferty,
                   zaintUA = if (is.null(cbop_file[[i]]$pracodZainteresZatrUA)) NA else cbop_file[[i]]$pracodZainteresZatrUA,
                   tlumUA = if (is.null(cbop_file[[i]]$zgodNaTlumaczenieUA)) NA else cbop_file[[i]]$zgodNaTlumaczenieUA)]
    prac_list[[i]] <- prac_df
  }
  
  prac_list_df <- rbindlist(prac_list, fill = T)
  
  ### internal data cleaning
  if (verbose) {
    cat("Number of rows:", nrow(prac_list_df), '\n')
  }
  
  ## this may be changed to base::as.Date
  prac_list_df[, ":="(poz_dataPrzyjZglosz=dmy(poz_dataPrzyjZglosz),
                      poz_ofertaWaznaDo=dmy(poz_ofertaWaznaDo))]
  
  final_df <- prac_list_df[, ":="(prac_nip = str_remove_all(prac_nip, "-"),
                                  poz_dni = qdata-poz_dataPrzyjZglosz,
                                  kod_pocztowy = str_extract(war_miejscePracy, "\\d{2}\\-\\d{3}"))]
  
  final_df[prac_pracodawca == "kontakt przez PUP", prac_pracodawca:=NA]
  final_df[prac_pracodawca == "kontakt przez OHP", prac_pracodawca:=NA]
  
  final_df[, ":="(war_gmina = tolower(war_gmina), war_ulica=tolower(war_ulica), war_miejscowosc=tolower(war_miejscowosc))]
  final_df[, ":="(war_gmina = str_remove(war_gmina, "m.st. "))]
  final_df[, prac_nip := str_remove_all(prac_nip, "-")]
  final_df[, war_ulica:=str_replace(war_ulica,  "pl\\.", "plac ")]
  final_df[, war_ulica:=str_replace(war_ulica,  "al\\.", "aleja ")]
  final_df[, war_ulica:=str_replace(war_ulica,  "  ", " ")]
  final_df[, war_ulica:=str_remove(war_ulica,  "^\\.|-$")]
  final_df[, kod_pocztowy:=str_remove(kod_pocztowy, "00-000")]
  final_df[kod_pocztowy == "", kod_pocztowy := NA]
  final_df[, poz_kodZawodu := str_remove(poz_kodZawodu, "RPd057\\|")]
  final_df[, qdata:=qdata]
  
  final_df[, row_id := 1:.N] ## row identifiers
  final_df[, prac_regon:=str_extract(prac_regon, "\\d{1,}")] ## remove non-numbers
  final_df[, prac_nip:=str_extract(prac_nip, "\\d{1,}")] ## ## remove non-numbers
  final_df[str_detect(prac_nip, "^([0-9])\\1*$"), prac_nip := NA] ## if all numbers are the same -- NA
  final_df[str_detect(prac_regon, "^([0-9])\\1*$"), prac_regon := NA] ## if all numbers are the same -- NA
  final_df[nchar(prac_regon) == 14 & str_detect(prac_regon, "00000$"), prac_regon:=substr(prac_regon,1,9)] ## correct number of digits
  final_df[nchar(prac_regon) == 10 & prac_regon == prac_nip, prac_regon := NA] ## regon the same as np 
  final_df[nchar(prac_regon) %in% c(3,10), prac_regon := NA] ## incorrect numbers
  final_df[nchar(prac_regon) == 8, prac_regon := str_pad(prac_regon, 9, "left", "0")] ## add leading zeros
  final_df <- final_df[!is.na(prac_regon) | !is.na(prac_nip)] ## remove missing in both identifiers
  final_df[!is.na(prac_nip), regon9_count:=uniqueN(substr(prac_regon[!is.na(prac_regon)],1,9)), prac_nip] ## count unique regons in ni
  final_df[!is.na(prac_regon), regon9_count:=uniqueN(prac_nip), prac_regon] ## count unique nip byu regons
  final_df[nchar(prac_regon) == 9, regon9_last_dig:=substr(prac_regon, 9,9)]
  final_df[nchar(prac_regon) == 14, regon14_last_dig:=substr(prac_regon, 14,14)]
  final_df[nchar(prac_nip) == 10, nip_last_dig:=substr(prac_nip, 10,10)]
  final_df[nchar(prac_regon) == 9, regon9_check:=regon_check_vec(prac_regon, as.numeric(regon9_last_dig))] # 646 in regon 9 digits
  final_df[nchar(prac_regon) == 14, regon14_check:=regon_check_vec(prac_regon, as.numeric(regon14_last_dig), 14)] # 30 in regon 14 digits
  final_df[nchar(prac_nip) == 10, nip_check:=nip_check_vec(prac_nip, as.numeric(nip_last_dig))] # NIPs are correct!
  final_df[regon9_check == FALSE, prac_regon := NA]
  final_df[regon14_check == FALSE, prac_regon := NA]
  
  ## Polish JVS definition
  final_df[typOferty == "OFERTA_PRACY" & czyWazna == TRUE & 
             as.Date(poz_dataPrzyjZglosz) <= qdata & as.Date(poz_ofertaWaznaDo) >= qdata & 
             war_kraj == "Polska" &
             !war_rodzajZatrudnienia %in% c("Praktyka absolwencka", "Nie dotyczy", "Umowa zlecenie / Umowa o świadczenie usług", "Umowa o dzieło",
                                            "Umowa agencyjna") & 
             zagranicznaEures == FALSE & 
             str_detect(war_stanowisko, regex("staż|praktyk", T), negate = T) & 
             (str_extract(war_wynagrodzenieBrutto, "[A-Z]{3}") %in% c("PLN", NA)), jvs_vac_def := TRUE]
  
  return(final_df[!is.na(prac_regon) | !is.na(prac_nip)])
}

sim_epraca <- function(df, p_1_s, p_2_s, missing_frac) {
  
  params_probs <- data.table(
    kod = as.factor(0:9),
    prob_1 = p_1_s,
    prob_2 = p_2_s
  )
  
  df <- merge(df, params_probs, by = "kod", sort = FALSE)
  df[, I1 := rbinom(n = .N, size = 1, prob = prob_1)]
  df[, I2 := rbinom(n = .N, size = 1, prob = prob_2)]
  df[, prob_1 := NULL]
  df[, prob_2 := NULL]
  
  df[, wakaty := as.numeric(wakaty)]
  df[, wakaty_true := wakaty]
  to_remove <- sample(1:NROW(df), size = floor(missing_frac * NROW(df)))
  df[to_remove, wakaty := NA]
  observed_means <- df[I1 == 1 | I2 == 1, .(mean_obs = mean(wakaty, na.rm = TRUE), mean_obs_all = mean(wakaty_true)), by = .(kod)]
  observed_means[is.nan(mean_obs), mean_obs := mean(df[I1 == 1 | I2 == 1, wakaty], na.rm = TRUE)]
  observed_means[is.nan(mean_obs_all), mean_obs_all := mean(df[I1 == 1 | I2 == 1, wakaty_true], na.rm = TRUE)]
  df <- merge(df, observed_means, by = "kod", all.x = TRUE, sort = FALSE)
  df[is.na(wakaty) & (I1 == 1 | I2 == 1), wakaty := mean_obs]
  df[, mean_obs := NULL]
  df[, mean_obs_all := NULL]
  
  true_total_wakaty <- sum(df[["wakaty_true"]])
  
  all_combinations <- CJ(
    kod = factor(0:9, levels = levels(df$kod)), 
    I1 = factor(c("0", "1"), levels = c("0", "1")),
    I2 = factor(c("0", "1"), levels = c("0", "1"))
  )
  all_combinations <- all_combinations[!(I1 == "0" & I2 == "0")]
  
  df_observed <- df[I1 == 1 | I2 == 1]
  df_agg <- df_observed[, .(count = .N), by = .(kod, I1 = as.factor(I1), I2 = as.factor(I2))]
  df_hidden <- data.table(expand.grid(
    kod = as.factor(0:9),
    I1 = factor("0", levels = c("0", "1")),
    I2 = factor("0", levels = c("0", "1"))
  ))
  df_agg <- df_agg[all_combinations, on = .(kod, I1, I2)]
  df_agg[is.na(count), count := 0]
  df_agg[, kod := relevel(kod, ref = "2")]
  
  model_count <- glm(count ~ (I1 + I2) * kod,
                     family = quasipoisson(),
                     data = df_agg)
  model_count_brglm <- glm(count ~ (I1 + I2) * kod,
                           family = poisson(),
                           data = df_agg,
                           method = "brglmFit")
  
  df_hidden[, n_pred := predict(model_count, newdata = df_hidden, type = "response")]
  df_hidden[, n_pred_brglm := predict(model_count_brglm, newdata = df_hidden, type = "response")]
  
  df_hidden <- merge(df_hidden, observed_means, by = "kod", all.x = TRUE, sort = FALSE)
  df_hidden[is.na(mean_obs), mean_obs := mean(df[I1 == 1 | I2 == 1, wakaty], na.rm = TRUE)]
  df_hidden[is.na(mean_obs_all), mean_obs_all := mean(df[I1 == 1 | I2 == 1, wakaty_true], na.rm = TRUE)]
  df_hidden[, wakaty_pred_from_count := n_pred * mean_obs]
  df_hidden[, wakaty_pred_from_count_all := n_pred * mean_obs_all]
  df_hidden[, wakaty_pred_from_count_brglm := n_pred_brglm * mean_obs]
  df_hidden[, wakaty_pred_from_count_all_brglm := n_pred_brglm * mean_obs_all]
  
  est_total_wakaty <- sum(df_observed[["wakaty"]]) + sum(df_hidden[["wakaty_pred_from_count"]])
  est_total_wakaty_all <- sum(df_observed[["wakaty_true"]]) + sum(df_hidden[["wakaty_pred_from_count_all"]])
  est_total_wakaty_brglm <- sum(df_observed[["wakaty"]]) + sum(df_hidden[["wakaty_pred_from_count_brglm"]])
  est_total_wakaty_all_brglm <- sum(df_observed[["wakaty_true"]]) + sum(df_hidden[["wakaty_pred_from_count_all_brglm"]])
  
  data.table(true_total_wakaty = true_total_wakaty,
             est_total_wakaty = est_total_wakaty,
             est_total_wakaty_all = est_total_wakaty_all,
             est_total_wakaty_brglm = est_total_wakaty_brglm,
             est_total_wakaty_all_brglm = est_total_wakaty_all_brglm)
  
}

calculate_metrics_epraca <- function(df_results) {
  
  metrics_list <- list()
  
  for (est in c("est_total_wakaty", "est_total_wakaty_all", "est_total_wakaty_brglm", "est_total_wakaty_all_brglm")) {
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
  
  # metrics_list[["true_total_Z"]] <- data.table(
  #   method = "true_total_z",
  #   mean_value = mean(df_results[["true_total_Z"]]),
  #   mean_bias = 0,
  #   mean_rel_bias = 0,
  #   rmse = 0
  # )
  
  rbindlist(metrics_list)
  
}
