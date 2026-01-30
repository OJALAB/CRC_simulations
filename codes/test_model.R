source("codes/reg_model.R")
source("codes/functions.R")
library(VGAM)
library(gamlss)
library(gamlss.cens)

load("data-raw/data.RData")

data <- data[str_sub(poz_kodZawodu, 1, 1) != "0"]
data <- data[, c("poz_kodZawodu", "poz_lWolnychMiejsc", "prac_kodWojewodztwa")]
data[, poz_kodZawodu := as.factor(str_sub(poz_kodZawodu, 1, 1))]
data[, prac_kodWojewodztwa := as.factor(prac_kodWojewodztwa)]
setnames(data, old = c("poz_kodZawodu", "poz_lWolnychMiejsc", "prac_kodWojewodztwa"), new = c("kod", "wakaty", "woj"))

to_be_censored <- sample(data[, .I[wakaty == 2]], size = 700)
data[, status := 1]
data[to_be_censored, status := 0]

set.seed(123)
data[, kod := relevel(kod, ref = "2")]
train_indices <- sample(NROW(data), size = 0.8 * NROW(data))

data_train <- data[train_indices]
data_test <- data[-train_indices]

# model_cens <- vglm(SurvS4(wakaty, status, type = "right") ~ kod + woj, 
#                    family = cens.poisson, 
#                    data = data_train)

gen.cens(NBI, type = "right")

model_wakaty_nb <- gamlss(Surv(wakaty, status) ~ kod + woj,
                          family = fam,
                          data = data_train,
                          trace = FALSE)
