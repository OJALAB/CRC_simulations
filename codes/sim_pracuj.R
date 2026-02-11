library(readxl)

source("codes/functions_pracuj.R")

data <- read_excel("data-raw/liczba miejsc pracy.xlsx", sheet = 1)
setDT(data)

data[is.na(data)] <- 0
data[, count := `2021` + `2022` + `2023` + `2024` + `2025`]

cens_frac <- sum(data[vacancies == 2 & status == 0, count]) / sum(data[vacancies == "2", count])

data <- data[, .(total = sum(count)), by = .(vacancies)]

data[vacancies == 2, total] / sum(data[["total"]])
