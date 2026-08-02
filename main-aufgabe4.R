library(tidyverse)
library(haven)
library(fixest)
library(modelsummary)

# Datebnaufbereitung
df <- read_stata("./data/operas_1781_1820.dta") %>%
  mutate(
    LV       = +(state1 %in% c(4, 8)),
    Post     = +(year >= 1801),
    LV_Post  = LV * Post,
    year_num = as.numeric(year)
  )

# Modelle
models <- list(
  "(1)" = feols(operas ~ LV_Post | state + year, data = df, cluster = ~state^year),
  "(2)" = feols(operas ~ LV_Post + LV | year, data = df, cluster = ~state^year),
  "(3)" = feols(operas ~ LV_Post + LV:year_num | state + year, data = df, cluster = ~state^year),
  "(4)" = feols(operas ~ LV_Post | state + year + state[year_num], data = df, cluster = ~state^year)
)

# Ausgaben als Wissenschaftliche Tabelle
modelsummary(models, stars = TRUE, gof_omit = "IC|Log|RMSE")

# Konsolen-Ausgabe
etable(models, digits = 3)