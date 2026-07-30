library(tidyverse)
library(fixest)
library(lmtest)
library(sandwich)

# Daten laden
df <- read_stata("./data/operas_1781_1820.dta")

# Faktoren
df <- df %>%
  mutate(
    state_factor = factor(state1),
    year_factor  = factor(year)
  )

# -----------------------------------------
# Hilfsvariablen wie in Tabelle 3
# -----------------------------------------

# L&V = Lombardy & Venetia (Treatmentgruppe)
df <- df %>% mutate(LV = ifelse(state1 %in% c(4,8), 1, 0)) 

# Post = nach 1801
df <- df %>% mutate(Post = ifelse(year >= 1801, 1, 0))

# Interaktion
df <- df %>% mutate(LV_Post = LV * Post)

# Pretrend für L&V (lineare Zeittrend NUR für L&V)
df <- df %>% mutate(LV_trend = LV * year)

# State‑spezifische Trends
df <- df %>% mutate(state_trend = year)

# -----------------------------------------
# TABELLE 3 – Modelle (1) bis (4)
# -----------------------------------------

# (1) OLS + State FE + Year FE
m1 <- feols(
  operas ~ LV_Post | state_factor + year_factor,
  data = df,
  cluster = "state_factor"
)

# (2) + Linear pretrend for L&V
m2 <- feols(
  operas ~ LV_Post + LV_trend | state_factor + year_factor,
  data = df,
  cluster = "state_factor"
)

# (3) + State‑specific linear pretrend
m3 <- feols(
  operas ~ LV_Post | state_factor + year_factor + state_trend,
  data = df,
  cluster = "state_factor"
)

# (4) Ohne Trends, aber mit beiden FE (Hauptergebnis)
m4 <- feols(
  operas ~ LV_Post | state_factor + year_factor,
  data = df,
  cluster = "state_factor"
)

# -----------------------------------------
# Ausgabe wie Tabelle 3
# -----------------------------------------
etable(
  m1, m2, m3, m4,
  se = "cluster",
  cluster = "state_factor",
  digits = 3,
  headers = c("(1)", "(2)", "(3)", "(4)")
)
