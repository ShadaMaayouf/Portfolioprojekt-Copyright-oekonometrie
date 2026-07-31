library(tidyverse)
library(fixest)
library(lmtest)
library(sandwich)
library(modelsummary) # Für eine übersichtliche Tabellendarstellung
library(dplyr)
library(haven)

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

# dummy variable L&V = Lombardy & Venetia (Treatmentgruppe)
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
# Notwendige Bibliotheken laden


# Datenvorbereitung
# Es wird davon ausgegangen, dass folgende Variablen existieren:
# operas: Anzahl der neuen Opern
# state: Identifikator für den Staat
# year: Das jeweilige Jahr
# LV: Dummy-Variable (1 für Lombardy & Venetia, 0 sonst)
# Post: Dummy-Variable (1 für Jahre nach 1800, 0 sonst)

df <- df %>%
  mutate(
    LV_Post = LV * Post,
    year_num = as.numeric(year) # Als numerische Variable für die linearen Pre-Trends
  )

# OLS Ohne Trends, aber mit beiden  State und Year Fixed Effects (Hauptergebnis)
m1 <- feols(operas ~ LV_Post | state + year, 
              data = df, 
              cluster = ~state^year)

# Spezifikation 2: OLS ohne State Fixed Effects (daher muss der LV-Dummy ins Modell)
m2 <- feols(operas ~ LV_Post + LV | year, 
              data = df, 
              cluster = ~state^year)

# Spezifikation 3: OLS mit State und Year FE + linearer Pre-Trend für L&V
m3 <- feols(operas ~ LV_Post + LV:year_num | state + year, 
              data = df, 
              cluster = ~state^year)

# Spezifikation 4: OLS mit State und Year FE + State-spezifische lineare Pre-Trends
m4 <- feols(operas ~ LV_Post | state + year + state[year_num], 
              data = df, 
              cluster = ~state^year)

# Ausgabe der Modelle im Stil einer wissenschaftlichen Tabelle
modelsummary(list("Spalte 1" = m1, "Spalte 2" = m2, "Spalte 3" = m3, "Spalte 4" = m4), 
             stars = TRUE, 
             gof_omit = "IC|Log|RMSE")


#Ausgabe in der Console
etable(
  m1, m2, m3, m4,
  se = "cluster",
  cluster = "state_factor",
  digits = 3,
  headers = c("(1)", "(2)", "(3)", "(4)")
)
