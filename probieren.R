library(dplyr)
library(haven)
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)

# Daten laden
df <- read_stata("./data/operas_1781_1820.dta")

#copyright_post1801 = Treatmentdummy (1 für Lombardy/Venetia nach 1801)
#state1 = numerische ID für Regionen (für Fixed Effects)
#year = Jahres-FE

# Faktoren erzeugen
df <- df %>%
  mutate(
    state_factor = factor(state1),
    year_factor  = factor(year)
  )

# Paneldatenstruktur definieren
pdata <- pdata.frame(df, index = c("state1", "year_factor"))


# Modell 1: Pooled OLS (keine fixed effects)
#Schätzt nur den Mittelwertsunterschied zwischen Copyright‑Regionen und Nicht‑Copyright‑Regionen
m1 <- plm(operas ~ copyright_post1801, data = pdata, model = "pooling")
coeftest(m1, vcov = vcovHC(m1, type = "HC1", cluster = "group"))
coeftest(
  m1, 
  vcov = vcovHC(m1, type = "HC1")
)

#Modell (2): Nur Regionen fixed Effekte (Individual Fixed Effects)
m2 <- plm(
  operas ~ copyright_post1801,
  data = pdata,
  model = "within",
  effect = "individual" # bedeutet FE über die erste Panel‑Dimension: state
)

coeftest(
  m2, 
  vcov = vcovHC(m2, type = "HC1", cluster = "group")
)

# Modell (3): Year Fixed Effects (nur Jahres‑FE)
pdata_year <- pdata.frame(df, index = c("year_factor", "state_factor")) #Panel‑Dimension tauschen damit year als individual verwendet wird

m3 <- plm(
  operas ~ copyright_post1801,
  data = pdata_year,
  model = "within",
  effect = "individual"
)

coeftest(m3, vcov = vcovHC(m3, type = "HC1", cluster = "group"))

#Modell (4): State FE + Year FE (Two‑Way FE)
m4 <- plm(
  operas ~ copyright_post1801,
  data = pdata,
  model = "within",
  effect = "twoways"
)

coeftest(m4, vcov = vcovHC(m4, type = "HC1", cluster = "group"))


summary(m1)
summary(m2)
summary(m3)
summary(m4)


#############################################
library(haven)     # Zum Einlesen der Stata-Daten
library(dplyr)     # Für Datenmanipulation
library(fixest)    # Für effiziente Panel-Regressionen mit festen Effekten (FE)

# 1. Daten einlesen
df <- read_stata("./data/operas_1781_1820.dta")

# 2. Modelle der Tabelle 3 schätzen (Spalten 1 bis 4)

# Spalte 1: Einfaches DiD-Modell ohne feste Effekte (nur Treatment-Indikator)
model_1 <- feols(operas ~ copyright_post1801, data = df)

# Spalte 2: Mit Staatsspezifischen festen Effekten (State FE)
model_2 <- feols(operas ~ copyright_post1801 | state, data = df)

# Spalte 3: Mit Staatsspezifischen UND Jahresspezifischen festen Effekten (State FE + Year FE)
model_3 <- feols(operas ~ copyright_post1801 | state + year, data = df)

# Spalte 4: Vollständiges Modell mit zusätzlichen zeitlich variierenden Kontrollen / Trends 
# (Je nach genauer Spezifikation im Paper, z.B. regionesspezifische Trends oder erweiterte Kontrollen)
model_4 <- feols(operas ~ copyright_post1801 + post1801 | state + year, data = df)

# 3. Gemeinsame Darstellung der Ergebnisse als Regressionstabelle
etable(model_1, model_2, model_3, model_4, 
       headers = c("Col 1", "Col 2", "Col 3", "Col 4"),
       fitstat = c("n", "r2"))