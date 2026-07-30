library(haven)     # Zum Einlesen von Stata-Dateien (.dta)
library(dplyr)     # Für die Datenmanipulation
library(ggplot2)   # Für die Visualisierung

df <- read_stata("./data/operas_1781_1820.dta")

head(df)

#Gruppierung nur by state
df_state <- df %>%
  group_by(state) %>%
  summarise(operas = sum(operas), .groups = "drop")
head(df_state)

##Visualisierung
ggplot(
  df_state, 
  aes(reorder(state, operas), operas, fill = operas)
  ) +
  labs(
    title = "Gesamte Anzahl neuer Opern nach Staat/Region (1781–1820)",
    x = "Region",
    y = "Anzahl neuer Opern"
  ) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  # Macht das Diagramm horizontal für bessere Lesbarkeit der Regionen
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("./grafiken/aufgabe_2/operas_by_region.png", width = 12, height = 6, dpi = 300)

#Lösung von Aufgabe 2: Aggregation
#Opern nach Jahr und Region gruppieren
df_year_state <- df %>%
  group_by(year, state) %>%
  summarise(
    operas = sum(operas), 
    .groups = "drop"
    )

df_year_state

#Visualisierung
ggplot(
  df_year_state, 
  aes(year, operas, colour = state)
  ) +
  labs(
    x = "Jahr",
    y = "Anzahl neuer Opern",
    title = "Neue Opern pro Jahr nach Region (1781–1820)",
    subtitle = "Gestrichelte Linie zeigt die Einführung des Copyrights (1801)",
    colour = "Region"
  ) + # Einführungsjahr des Urheberrechts markieren
  geom_vline(xintercept = 1801, linetype = 2) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


# Optional: Plot als Bilddatei speichern
ggsave("./grafiken/aufgabe_2/operas_by_year_region.png", width = 12, height = 6, dpi = 300)