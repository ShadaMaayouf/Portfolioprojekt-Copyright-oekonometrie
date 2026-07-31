library(haven)     # Zum Einlesen von Stata-Dateien (.dta)
library(dplyr)     # Für Datenmanipulation
library(ggplot2)   # Für die Visualisierung

# 1. Daten einlesen (Beispielhafter Pfad, je nach lokaler Ordnerstruktur)
df <- read_stata("./data/synthetic_control.dta")

# 2. Daten für Lombardei (Tatsächlicher Wert) und Synthetic Lombardy (Gegenfaktischer Wert) filtern/aufbereiten
# Hinweis: Im replizierten Datensatz ist die synthetische Kontrollgruppe oft als eigene Variable oder aggregierter Wert hinterlegt
df_lombardy <- df %>%
  select(year, operas = true_lombardy) %>%
  mutate(type = "Lombardy")

# (Beispielhafter Datenextrakt für das Synthetic Control / Matching-Ergebnis aus dem Datensatz)
# Falls eine spezifische Variable für die Kontrollgruppe vorliegt, wird diese hier zugewiesen:
df_synthetic <- df %>%
  select(year, operas = synthetic_lombardy) %>%
  mutate(type = "Synthetic Lombardy")

# Zusammenführen der beiden Zeitreihen für den Plott
df_plot <- bind_rows(df_lombardy, df_synthetic)

# 3. Visualisierung in Anlehnung an Abbildung 4
ggplot(df_plot, aes(x = year, y = operas, linetype = type, colour = type)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Lombardy" = "blue", "Synthetic Lombardy" = "red")) +
  scale_linetype_manual(values = c("Lombardy" = "solid", "Synthetic Lombardy" = "dashed")) +
  # Vertikale Linie für das Einführungsjahr 1801
  geom_vline(xintercept = 1801, linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 1805, y = 9.5, label = "1801 Copyright Law", hjust = 0, size = 3.5) +
  labs(
    x = NULL,
    y = "New Operas per Year",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(xlim = c(1780, 1820), ylim = c(0, 10))

# Speichern der Grafik
ggsave("./grafiken/figure_4_replication.png", width = 8, height = 6, dpi = 300)