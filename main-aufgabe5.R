library(haven)    
library(dplyr)     
library(ggplot2) 

df <- read_stata("./data/synthetic_control.dta")

# Daten für Lombardy und Synthetic Lombardy aufbereiten und zusammenführen für den Plot
df_lombardy  <- df %>% select(year, operas = true_lombardy) %>% mutate(type = "Lombardy")
df_synthetic <- df %>% select(year, operas = synthetic_lombardy) %>% mutate(type = "Synthetic Lombardy")

df_plot <- bind_rows(df_lombardy, df_synthetic)

# Visualisierung
ggplot(df_plot, aes(year, operas, linetype = type, colour = type)) +
  geom_line(linewidth = 0.8) +
  #Farbe
  scale_color_manual(values = c("Lombardy" = "blue", "Synthetic Lombardy" = "red")) +
  scale_linetype_manual(values = c("Lombardy" = "solid", "Synthetic Lombardy" = "dashed")) +
  
  # Vertikale Linie
  geom_vline(xintercept = 1801, colour = "darkgray", linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 1802.5, y = 9.5, label = "1801 Copyright Law", hjust = 0, size = 3.5, fontface = "italic") +
  labs(
    title = "Replication of Figure 4 in the paper Giorcelli & Moser (2020)",
    x = "Year",
    y = "New Operas per Year",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(xlim = c(1780, 1820), ylim = c(0, 10))

# Speichern der Grafik
ggsave("./grafiken/figure_4_replication.png", width = 8, height = 6, dpi = 300)