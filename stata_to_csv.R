library(haven)

path <- "./data"

# Liste aller .dta Dateien im Ordner
files <- list.files(path, pattern = "\\.dta$", full.names = TRUE)

# Schleife: jede Datei einlesen und als CSV speichern
for (f in files) {
  
  # Einlesen
  df <- read_stata(f)
  
  # Neuen Dateinamen erzeugen
  csv_name <- sub("\\.dta$", ".csv", f)
  
  # Speichern
  write.csv(df, csv_name, row.names = FALSE)
  
  cat("Gespeichert:", csv_name, "\n")
}
