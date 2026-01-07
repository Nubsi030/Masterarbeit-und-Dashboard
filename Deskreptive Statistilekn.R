# Lade notwendige Bibliothek (falls nicht installiert, vorher install.packages("dplyr") ausführen)
if(!require(dplyr)) install.packages("dplyr")
if(!require(corrplot)) install.packages("corrplot")
library(dplyr)
library(corrplot)

# Sicherstellen, dass das Dataset als Tibble oder Dataframe behandelt wird
df <- KPIs_alle1864

# Definition der relevanten Spalten für die Analyse
relevante_spalten <- c("Awareness", "Popularity", "Usage", "Loyalty", "Buzz")

# ---------------------------------------------------------
# TEIL 1: Analyse für Involvement = 1
# ---------------------------------------------------------
cat("\n========================================\n")
cat("ERGEBNISSE FÜR INVOLVEMENT = 1\n")
cat("========================================\n")

# Daten filtern
data_inv1 <- df %>% 
  filter(Involvement == 1) %>%
  select(all_of(relevante_spalten))

# Berechnung der Statistiken (Mittelwert, Max, Min, Range)
stats_inv1 <- data_inv1 %>%
  summarise(across(everything(), list(
    Mittelwert = ~mean(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Range = ~max(., na.rm = TRUE) - min(., na.rm = TRUE)
  )))

# Umformung für bessere Lesbarkeit (Zeilen = Variablen, Spalten = Metriken)
stats_inv1_clean <- t(stats_inv1)
print(stats_inv1_clean)

cat("\n--- Korrelationsmatrix (Involvement = 1) ---\n")
cor_matrix_inv1 <- cor(data_inv1, use = "complete.obs")
print(cor_matrix_inv1)


# ---------------------------------------------------------
# TEIL 2: Analyse für Involvement = 0
# ---------------------------------------------------------
cat("\n\n========================================\n")
cat("ERGEBNISSE FÜR INVOLVEMENT = 0\n")
cat("========================================\n")

# Daten filtern
data_inv0 <- df %>% 
  filter(Involvement == 0) %>%
  select(all_of(relevante_spalten))

# Berechnung der Statistiken
stats_inv0 <- data_inv0 %>%
  summarise(across(everything(), list(
    Mittelwert = ~mean(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Range = ~max(., na.rm = TRUE) - min(., na.rm = TRUE)
  )))

# Umformung für bessere Lesbarkeit
stats_inv0_clean <- t(stats_inv0)
print(stats_inv0_clean)

cat("\n--- Korrelationsmatrix (Involvement = 0) ---\n")
cor_matrix_inv0 <- cor(data_inv0, use = "complete.obs")
print(cor_matrix_inv0)

# ---------------------------------------------------------
# TEIL 3: Analyse für den GESAMTEN Datensatz
# ---------------------------------------------------------
cat("\n\n========================================\n")
cat("ERGEBNISSE FÜR DEN GESAMTEN DATENSATZ\n")
cat("========================================\n")

# Gesamte Daten auswählen (nur relevante Spalten)
data_total <- df %>% 
  select(all_of(relevante_spalten))

cat("\n--- Korrelationsmatrix (Gesamt) ---\n")
cor_matrix_total <- cor(data_total, use = "complete.obs")
print(cor_matrix_total)

# Visualisierung als Heatmap
cat("\n--- Erstelle Heatmap Plot... ---\n")
corrplot(cor_matrix_total, 
         method = "color",       # Farben statt Kreise
         type = "upper",         # Nur obere Hälfte anzeigen
         addCoef.col = "black",  # Korrelationswerte in schwarz anzeigen
         tl.col = "black",       # Textfarbe der Labels
         tl.srt = 45,            # Text um 45 Grad drehen
         diag = FALSE,           # Diagonale ausblenden (ist immer 1)
         title = "Heatmap: Korrelationen (Gesamt)", 
         mar = c(0,0,2,0))       # Ränder anpassen damit Titel sichtbar ist
