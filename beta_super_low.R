# ==============================================================================
# BETA REGRESSION: LOW INVOLVEMENT (Multi-KPI Analyse)
# ==============================================================================
# ZIEL:
# Dieses Skript analysiert den Einfluss von 'Buzz' auf VIER verschiedene 
# Zielgrößen (KPIs): Awareness, Popularity, Usage und Loyalty.
# Es filtert automatisch auf Low Involvement Produkte.
#
# STRUKTUR:
# 1. Setup & Daten laden
# 2. Definition der Analyse-Funktion (um Wiederholungen zu vermeiden)
# 3. Durchführung der Analyse für alle 4 KPIs
# 4. Speichern der Ergebnisse
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & PAKETE
# ------------------------------------------------------------------------------
if(!require(betareg)) install.packages("betareg") # Für die statistische Berechnung
if(!require(dplyr)) install.packages("dplyr")     # Für Datenfilterung
if(!require(broom)) install.packages("broom")     # Für schöne Ergebnistabellen
if(!require(tidyr)) install.packages("tidyr")     # Für Datenstrukturierung

library(betareg)
library(dplyr)
library(broom)
library(tidyr)

# Prüfen, ob der Hauptdatensatz existiert
if (!exists("KPIs_alle1864")) {
  stop("FEHLER: Bitte lade zuerst den Datensatz 'KPIs_alle1864'.")
}

# Daten kopieren, um das Original zu schützen
df_raw <- KPIs_alle1864

# ------------------------------------------------------------------------------
# 2. FILTERUNG: LOW INVOLVEMENT
# ------------------------------------------------------------------------------
# Wir filtern nur Produkte/Marken mit geringem Involvement (0)
df_low <- df_raw %>% 
  filter(Involvement == 0)

cat(paste("Analyse gestartet für LOW INVOLVEMENT.\nAnzahl Beobachtungen:", nrow(df_low), "\n"))

# Definition der Variablen (JETZT MIT AWARENESS)
target_kpis <- c("Awareness", "Popularity", "Usage", "Loyalty") 
predictor_var <- "Buzz"                            # Unser Treiber

# ------------------------------------------------------------------------------
# 3. DEFINITION DER ANALYSE-FUNKTION
# ------------------------------------------------------------------------------
# Anstatt den Code 4x zu kopieren, schreiben wir eine Funktion.
# Diese Funktion nimmt einen KPI (z.B. "Awareness") und führt die komplette
# Beta-Regression dafür durch.

analyze_kpi <- function(data, kpi_name, predictor) {
  
  cat(paste0("\n>>> Starte Analyse für Zielvariable: ", kpi_name, " <<<\n"))
  
  # A. Daten vorbereiten für diesen spezifischen KPI
  # ------------------------------------------------
  # Wir wählen nur die nötigen Spalten, um NAs (fehlende Werte) sauber zu entfernen.
  # WICHTIG: rename() nutzt 'all_of', um dynamisch den Spaltennamen zu wählen.
  work_data <- data %>%
    select(Group = Industry, Target = all_of(kpi_name), Predictor = all_of(predictor)) %>%
    filter(!is.na(Target) & !is.na(Predictor))
  
  # B. Transformation (Squeezing)
  # -----------------------------
  # Beta-Regression benötigt Werte STRIKT zwischen 0 und 1 (exklusive 0 und 1).
  # Falls wir 0.0 oder 1.0 haben, wenden wir die Smithson-Transformation an.
  y_raw <- work_data$Target
  n <- length(y_raw)
  
  if (any(y_raw <= 0) | any(y_raw >= 1)) {
    # Formel: (y * (n-1) + 0.5) / n
    work_data$Target_Transformed <- (y_raw * (n - 1) + 0.5) / n
    cat("   -> Werte wurden transformiert (skaliert in Intervall (0,1))\n")
  } else {
    work_data$Target_Transformed <- y_raw
  }
  
  # C. Berechnung pro Industrie (Loop)
  # ----------------------------------
  results_list <- list()
  industries <- unique(work_data$Group)
  
  for(ind in industries) {
    # Daten für EINE Industrie filtern
    sub_df <- work_data %>% filter(Group == ind)
    
    # Mindestens 5 Datenpunkte nötig für Statistik
    if(nrow(sub_df) < 5) {
      next # Überspringen, wenn zu wenig Daten
    }
    
    tryCatch({
      # Das Modell: Zielvariable ~ Treiber
      m <- betareg(Target_Transformed ~ Predictor, data = sub_df)
      
      # Ergebnisse extrahieren
      res <- tidy(m) %>%
        filter(term == "Predictor") %>% # Wir wollen nur den Effekt des Treibers
        mutate(
          KPI = kpi_name,         # Damit wir später wissen, wozu die Zahl gehört
          Industry = ind,
          R_Squared = summary(m)$pseudo.r.squared,
          Signifikanz = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01  ~ "**",
            p.value < 0.05  ~ "*",
            p.value < 0.1   ~ ".",
            TRUE ~ ""
          )
        ) %>%
        select(KPI, Industry, Estimate = estimate, Std_Error = std.error, 
               Test_Statistic = statistic, P_Value = p.value, Signifikanz, R_Squared)
      
      results_list[[length(results_list) + 1]] <- res
      
    }, error = function(e) {
      # Fehler ignorieren (z.B. wenn Modell nicht konvergiert)
    })
  }
  
  # Alle Ergebnisse für diesen KPI zusammenfügen
  return(bind_rows(results_list))
}

# ------------------------------------------------------------------------------
# 4. DURCHFÜHRUNG (LOOP ÜBER ALLE KPIS)
# ------------------------------------------------------------------------------

all_results_low <- list()

for (kpi in target_kpis) {
  # Prüfen, ob Spalte im Datensatz existiert
  if (kpi %in% names(df_low)) {
    all_results_low[[kpi]] <- analyze_kpi(df_low, kpi, predictor_var)
  } else {
    warning(paste("Spalte", kpi, "nicht im Datensatz gefunden!"))
  }
}

# Alles zu einer großen Tabelle zusammenfügen
final_table_low <- bind_rows(all_results_low) %>%
  arrange(KPI, desc(Estimate)) # Sortieren

# ------------------------------------------------------------------------------
# 5. ERGEBNIS-VORSCHAU & FORMATIERUNG (Deutsch)
# ------------------------------------------------------------------------------

# Deutsche Formatierung für bessere Lesbarkeit (Komma statt Punkt)
final_table_low_de <- final_table_low %>%
  mutate(
    Estimate = format(Estimate, decimal.mark = ",", digits = 5, nsmall = 5),
    Std_Error = format(Std_Error, decimal.mark = ",", digits = 5, nsmall = 5),
    Test_Statistic = format(Test_Statistic, decimal.mark = ",", digits = 5, nsmall = 5),
    R_Squared = format(R_Squared, decimal.mark = ",", digits = 5, nsmall = 5),
    P_Value = format(P_Value, scientific = TRUE, digits = 5, decimal.mark = ",")
  )

cat("\n==========================================================\n")
cat(" ERGEBNISSE LOW INVOLVEMENT (Auszug)\n")
cat("==========================================================\n")
print(head(final_table_low_de, 20))

# Optional: Speichern als CSV
write.csv2(final_table_low_de, "Results_Low_Involvement_MultiKPI_2.csv", row.names = FALSE)
