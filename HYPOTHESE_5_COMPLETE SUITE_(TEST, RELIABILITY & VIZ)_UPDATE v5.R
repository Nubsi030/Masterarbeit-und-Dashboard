# ==============================================================================
# HYPOTHESE 5: COMPLETE SUITE (TEST, RELIABILITY & VIZ) - UPDATE v5
# ==============================================================================
# 
# ZIEL:
# 1. Konstruktion der Variablen 'Brand_Performance' (Aggregat aus 4 KPIs).
# 2. Reliabilitätsprüfung: Darf man diese 4 KPIs überhaupt zusammenfassen?
#    (Cronbach's Alpha & Korrelationsmatrix).
# 3. Hypothesentest H5: Einfluss von Buzz auf Brand_Performance (Beta-Regression).
# 4. Visualisierung: Streudiagramme für Low, High und Gesamt.
#
# ÄNDERUNG v5: R² (Pseudo-R-Squared) zur Ergebnistabelle hinzugefügt.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & PAKETE LADEN
# ------------------------------------------------------------------------------
# Wir prüfen, ob alle Pakete da sind. Wenn nicht, werden sie installiert.
# 'psych' ist wichtig für Cronbachs Alpha (Reliabilität).
if(!require(betareg)) install.packages("betareg") # Für Beta-Regression
if(!require(dplyr)) install.packages("dplyr")     # Datenmanipulation
if(!require(broom)) install.packages("broom")     # Saubere Tabellen
if(!require(tidyr)) install.packages("tidyr")     # Tidy Data
if(!require(ggplot2)) install.packages("ggplot2") # Grafiken
if(!require(psych)) install.packages("psych")     # NEU: Für Reliabilität (Alpha)
if(!require(corrplot)) install.packages("corrplot") # NEU: Für Korrelations-Plot

library(betareg)
library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)
library(psych)
library(corrplot)

# Sicherheitscheck: Ist der Datensatz geladen?
if (!exists("KPIs_alle1864")) {
  stop("FEHLER: Bitte lade zuerst den Datensatz 'KPIs_alle1864'.")
}

df_raw <- KPIs_alle1864

# ------------------------------------------------------------------------------
# 2. DATENVORBEREITUNG & AGGREGATION
# ------------------------------------------------------------------------------
cat("\n--- SCHRITT 1: Datenvorbereitung ---\n")

# Wir filtern NAs heraus, bevor wir rechnen, damit die Statistik sauber bleibt.
# Wir brauchen Buzz und die 4 Performance-KPIs.
df_clean <- df_raw %>%
  filter(
    !is.na(Buzz), 
    !is.na(Awareness), 
    !is.na(Popularity), 
    !is.na(Usage), 
    !is.na(Loyalty)
  )

cat(paste("Anzahl sauberer Datensätze für Analyse:", nrow(df_clean), "\n"))

# Erstellen der aggregierten Variable (Mittelwert)
# Wir fassen Awareness, Popularity, Usage und Loyalty zu einem Wert zusammen.
df_analysis <- df_clean %>%
  rowwise() %>%
  mutate(
    Brand_Performance = mean(c(Awareness, Popularity, Usage, Loyalty))
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 3. RELIABILITÄTS-CHECK (CRONBACHS ALPHA)
# ------------------------------------------------------------------------------
# WISSENSCHAFTLICHER HINTERGRUND:
# Bevor wir H5 testen, müssen wir beweisen, dass "Brand Performance" ein valides
# Konstrukt ist. Messen Awareness, Usage, Loyalty etc. wirklich das Gleiche?
# -> Wir nutzen Cronbachs Alpha. Werte > 0.7 gelten als akzeptabel/gut.

cat("\n--- SCHRITT 2: Reliabilitäts-Check (Cronbachs Alpha) ---\n")

# HILFSFUNKTION: Berechnet Alpha für eine gegebene Datengruppe
check_reliability <- function(data_subset, label) {
  cat(paste0("\n>>> Prüfe Reliabilität für: ", label, " <<<\n"))
  
  # Nur die 4 KPIs auswählen
  kpi_sub <- data_subset %>% select(Awareness, Popularity, Usage, Loyalty)
  
  # Berechnung (mit Unterdrückung von Warnungen für saubereren Output)
  # check.keys=TRUE sorgt dafür, dass 'polare' Items gedreht würden (hier nicht nötig, aber sicher)
  alpha_res <- psych::alpha(kpi_sub, check.keys=TRUE, warnings=FALSE)
  
  # Runde auf 4 Nachkommastellen für die interne Logik
  raw_alpha <- round(alpha_res$total$raw_alpha, 4)
  
  # Ausgabe mit deutschem Komma und zwingend 4 Nachkommastellen
  cat("   Cronbach's Alpha:", format(raw_alpha, decimal.mark = ",", nsmall = 4), "\n")
  
  if(raw_alpha > 0.8) {
    cat("   BEWERTUNG: Sehr Gut (> 0,8). Hohe Konsistenz.\n")
  } else if(raw_alpha > 0.7) {
    cat("   BEWERTUNG: Akzeptabel (> 0,7). Aggregation erlaubt.\n")
  } else {
    cat("   BEWERTUNG: Kritisch (< 0,7). Die KPIs driften hier auseinander.\n")
  }
  return(raw_alpha)
}

# 1. TEST: GESAMTMARKT
alpha_total <- check_reliability(df_analysis, "GESAMTMARKT")

# 2. TEST: LOW INVOLVEMENT
alpha_low <- check_reliability(df_analysis %>% filter(Involvement == 0), "LOW INVOLVEMENT")

# 3. TEST: HIGH INVOLVEMENT
alpha_high <- check_reliability(df_analysis %>% filter(Involvement == 1), "HIGH INVOLVEMENT")


# ZUSATZ: Korrelations-Plot (Visualisierung der Zusammenhänge für Gesamt)
# Speichert eine Grafik 'Correlation_Matrix.png' im Arbeitsverzeichnis
png("Correlation_Matrix.png", width=800, height=800)
M <- cor(df_analysis %>% select(Awareness, Popularity, Usage, Loyalty))
corrplot(M, method="color", type="upper", addCoef.col = "black", 
         tl.col="black", tl.srt=45, title="Korrelation der 4 KPIs (Gesamt)", mar=c(0,0,1,0))
dev.off()
cat("\nInfo: 'Correlation_Matrix.png' wurde erstellt.\n")


# ------------------------------------------------------------------------------
# 4. HYPOTHESEN-TEST (H5) MIT BETA-REGRESSION
# ------------------------------------------------------------------------------
cat("\n--- SCHRITT 3: Hypothesentest H5 (Beta-Regression) ---\n")

# Hilfsfunktion für den Test (wie zuvor, aber robuster)
run_h5_test <- function(data_subset, label) {
  
  # Transformation (Smithson), da Beta-Regression keine exakten 0 oder 1 mag.
  # Wenn Werte 0 oder 1 sind, werden sie minimal verschoben (Squeezing).
  y <- data_subset$Brand_Performance
  n <- length(y)
  if (any(y <= 0) | any(y >= 1)) {
    data_subset$Brand_Performance_Transformed <- (y * (n - 1) + 0.5) / n
  } else {
    data_subset$Brand_Performance_Transformed <- y
  }
  
  tryCatch({
    # Das Modell: Performance ~ Buzz
    model <- betareg(Brand_Performance_Transformed ~ Buzz, data = data_subset)
    res <- tidy(model) %>% filter(term == "Buzz")
    
    # Extraktion & Entscheidung
    est <- res$estimate
    p <- res$p.value
    # HIER NEU: R-Squared extrahieren
    r_sq <- summary(model)$pseudo.r.squared
    
    # Hypothese H1 (Positiver Effekt): Estimate muss > 0 sein UND p < 0.05
    decision <- ifelse(est > 0 & p < 0.05, "ANGENOMMEN", "ABGELEHNT")
    
    # Return als saubere Zeile
    return(tibble(
      Cluster = label,
      N = n,
      Estimate = est,
      Std_Error = res$std.error,
      Z_Value = res$statistic,
      P_Value = p,
      R_Squared = r_sq,
      Entscheidung = decision
    ))
    
  }, error = function(e) {
    return(tibble(Cluster = label, Entscheidung = "Error"))
  })
}

# Durchführen für Low, High und Gesamt
res_low <- run_h5_test(df_analysis %>% filter(Involvement == 0), "Low Involvement")
res_high <- run_h5_test(df_analysis %>% filter(Involvement == 1), "High Involvement")
res_total <- run_h5_test(df_analysis, "Gesamtmarkt")

# Tabelle zusammenfügen
final_table <- bind_rows(res_low, res_high, res_total)

# Formatierung (Deutsch) auf 4 Nachkommastellen
final_formatted <- final_table %>%
  mutate(
    Estimate = format(Estimate, decimal.mark = ",", digits = 4, nsmall = 4),
    Std_Error = format(Std_Error, decimal.mark = ",", digits = 4, nsmall = 4),
    Z_Value = format(Z_Value, decimal.mark = ",", digits = 4, nsmall = 4),
    P_Value = format(P_Value, decimal.mark = ",", scientific = TRUE, digits = 4),
    R_Squared = format(R_Squared, decimal.mark = ",", digits = 4, nsmall = 4)
  )

print(final_formatted)


# ------------------------------------------------------------------------------
# 5. VISUALISIERUNG (SCATTERPLOTS MIT REGRESSIONS-KURVE)
# ------------------------------------------------------------------------------
cat("\n--- SCHRITT 4: Visualisierung der Ergebnisse ---\n")

# Wir speichern die Grafiken in eine PDF Datei
pdf("H5_Visualisierung_Buzz_vs_Performance.pdf", width = 10, height = 6)

# PLOT A: Gesamtübersicht (Low vs High eingefärbt)
# Zeigt alle Datenpunkte. Die Linien zeigen den Trend für beide Gruppen.
p1 <- ggplot(df_analysis, aes(x = Buzz, y = Brand_Performance)) +
  # Punkte (leicht transparent, damit man Dichte erkennt). Größe = size.
  geom_point(aes(color = factor(Involvement)), alpha = 0.4, size = 2) +
  
  # Regressionslinien (für Low und High getrennt)
  geom_smooth(aes(color = factor(Involvement), fill = factor(Involvement)), 
              method = "lm", alpha = 0.2) +
  
  # Farben & Labels manuell setzen (Blau für Low, Rot für High)
  scale_color_manual(values = c("0"="#3b82f6", "1"="#ef4444"), 
                     labels = c("Low Inv.", "High Inv."), name = "Cluster") +
  scale_fill_manual(values = c("0"="#3b82f6", "1"="#ef4444"), 
                    labels = c("Low Inv.", "High Inv."), name = "Cluster") +
  
  # Design und Beschriftung
  labs(
    title = "Hypothese 5: Einfluss von Buzz auf Brand Performance",
    subtitle = paste("Aggregierte Performance (Mittelwert aus 4 KPIs) | n =", nrow(df_analysis)),
    x = "Brand Buzz",
    y = "Brand Performance (Aggregiert)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# PLOT B: Facet Grid (Getrennte Fenster für Low/High)
# Trennt die Grafik in zwei Boxen für besseren Vergleich.
p2 <- ggplot(df_analysis, aes(x = Buzz, y = Brand_Performance)) +
  geom_point(alpha = 0.3, color = "darkgrey") +
  
  # WICHTIG: Hier 'linewidth' statt 'size' nutzen, um Warnung zu vermeiden!
  geom_smooth(method = "lm", color = "black", linewidth = 1) +
  
  # Aufteilung in zwei Fenster (Facets)
  facet_wrap(~ ifelse(Involvement == 0, "Low Involvement", "High Involvement")) +
  
  labs(
    title = "Detailansicht: Vergleich der Cluster",
    subtitle = "Steigung der Linie zeigt die Stärke des Buzz-Effekts",
    x = "Buzz",
    y = "Brand Performance"
  ) +
  theme_bw()

print(p2)

dev.off() # PDF schließen
cat("Info: 'H5_Visualisierung_Buzz_vs_Performance.pdf' wurde erstellt.\n")
cat("\n--- ANALYSE ABGESCHLOSSEN ---\n")