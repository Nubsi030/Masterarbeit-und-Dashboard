# ==============================================================================
# HYPOTHESE 6 (a-e): MODERATION DURCH INVOLVEMENT (Z-TEST mit BETA-REGRESSION)
# ==============================================================================
# 
# ZIEL:
# Prüfung, ob der Effekt von Buzz auf die KPIs im "Low Involvement"-Cluster
# signifikant stärker ist als im "High Involvement"-Cluster.
#
# METHODIK (Update auf Beta-Regression für Konsistenz):
# 1. Beta-Regression (betareg) getrennt für Low und High.
#    (Vorteil: Berücksichtigt Grenzen [0,1] und ist konsistent mit H1-H5).
# 2. Transformation: Smithson-Verfahren für Werte 0 oder 1.
# 3. Signifikanztest: Z-Test des Unterschieds der Koeffizienten.
#    H0: Beta_Low = Beta_High
#    H1: Beta_Low > Beta_High (Einseitiger Test)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & PAKETE
# ------------------------------------------------------------------------------
if(!require(dplyr)) install.packages("dplyr")       # Datenmanipulation
if(!require(broom)) install.packages("broom")       # Tidy Output
if(!require(betareg)) install.packages("betareg")   # Beta Regression
if(!require(ggplot2)) install.packages("ggplot2")   # Plots

library(dplyr)
library(broom)
library(betareg)
library(ggplot2)

# Datensatz laden Check
if (!exists("KPIs_alle1864")) {
  stop("FEHLER: Bitte lade zuerst den Datensatz 'KPIs_alle1864'.")
}

df_raw <- KPIs_alle1864

# Datenvorbereitung: Variable "Brand_Performance" erstellen & NAs reinigen
df_mod <- df_raw %>%
  rowwise() %>%
  mutate(Brand_Performance = mean(c(Awareness, Popularity, Usage, Loyalty), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(Buzz), !is.na(Involvement))

# ------------------------------------------------------------------------------
# 2. FUNKTION: BETA-REGRESSION MIT TRANSFORMATION
# ------------------------------------------------------------------------------
# Diese Funktion rechnet das Modell für EINE Gruppe und gibt Beta & SE zurück.

run_beta_model <- function(data, kpi_name) {
  
  # A. Transformation (Smithson), da Beta-Regression (0,1) braucht
  y <- data[[kpi_name]]
  n <- length(y)
  
  # Daten temporär transformieren
  if (any(y <= 0) | any(y >= 1)) {
    y_trans <- (y * (n - 1) + 0.5) / n
  } else {
    y_trans <- y
  }
  
  # Temporären Dataframe für das Modell erstellen
  model_data <- data
  model_data$Target_Variable <- y_trans
  
  # B. Modell rechnen
  tryCatch({
    model <- betareg(Target_Variable ~ Buzz, data = model_data)
    
    # Koeffizienten extrahieren
    res_tidy <- tidy(model) %>% filter(term == "Buzz")
    
    return(list(
      beta = res_tidy$estimate,
      se = res_tidy$std.error,
      n = nobs(model),
      r2 = summary(model)$pseudo.r.squared,
      status = "OK"
    ))
    
  }, error = function(e) {
    return(list(beta = NA, se = NA, n = n, r2 = NA, status = "Error"))
  })
}

# ------------------------------------------------------------------------------
# 3. FUNKTION: Z-TEST FÜR GRUPPENVERGLEICH (BETA-BASIERT)
# ------------------------------------------------------------------------------
# Vergleicht Beta_Low vs. Beta_High (auf Logit-Skala)

compare_groups <- function(df, kpi, hyp_label) {
  
  # Daten splitten
  data_low <- df %>% filter(Involvement == 0)
  data_high <- df %>% filter(Involvement == 1)
  
  # Modelle rechnen
  res_low <- run_beta_model(data_low, kpi)
  res_high <- run_beta_model(data_high, kpi)
  
  # Check auf Fehler
  if(res_low$status == "Error" | res_high$status == "Error") {
    return(tibble(Hypothese = hyp_label, Entscheidung = "Modell-Fehler"))
  }
  
  # --- DER Z-TEST ---
  # Wir vergleichen die Steigungen (Log-Odds)
  # H1: Low > High (Einseitig)
  
  diff_beta <- res_low$beta - res_high$beta
  pooled_se <- sqrt(res_low$se^2 + res_high$se^2)
  z_score   <- diff_beta / pooled_se
  
  # P-Wert (einseitig: lower.tail = FALSE prüft ob Z > 0)
  p_val <- pnorm(z_score, lower.tail = FALSE)
  
  # Entscheidung
  decision <- ifelse(p_val < 0.05, "H1 ANGENOMMEN", "H0 BEIBEHALTEN")
  
  # Output zusammenstellen
  return(tibble(
    Hypothese = hyp_label,
    KPI = kpi,
    Beta_Low = res_low$beta,
    SE_Low = res_low$se,
    Beta_High = res_high$beta,
    SE_High = res_high$se,
    Beta_Diff = diff_beta,
    Z_Score = z_score,
    P_Value_1tailed = p_val,
    Entscheidung = decision
  ))
}

# ------------------------------------------------------------------------------
# 4. ANALYSE DURCHFÜHREN (H6a - H6e)
# ------------------------------------------------------------------------------

cat("\n--- Starte Moderationsanalyse (Beta-Regression Z-Test) ---\n")

# H6a: Awareness
res_h6a <- compare_groups(df_mod, "Awareness", "H6a (Awareness)")

# H6b: Popularity
res_h6b <- compare_groups(df_mod, "Popularity", "H6b (Popularity)")

# H6c: Usage
res_h6c <- compare_groups(df_mod, "Usage", "H6c (Usage)")

# H6d: Loyalty
res_h6d <- compare_groups(df_mod, "Loyalty", "H6d (Loyalty)")

# H6e: Performance (Aggregiert)
res_h6e <- compare_groups(df_mod, "Brand_Performance", "H6e (Performance)")

# Alle Ergebnisse binden
results_h6 <- bind_rows(res_h6a, res_h6b, res_h6c, res_h6d, res_h6e)

# ------------------------------------------------------------------------------
# 5. FORMATIERUNG & AUSGABE
# ------------------------------------------------------------------------------

# Hilfsfunktion für deutsche Formatierung
fmt <- function(x) format(round(x, 4), decimal.mark = ",", nsmall = 4)

results_h6_de <- results_h6 %>%
  mutate(
    # Signifikanz-Sterne berechnen
    Sig = case_when(
      P_Value_1tailed < 0.001 ~ "***",
      P_Value_1tailed < 0.01  ~ "**",
      P_Value_1tailed < 0.05  ~ "*",
      P_Value_1tailed < 0.1   ~ ".",
      TRUE                    ~ "n.s."
    ),
    Beta_Low = fmt(Beta_Low),
    Beta_High = fmt(Beta_High),
    SE_Low = fmt(SE_Low),
    SE_High = fmt(SE_High),
    Z_Score = fmt(Z_Score),
    P_Value = format(P_Value_1tailed, decimal.mark = ",", scientific = TRUE, digits = 4)
  ) %>%
  select(Hypothese, Beta_Low, Beta_High, Z_Score, P_Value, Sig, Entscheidung)

cat("\n=================================================================================\n")
cat(" ERGEBNISSE HYPOTHESE 6: MODERATION DURCH INVOLVEMENT (BETA REGRESSION)\n")
cat(" (H1: Effekt in Low > Effekt in High)\n")
cat("=================================================================================\n")
print(results_h6_de)

# NEU: Speichern der Ergebnisse als CSV für Excel (Deutsches Format mit Semikolon)
write.csv2(results_h6_de, "H6_Moderation_Results.csv", row.names = FALSE)
cat("\nInfo: Tabelle 'H6_Moderation_Results.csv' wurde erfolgreich gespeichert.\n")


# ------------------------------------------------------------------------------
# 6. VISUALISIERUNG (INTERACTION PLOTS A-E)
# ------------------------------------------------------------------------------
cat("\n--- Erstelle Visualisierungen für alle Hypothesen (H6a-e) ---\n")

# Wir speichern ALLE Grafiken in einer mehrseitigen PDF
pdf("H6_All_Moderation_Plots.pdf", width = 10, height = 7)

# Liste der zu visualisierenden KPIs (Mapping auf Hypothesen)
plot_config <- list(
  list(kpi = "Awareness", title = "H6a: Awareness", desc = "Moderierender Effekt von Involvement"),
  list(kpi = "Popularity", title = "H6b: Popularity", desc = "Moderierender Effekt von Involvement"),
  list(kpi = "Usage", title = "H6c: Usage", desc = "Moderierender Effekt von Involvement"),
  list(kpi = "Loyalty", title = "H6d: Loyalty", desc = "Moderierender Effekt von Involvement"),
  list(kpi = "Brand_Performance", title = "H6e: Brand Performance", desc = "Aggregierter Moderationseffekt")
)

# Loop durch die Konfiguration
for (conf in plot_config) {
  
  # Aktueller KPI
  curr_kpi <- conf$kpi
  
  # Plot erstellen
  # Wir nutzen .data[[curr_kpi]], um die Variable dynamisch anzusprechen
  p <- ggplot(df_mod, aes(x = Buzz, y = .data[[curr_kpi]], color = factor(Involvement))) +
    # Punkte im Hintergrund (transparent)
    geom_point(alpha = 0.15, size = 1.5) + 
    
    # Regressionskurven (GLM Quasibinomial imitiert die Logit-Link-Funktion der Beta-Reg)
    geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), 
                se = TRUE, linewidth = 1.5) +
    
    # Farben (Low = Blau, High = Rot)
    scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                       labels = c("Low Inv.", "High Inv."), name = "Cluster") +
    
    # Beschriftung
    labs(title = conf$title,
         subtitle = paste(conf$desc, "- Vergleich der Steigungen (Blau vs. Rot)"),
         x = "Brand Buzz",
         y = curr_kpi) +
    
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 14))
  
  print(p)
}

dev.off()

cat("\nInfo: Datei 'H6_All_Moderation_Plots.pdf' wurde erfolgreich erstellt.\n")