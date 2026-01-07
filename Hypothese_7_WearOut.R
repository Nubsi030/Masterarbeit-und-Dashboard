# ==============================================================================
# HYPOTHESE 7: MERE-EXPOSURE & WEAR-OUT EFFEKTE (QUADRATISCHE ANALYSE)
# ==============================================================================
# 
# ZIEL:
# Prüfung auf nicht-lineare Zusammenhänge (kurvilinear).
# 1. Mere-Exposure: Positiver Basistrend.
# 2. Wear-Out: Abflachung oder Negativtrend bei sehr hohem Buzz (Inverted U).
#
# METHODIK:
# Vergleich von zwei Beta-Regressions-Modellen mittels Likelihood-Ratio-Test:
# - Modell 1 (Linear): Popularity ~ Buzz
# - Modell 2 (Quadratisch): Popularity ~ Buzz + Buzz^2
#
# INTERPRETATION:
# Wenn der quadratische Term (I(Buzz^2)) signifikant NEGATIV ist,
# liegt ein Sättigungs- oder Wear-Out-Effekt vor (Kurve biegt sich nach unten).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
if(!require(betareg)) install.packages("betareg")
if(!require(dplyr)) install.packages("dplyr")
if(!require(broom)) install.packages("broom")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lmtest)) install.packages("lmtest") # Für den Modellvergleich (lrtest)

library(betareg)
library(dplyr)
library(broom)
library(ggplot2)
library(lmtest)

# Datensatz laden Check
if (!exists("KPIs_alle1864")) {
  stop("FEHLER: Bitte lade zuerst den Datensatz 'KPIs_alle1864'.")
}

df_raw <- KPIs_alle1864

# Daten vorbereiten (Smithson Transformation für Beta Regression)
df_analysis <- df_raw %>%
  filter(!is.na(Buzz), !is.na(Popularity)) %>%
  mutate(
    # Transformation für Beta-Reg (0/1 vermeiden)
    Popularity_Trans = (Popularity * (n() - 1) + 0.5) / n()
  )

# ------------------------------------------------------------------------------
# 2. FUNKTION: WEAR-OUT TEST ENGINE
# ------------------------------------------------------------------------------
run_wearout_test <- function(data, label) {
  
  cat(paste0("\n>>> Analysiere: ", label, " <<<\n"))
  
  # A. Lineares Modell (Referenz)
  m_lin <- betareg(Popularity_Trans ~ Buzz, data = data)
  
  # B. Quadratisches Modell (Test auf Wear-Out)
  # I(Buzz^2) erzeugt den quadratischen Term
  m_quad <- betareg(Popularity_Trans ~ Buzz + I(Buzz^2), data = data)
  
  # C. Modellvergleich (Ist das quadratische Modell signifikant besser?)
  lr_result <- lrtest(m_lin, m_quad)
  p_val_improvement <- lr_result$`Pr(>Chisq)`[2]
  
  # D. Koeffizienten des quadratischen Modells holen
  res <- tidy(m_quad)
  
  beta_lin <- res$estimate[res$term == "Buzz"]
  beta_quad <- res$estimate[res$term == "I(Buzz^2)"]
  p_val_quad <- res$p.value[res$term == "I(Buzz^2)"]
  
  # E. Interpretation
  # Wear-Out liegt vor, wenn Beta_Quadratisch signifikant NEGATIV ist.
  wear_out_detected <- (beta_quad < 0 & p_val_quad < 0.05)
  
  # Berechnung des "Kipp-Punkts" (Scheitelpunkt der Parabel): x = -b / 2a
  # Ab diesem Buzz-Wert würde der Effekt negativ werden.
  turning_point <- -beta_lin / (2 * beta_quad)
  
  # Liegt der Turning Point im realen Datenbereich?
  max_buzz_in_data <- max(data$Buzz, na.rm=TRUE)
  is_in_range <- (turning_point > 0 & turning_point < max_buzz_in_data)
  
  cat(paste("   Lineare Komponente (Beta):", round(beta_lin, 4), "\n"))
  cat(paste("   Quadratische Komponente:", round(beta_quad, 4), "\n"))
  cat(paste("   Signifikanz (p):", format(p_val_quad, scientific=TRUE, digits=4), "\n"))
  cat(paste("   Modellverbesserung (LR-Test):", format(p_val_improvement, scientific=TRUE, digits=4), "\n"))
  
  if(wear_out_detected) {
    cat("   ERGEBNIS: Wear-Out / Sättigung statistisch bestätigt.\n")
    cat(paste("   Theoretischer Kipp-Punkt (Maximaler Effekt) bei Buzz =", round(turning_point, 2), "\n"))
    if(is_in_range) {
      cat("   ACHTUNG: Dieser Punkt liegt innerhalb deiner Daten! Manche Marken haben zu viel Buzz.\n")
    } else {
      cat("   Info: Der Kipp-Punkt liegt außerhalb der Daten (Diminishing Returns, aber kein absoluter Abfall).\n")
    }
  } else {
    cat("   ERGEBNIS: Kein Wear-Out nachweisbar (Beziehung ist linear oder exponentiell).\n")
  }
  
  return(data.frame(Group=label, Beta_Lin=beta_lin, Beta_Quad=beta_quad, Turning_Point=turning_point, Sig=p_val_quad))
}

# ------------------------------------------------------------------------------
# 3. DURCHFÜHRUNG DER ANALYSE
# ------------------------------------------------------------------------------

# Gesamtmarkt
res_total <- run_wearout_test(df_analysis, "GESAMTMARKT")

# Low Involvement
res_low <- run_wearout_test(df_analysis %>% filter(Involvement == 0), "LOW INVOLVEMENT")

# High Involvement
res_high <- run_wearout_test(df_analysis %>% filter(Involvement == 1), "HIGH INVOLVEMENT")


# ------------------------------------------------------------------------------
# 4. VISUALISIERUNG (DIE KURVE)
# ------------------------------------------------------------------------------
cat("\n--- Erstelle Visualisierung 'Wear_Out_Analysis.pdf' ---\n")

pdf("Wear_Out_Analysis.pdf", width = 10, height = 6)

p <- ggplot(df_analysis, aes(x = Buzz, y = Popularity)) +
  # Echte Datenpunkte
  geom_point(aes(color = factor(Involvement)), alpha = 0.3, size = 1.5) +
  
  # Die quadratische Regressionskurve (Modell visualisiert)
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              aes(color = factor(Involvement), fill = factor(Involvement)), 
              alpha = 0.2, linewidth = 1.2) +
  
  scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                     labels = c("Low Inv.", "High Inv."), name = "Cluster") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), 
                    labels = c("Low Inv.", "High Inv."), name = "Cluster") +
  
  labs(title = "Hypothese 7: Mere-Exposure vs. Wear-Out",
       subtitle = "Quadratischer Fit: Biegt sich die Kurve nach unten (Sättigung)?",
       x = "Brand Buzz",
       y = "Brand Popularity") +
  
  theme_minimal()

print(p)
dev.off()
cat("Info: PDF gespeichert.\n")
summary(res_total)
