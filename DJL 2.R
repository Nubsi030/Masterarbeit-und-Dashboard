# --- KOMPLETTER R-CODE ZUR ANALYSE DES DOUBLE JEOPARDY LAW (Usage vs. Loyalty) ---

# Bibliotheken laden.
library(tidyverse)
library(broom)

# =========================================================================
# 1. DATENVORBEREITUNG
# =========================================================================

# Bereinigen des Spaltennamens 'Industry sector' zu 'Industry' für einfachere Handhabung
if ("Industry sector" %in% names(KPIs_alle1864)) {
  KPIs_alle1864 <- KPIs_alle1864 %>%
    rename(Industry = `Industry sector`)
}

# Entfernen von Zeilen mit fehlenden Werten in den Schlüsselspalten
KPIs_alle1864 <- KPIs_alle1864 %>%
  drop_na(Usage, Loyalty, Industry)

# =========================================================================
# 2. GLOBALE ANALYSE
# =========================================================================

# Pearson-Korrelation und p-Wert für den gesamten Datensatz berechnen
global_corr_result <- cor.test(KPIs_alle1864$Usage, KPIs_alle1864$Loyalty, method = "pearson")

cat("--- GLOBALE KORRELATIONSANALYSE (Usage vs. Loyalty) ---\n")
cat("Pearson-Korrelationskoeffizient (r):", round(global_corr_result$estimate, 4), "\n")
cat("p-Wert:", format.pval(global_corr_result$p.value, digits = 4), "\n")
cat("----------------------------------------------------------\n\n")

# =========================================================================
# 3. BRANCHEN-ANALYSE (Gruppiert nach Industry)
# =========================================================================

# Korrelation pro Branche berechnen und nach Korrelationsstärke sortieren
industry_correlation_df <- KPIs_alle1864 %>%
  # 1. Gruppieren nach Branche
  group_by(Industry) %>%
  # 2. Filtern: Nur Branchen mit mindestens 3 Beobachtungen (für cor.test notwendig)
  filter(n() >= 3) %>%
  # 3. Korrelationstest durchführen
  summarize(
    correlation_test = list(cor.test(Usage, Loyalty, method = "pearson")),
    .groups = 'drop'
  ) %>%
  # 4. Ergebnisse mithilfe von broom::tidy in saubere Spalten umwandeln
  unnest(correlation_test %>% map(tidy)) %>%
  # 5. Relevante Spalten auswählen und umbenennen
  select(Industry, `Korrelation (r)` = estimate, `p-Wert` = p.value, N = parameter) %>%
  # 6. Nach Korrelationsstärke (absteigend) sortieren
  arrange(desc(`Korrelation (r)`))

cat("--- BRANCHEN-KORRELATIONSANALYSE (Usage vs. Loyalty) ---\n")
print(industry_correlation_df)
cat("----------------------------------------------------------\n\n")

# =========================================================================
# 4. VISUALISIERUNG
# =========================================================================

# Professioneller Scatterplot mit linearer Trendlinie pro Branche
double_jeopardy_plot <- KPIs_alle1864 %>%
  # Filtern Sie dieselben Branchen wie für die Korrelationsanalyse
  group_by(Industry) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  
  ggplot(aes(x = Usage, y = Loyalty)) +
  
  # Scatter-Punkte
  geom_point(aes(color = Industry), alpha = 0.7, size = 3) +
  
  # Lineare Trendlinie (Linear Model - lm)
  geom_smooth(method = "lm",
              se = TRUE,         # Vertrauensintervall anzeigen
              color = "black",
              linewidth = 0.8) +
  
  # Facetierung nach Branche, Achsen freigeben
  facet_wrap(~ Industry, scales = "free") +
  
  # Beschriftungen und Titel
  labs(
    title = "Test des 'Double Jeopardy Law': Markentreue (Loyalty) vs. Nutzung (Usage)",
    subtitle = paste0(
      "Globale Pearson-Korrelation: r = ", round(global_corr_result$estimate, 3),
      " (p = ", format.pval(global_corr_result$p.value, digits = 3), "). ",
      "Eine positive Korrelation bestätigt das Gesetz."
    ),
    x = "Nutzung / Marktanteil (Usage)",
    y = "Markentreue (Loyalty)",
    caption = "Daten: KPIs_alle1864 | Lineare Regression pro Sektor dargestellt. (N >= 3)"
  ) +
  
  # Theme und Anpassungen
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0),
    legend.position = "none" # Legende deaktivieren
  )

# Plot anzeigen
print(double_jeopardy_plot)