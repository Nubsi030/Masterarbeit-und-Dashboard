# ============================================================================
# Double Jeopardy Law Analysis - VEREINFACHTE & KORRIGIERTE VERSION
# ============================================================================

library(tidyverse)

# ============================================================================
# 1. GLOBALE ANALYSE
# ============================================================================

global_correlation <- cor.test(KPIs_alle1864$Usage, KPIs_alle1864$Loyalty, 
                               method = "pearson")

cat("\n========== GLOBALE ANALYSE ==========\n")
cat("Korrelationskoeffizient (r):", round(global_correlation$estimate, 4), "\n")
cat("p-Wert:", global_correlation$p.value, "\n")
cat("95% Konfidenzintervall:", 
    round(global_correlation$conf.int[1], 4), "bis", 
    round(global_correlation$conf.int[2], 4), "\n\n")

# ============================================================================
# 2. BRANCHEN-ANALYSE - VEREINFACHTE VERSION
# ============================================================================

kpi_clean <- KPIs_alle1864 %>%
  filter(!is.na(Usage), !is.na(Loyalty), !is.na(Industry))

# Zähle Branchen
branch_counts <- kpi_clean %>%
  group_by(Industry) %>%
  summarise(n = n(), .groups = "drop")

# Berechne Korrelation nur für Branchen mit n >= 3
branch_correlation <- branch_counts %>%
  filter(n >= 3) %>%
  pull(Industry) %>%
  map_df(~{
    data <- kpi_clean %>% filter(Industry == .x)
    test <- cor.test(data$Usage, data$Loyalty, method = "pearson")
    
    tibble(
      Industry = .x,
      n = nrow(data),
      r = test$estimate,
      p_value = test$p.value
    )
  }) %>%
  arrange(desc(abs(r)))

# Ausgabe
cat("========== BRANCHEN-ANALYSE (sortiert nach Korrelation) ==========\n")
print(branch_correlation)

# Formatierte Tabelle
branch_corr_table <- branch_correlation %>%
  mutate(
    r = round(r, 4),
    p_value = round(p_value, 6),
    significant = ifelse(p_value < 0.05, "Ja", "Nein")
  ) %>%
  select(Industry, n, r, p_value, significant) %>%
  rename(
    Branche = Industry,
    "n (Marken)" = n,
    "Korr. (r)" = r,
    "p-Wert" = p_value,
    "Signifikant (α=0.05)" = significant
  )

cat("\n========== ÜBERSICHTSTABELLE ==========\n")
print(branch_corr_table)

# ============================================================================
# 3. VISUALISIERUNG
# ============================================================================

kpi_plot <- kpi_clean %>%
  semi_join(branch_correlation %>% select(Industry), 
            by = "Industry")

plot_double_jeopardy <- kpi_plot %>%
  ggplot(aes(x = Usage, y = Loyalty)) +
  geom_point(size = 2.5, alpha = 0.6, color = "#2E86AB") +
  geom_smooth(method = "lm", se = TRUE, color = "#A23B72", 
              fill = "#A23B72", alpha = 0.15, size = 1) +
  facet_wrap(~Industry, scales = "free") +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    panel.border = element_rect(color = "gray30", fill = NA, size = 0.5),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "gray50", margin = margin(b = 15)),
    axis.title = element_text(size = 10, face = "bold")
  ) +
  labs(
    title = "Double Jeopardy Law: Usage vs. Loyalty nach Branche",
    subtitle = "Nur Branchen mit mindestens 3 Marken (n ≥ 3)",
    x = "Marktanteil / Usage",
    y = "Markentreue / Loyalty",
    caption = "Quelle: KPIs_alle1864"
  )

print(plot_double_jeopardy)

ggsave("double_jeopardy_analysis.png", plot_double_jeopardy, 
       width = 14, height = 10, dpi = 300, bg = "white")

# ============================================================================
# 4. ZUSAMMENFASSUNG - KORRIGIERT
# ============================================================================

cat("\n========== ZUSAMMENFASSUNG ==========\n")

r_global <- global_correlation$estimate

# Effektgröße nach Cohen bestimmen
if (abs(r_global) < 0.1) {
  effect_size <- "Vernachlässigbar"
} else if (abs(r_global) < 0.3) {
  effect_size <- "Schwach"
} else if (abs(r_global) < 0.5) {
  effect_size <- "Mittel"
} else {
  effect_size <- "Stark"
}

cat("Globale Effektgröße (Cohen):", effect_size, "\n")
cat("Globale Korrelation (r):", round(r_global, 4), "\n")

sig_count <- sum(branch_correlation$p_value < 0.05, na.rm = TRUE)
cat("Signifikante Branchen (p < 0.05):", sig_count, "von", 
    nrow(branch_correlation), "\n")

cat("\nDouble Jeopardy Law wird", 
    ifelse(global_correlation$p.value < 0.05, "✓ BESTÄTIGT", "✗ NICHT BESTÄTIGT"),
    "(p =", round(global_correlation$p.value, 6), ")\n")

excluded <- nrow(branch_counts %>% filter(n < 3))
cat("\nBranchen ausgeschlossen (n < 3):", excluded, "von", 
    nrow(branch_counts), "\n")

# ============================================================================
# BONUS: Top 10 stärkste Korrelationen
# ============================================================================

cat("\n========== TOP 10 STÄRKSTE KORRELATIONEN ==========\n")
top10 <- branch_correlation %>%
  slice_head(n = 10) %>%
  mutate(
    r = round(r, 4),
    p_value = round(p_value, 6)
  ) %>%
  select(Industry, n, r, p_value)

print(top10)
