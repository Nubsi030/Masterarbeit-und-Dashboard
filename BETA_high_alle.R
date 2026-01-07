# ==============================================================================
# BETA REGRESSION: HIGH INVOLVEMENT (Multi-KPI Analyse)
# ==============================================================================
# ZIEL:
# Analyse von Popularity, Usage und Loyalty getrieben durch Buzz.
# Fokus: High Involvement Produkte (Involvement == 1).
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
if(!require(betareg)) install.packages("betareg")
if(!require(dplyr)) install.packages("dplyr")
if(!require(broom)) install.packages("broom")
if(!require(tidyr)) install.packages("tidyr")

library(betareg)
library(dplyr)
library(broom)
library(tidyr)

if (!exists("KPIs_alle1864")) {
  stop("FEHLER: Datensatz 'KPIs_alle1864' fehlt.")
}

df_raw <- KPIs_alle1864

# ------------------------------------------------------------------------------
# 2. FILTERUNG: HIGH INVOLVEMENT
# ------------------------------------------------------------------------------
# Hier liegt der Unterschied zum anderen Skript: Involvement == 1
df_high <- df_raw %>% 
  filter(Involvement == 1)

cat(paste("Analyse gestartet für HIGH INVOLVEMENT.\nAnzahl Beobachtungen:", nrow(df_high), "\n"))

target_kpis <- c("Popularity", "Usage", "Loyalty")
predictor_var <- "Buzz"

# ------------------------------------------------------------------------------
# 3. ANALYSE-FUNKTION (Identisch zur Low-Version für Konsistenz)
# ------------------------------------------------------------------------------
analyze_kpi <- function(data, kpi_name, predictor) {
  
  cat(paste0("\n>>> Starte Analyse für Zielvariable: ", kpi_name, " <<<\n"))
  
  # A. Datenvorbereitung (Selektion & NA-Bereinigung)
  work_data <- data %>%
    select(Group = Industry, Target = all_of(kpi_name), Predictor = all_of(predictor)) %>%
    filter(!is.na(Target) & !is.na(Predictor))
  
  # B. Transformation (0/1 Problem lösen)
  y_raw <- work_data$Target
  n <- length(y_raw)
  
  if (any(y_raw <= 0) | any(y_raw >= 1)) {
    work_data$Target_Transformed <- (y_raw * (n - 1) + 0.5) / n
  } else {
    work_data$Target_Transformed <- y_raw
  }
  
  # C. Loop über Industrien
  results_list <- list()
  industries <- unique(work_data$Group)
  
  for(ind in industries) {
    sub_df <- work_data %>% filter(Group == ind)
    
    if(nrow(sub_df) < 5) next 
    
    tryCatch({
      m <- betareg(Target_Transformed ~ Predictor, data = sub_df)
      
      res <- tidy(m) %>%
        filter(term == "Predictor") %>%
        mutate(
          KPI = kpi_name,
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
      
    }, error = function(e) { })
  }
  
  return(bind_rows(results_list))
}

# ------------------------------------------------------------------------------
# 4. DURCHFÜHRUNG
# ------------------------------------------------------------------------------

all_results_high <- list()

for (kpi in target_kpis) {
  if (kpi %in% names(df_high)) {
    all_results_high[[kpi]] <- analyze_kpi(df_high, kpi, predictor_var)
  } else {
    warning(paste("Spalte", kpi, "nicht gefunden!"))
  }
}

final_table_high <- bind_rows(all_results_high) %>%
  arrange(KPI, desc(Estimate))

# ------------------------------------------------------------------------------
# 5. ERGEBNIS (Deutsch)
# ------------------------------------------------------------------------------

final_table_high_de <- final_table_high %>%
  mutate(
    Estimate = format(Estimate, decimal.mark = ",", digits = 5, nsmall = 5),
    Std_Error = format(Std_Error, decimal.mark = ",", digits = 5, nsmall = 5),
    Test_Statistic = format(Test_Statistic, decimal.mark = ",", digits = 5, nsmall = 5),
    R_Squared = format(R_Squared, decimal.mark = ",", digits = 5, nsmall = 5),
    P_Value = format(P_Value, scientific = TRUE, digits = 5, decimal.mark = ",")
  )

cat("\n==========================================================\n")
cat(" ERGEBNISSE HIGH INVOLVEMENT (Auszug)\n")
cat("==========================================================\n")
print(head(final_table_high_de, 15))

# Optional: Speichern
# write.csv2(final_table_high_de, "Results_High_Involvement_MultiKPI.csv", row.names = FALSE)