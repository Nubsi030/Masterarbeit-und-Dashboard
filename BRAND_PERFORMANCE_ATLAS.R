# ==============================================================================
# BRAND PERFORMANCE ATLAS: UMFASSENDE VISUALISIERUNG (Auto-Fix Version)
# ==============================================================================
# ZIEL:
# Erstellung eines PDF-Berichts mit Streudiagrammen (Scatterplots) und
# Regressionslinien für ALLE Ebenen:
# 1. Gesamtmarkt
# 2. Pro Sektor (Branchen)
# 3. Pro Industrie (mit Marken-Labels)
#
# X-Achse: Immer "Buzz"
# Y-Achsen: Awareness, Popularity, Usage, Loyalty (Loop durch alle)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & PAKETE
# ------------------------------------------------------------------------------
if(!require(ggplot2)) install.packages("ggplot2")   # Für die Grafiken
if(!require(dplyr)) install.packages("dplyr")       # Für Datenmanipulation
if(!require(ggrepel)) install.packages("ggrepel")   # Damit sich Text-Labels nicht überlappen
if(!require(scales)) install.packages("scales")     # Für schöne Achsen

library(ggplot2)
library(dplyr)
library(ggrepel)
library(scales)

# Daten laden (Sicherheitscheck)
if (!exists("KPIs_alle1864")) {
  stop("FEHLER: Bitte lade zuerst den Datensatz 'KPIs_alle1864'.")
}

df_viz <- KPIs_alle1864

# --- AUTO-DETECTION: WIE HEISST DIE MARKEN-SPALTE? ---
possible_names <- c("Brand", "Marke", "Company", "Name", "Organization", "Marken")
# Wir nehmen den ersten Namen, der im Datensatz gefunden wird
brand_col_name <- intersect(names(df_viz), possible_names)[1]

if(is.na(brand_col_name)) {
  # Fallback: Wenn gar nichts gefunden wird, brechen wir kontrolliert ab und zeigen die Namen an
  print(names(df_viz))
  stop("FEHLER: Konnte keine Spalte für den Markennamen finden (z.B. 'Brand', 'Marke'). Bitte prüfe deine Spaltennamen oben.")
} else {
  cat(paste("Info: Nutze Spalte '", brand_col_name, "' als Markennamen.\n", sep=""))
}

# --- AUTO-DETECTION: WIE HEISST DIE SEKTOR-SPALTE? ---
# Hier habe ich 'übergeordneter Sektor' hinzugefügt
possible_sector_names <- c("Sector", "Sektor", "Category", "Kategorie", "Branche", "Cluster", "übergeordneter Sektor", "uebergeordneter Sektor")
# Wir nehmen den ersten Namen, der im Datensatz gefunden wird
sector_col_name <- intersect(names(df_viz), possible_sector_names)[1]

if(is.na(sector_col_name)) {
  # Fallback
  print(names(df_viz))
  stop("FEHLER: Konnte keine Spalte für den Sektor finden. Bitte prüfe, ob die Spalte 'übergeordneter Sektor' oder ähnlich heißt.")
} else {
  cat(paste("Info: Nutze Spalte '", sector_col_name, "' als Sektor.\n", sep=""))
}

# Definieren der KPIs, die wir plotten wollen
kpis_to_plot <- c("Awareness", "Popularity", "Usage", "Loyalty")

# Dateiname für den Output
output_filename <- "Brand_Performance_Atlas.pdf"

# ------------------------------------------------------------------------------
# 2. PDF-GENERATOR STARTEN
# ------------------------------------------------------------------------------
# Wir öffnen ein PDF-Gerät. Alle folgenden Plots werden in diese Datei geschrieben.
pdf(output_filename, width = 11, height = 8) # Querformat DIN A4

# ------------------------------------------------------------------------------
# 3. DER GROSSE LOOP (Durch alle KPIs)
# ------------------------------------------------------------------------------
for (current_kpi in kpis_to_plot) {
  
  # Prüfen, ob KPI im Datensatz ist
  if(!current_kpi %in% names(df_viz)) next
  
  cat(paste("Erstelle Grafiken für KPI:", current_kpi, "...\n"))
  
  # Temporären Datensatz für diesen KPI erstellen (NAs entfernen)
  # WICHTIG: Wir benennen die gefundenen Spalten temporär um, damit der Rest des Codes funktioniert
  plot_data <- df_viz %>%
    select(
      Brand = all_of(brand_col_name), 
      Sector = all_of(sector_col_name), 
      Industry, 
      Buzz, 
      Target = all_of(current_kpi), 
      Involvement
    ) %>%
    filter(!is.na(Buzz) & !is.na(Target))
  
  # ============================================================================
  # LEVEL 1: GLOBALER ÜBERBLICK (Alle Marken in einem Plot)
  # ============================================================================
  
  p_global <- ggplot(plot_data, aes(x = Buzz, y = Target)) +
    # Punkte: Farbe nach Involvement (High/Low)
    geom_point(aes(color = factor(Involvement)), alpha = 0.4, size = 1.5) +
    
    # Regressionslinie (Linear Model 'lm') für den Gesamttrend
    geom_smooth(method = "lm", color = "black", se = TRUE, linetype = "dashed") +
    
    # Farben für Involvement
    scale_color_manual(values = c("0" = "blue", "1" = "red"), 
                       labels = c("Low Inv.", "High Inv."), name = "Cluster") +
    
    # Design
    theme_minimal() +
    labs(title = paste("GLOBALER ÜBERBLICK:", current_kpi, "vs. Buzz"),
         subtitle = paste("Jeder Punkt ist eine Marke (n =", nrow(plot_data), ")"),
         x = "Brand Buzz",
         y = current_kpi)
  
  print(p_global) # Plot in PDF schreiben
  
  # ============================================================================
  # LEVEL 2: SEKTOR-ANALYSE (Ein Plot pro Sektor)
  # ============================================================================
  # Wir loopen durch jeden Sektor (z.B. Food, Tech, Finance)
  
  sectors <- unique(plot_data$Sector)
  
  for(sec in sectors) {
    sec_data <- plot_data %>% filter(Sector == sec)
    
    if(nrow(sec_data) < 5) next # Überspringen, wenn zu wenig Daten
    
    p_sector <- ggplot(sec_data, aes(x = Buzz, y = Target)) +
      # Punkte einfärben nach Industrie innerhalb des Sektors
      geom_point(aes(color = Industry), alpha = 0.6, size = 2) +
      
      # Regressionslinie für diesen Sektor
      geom_smooth(method = "lm", color = "darkgrey", se = FALSE) +
      
      theme_minimal() +
      # Legende ausblenden, wenn es zu viele Industrien sind (>15), sonst wird der Plot unlesbar
      theme(legend.position = if(length(unique(sec_data$Industry)) > 15) "none" else "right") +
      
      labs(title = paste("SEKTOR:", sec, "-", current_kpi),
           subtitle = "Gefärbt nach Industrien",
           x = "Buzz", y = current_kpi)
    
    print(p_sector)
  }
  
  # ============================================================================
  # LEVEL 3: INDUSTRIE-DETAILS (Ein Plot pro Industrie mit MARKEN-LABELS)
  # ============================================================================
  # Das ist der detaillierteste Teil. Hier sehen wir einzelne Marken.
  
  industries <- unique(plot_data$Industry)
  
  for(ind in industries) {
    ind_data <- plot_data %>% filter(Industry == ind)
    
    # Nur plotten, wenn genug Marken da sind (>3)
    if(nrow(ind_data) < 3) next
    
    # Berechnung der Korrelation für den Untertitel
    corr_val <- cor(ind_data$Buzz, ind_data$Target, use = "complete.obs")
    
    p_ind <- ggplot(ind_data, aes(x = Buzz, y = Target)) +
      # Regressionslinie zuerst (damit sie hinter den Punkten liegt)
      geom_smooth(method = "lm", color = "red", alpha = 0.1, se = TRUE) +
      
      # Die Marken als Punkte
      geom_point(color = "darkblue", size = 3, alpha = 0.7) +
      
      # HIER IST DER TRICK: Marken-Namen als Labels
      # Wir nutzen ggrepel, damit die Texte sich nicht überlappen
      geom_text_repel(aes(label = Brand), 
                      size = 3, 
                      max.overlaps = 15, # Max Labels, um Chaos zu vermeiden
                      box.padding = 0.3) +
      
      theme_bw() + # Klare Box für Details
      labs(title = paste("INDUSTRIE:", ind),
           subtitle = paste("KPI:", current_kpi, "| Korrelation r =", round(corr_val, 2)),
           x = "Buzz", y = current_kpi)
    
    print(p_ind)
  }
}

# ------------------------------------------------------------------------------
# 4. ABSCHLUSS
# ------------------------------------------------------------------------------
dev.off() # Schließt die PDF-Datei und speichert sie

cat(paste("\nFERTIG! Der Atlas wurde erstellt.\nDatei:", output_filename, "\n"))
cat("Bitte öffne die PDF-Datei in deinem Arbeitsverzeichnis, um die Grafiken zu sehen.")