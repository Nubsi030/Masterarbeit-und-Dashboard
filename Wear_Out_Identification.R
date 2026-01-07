# ==============================================================================
# IDENTIFIKATION DER "WEAR-OUT"-MARKEN (Update: 4 Nachkommastellen)
# ==============================================================================
# ZIEL:
# 1. Berechnung des exakten Sättigungspunktes (Turning Point) für High Involvement.
# 2. Identifikation der Marken, die diesen Punkt überschritten haben.
# 3. Visualisierung dieser spezifischen "Gefahrenzone".
# ==============================================================================

# 1. SETUP
if(!require(betareg)) install.packages("betareg")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel") # Für Beschriftungen

library(betareg)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Daten laden
if (!exists("KPIs_alle1864")) {
  stop("FEHLER: Bitte lade zuerst den Datensatz 'KPIs_alle1864'.")
}

# Wir fokussieren uns auf High Involvement (da hier der Effekt signifikant war)
df_high <- KPIs_alle1864 %>% 
  filter(Involvement == 1, !is.na(Buzz), !is.na(Popularity)) %>%
  mutate(
    # Transformation für Beta-Reg
    Popularity_Trans = (Popularity * (n() - 1) + 0.5) / n(),
    # Namen für Plot (Falls Spalte anders heißt, hier anpassen)
    Label_Name = if("Brand" %in% names(.)) Brand else Name
  )

# ------------------------------------------------------------------------------
# 2. BERECHNUNG DES KIPP-PUNKTS (TURNING POINT)
# ------------------------------------------------------------------------------
# Modell: Popularity ~ Buzz + Buzz^2
model_quad <- betareg(Popularity_Trans ~ Buzz + I(Buzz^2), data = df_high)

# Koeffizienten extrahieren
beta_lin <- coef(model_quad)["Buzz"]
beta_quad <- coef(model_quad)["I(Buzz^2)"]

# Kipp-Punkt Formel: x = -b / 2a
turning_point <- -beta_lin / (2 * beta_quad)

cat("\n=======================================================\n")
cat(" ANALYSE DER SÄTTIGUNG (HIGH INVOLVEMENT)\n")
cat("=======================================================\n")
# Formatierung auf exakt 4 Nachkommastellen erzwungen (nsmall=4)
cat(paste("Lineares Beta:      ", format(round(beta_lin, 4), nsmall = 4), "\n"))
cat(paste("Quadratisches Beta: ", format(round(beta_quad, 4), nsmall = 4), "\n"))
cat(paste("KIPP-PUNKT (Buzz):  ", format(round(turning_point, 4), nsmall = 4), "\n"))

# ------------------------------------------------------------------------------
# 3. IDENTIFIKATION DER BETROFFENEN MARKEN
# ------------------------------------------------------------------------------
# Welche Marken haben mehr Buzz als der Kipp-Punkt?
wearout_brands <- df_high %>% 
  filter(Buzz > turning_point) %>%
  select(Label_Name, Industry, Buzz, Popularity) %>%
  arrange(desc(Buzz))

cat("\n--- MARKEN IN DER WEAR-OUT ZONE (Buzz > Kipp-Punkt) ---\n")
print(wearout_brands)

# Speichern der Liste
write.csv2(wearout_brands, "Wear_Out_Brands_List.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 4. VISUALISIERUNG: "THE DANGER ZONE"
# ------------------------------------------------------------------------------
# Wir erstellen einen Plot, der den Kipp-Punkt als vertikale Linie zeigt
# und alle Marken rechts davon rot markiert.

# Daten für Plot markieren
plot_data <- df_high %>%
  mutate(
    Status = ifelse(Buzz > turning_point, "Wear-Out Zone", "Safe Zone")
  )

pdf("Wear_Out_Brands_Visualization.pdf", width = 10, height = 7)

p <- ggplot(plot_data, aes(x = Buzz, y = Popularity)) +
  
  # A. Die "normalen" Punkte (Grau)
  geom_point(data = subset(plot_data, Status == "Safe Zone"), 
             color = "grey80", alpha = 0.5, size = 1.5) +
  
  # B. Die "gefährdeten" Punkte (Rot & Größer)
  geom_point(data = subset(plot_data, Status == "Wear-Out Zone"), 
             color = "#D9534F", alpha = 0.9, size = 3) +
  
  # C. Die quadratische Kurve (Trend)
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "black", linewidth = 1, se = FALSE) +
  
  # D. Der Kipp-Punkt (Vertikale Linie)
  geom_vline(xintercept = turning_point, linetype = "dashed", color = "#D9534F") +
  annotate("text", x = turning_point + 0.02, y = min(df_high$Popularity), 
           # Hier auch 4 Nachkommastellen für die Grafikbeschriftung
           label = paste("Sättigung bei Buzz =", format(round(turning_point, 4), nsmall = 4)), 
           color = "#D9534F", angle = 90, hjust = 0, vjust = 0) +
  
  # E. Beschriftung NUR der betroffenen Marken
  geom_text_repel(data = subset(plot_data, Status == "Wear-Out Zone"),
                  aes(label = Label_Name),
                  box.padding = 0.5,
                  point.padding = 0.3,
                  force = 10,
                  size = 3,
                  color = "#B94A48") +
  
  # Styling
  labs(title = "Wear-Out Analyse: Welche Marken haben 'zu viel' Buzz?",
       # Untertitel ebenfalls mit 4 Nachkommastellen
       subtitle = paste("High Involvement Cluster | Kipp-Punkt bei Buzz >", format(round(turning_point, 4), nsmall = 4)),
       x = "Brand Buzz",
       y = "Brand Popularity") +
  theme_minimal()

print(p)
dev.off()

cat("\nInfo: Grafik 'Wear_Out_Brands_Visualization.pdf' erstellt.\n")
cat("Info: Liste 'Wear_Out_Brands_List1.1.csv' gespeichert.\n")