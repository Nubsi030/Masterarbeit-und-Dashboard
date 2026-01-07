library(dplyr)
library(stringr)

# 1. Definieren der Begriffe, nach denen wir suchen (Case Insensitive)
# Wir suchen nach Teilen von Wörtern, um alle Varianten zu erwischen:
# "Jewelry" -> fängt Jewelry market & Jewelry retailer
# "Schmuck" -> fängt deutsche Bezeichnungen
# "Möbel", "Furniture" -> fängt Möbelhäuser & Online-Shops
# "Car", "Motorcycle", "Auto" -> fängt Fahrzeuge
# "Luxus", "Luxury", "Premium Fashion" -> fängt Luxusmode

search_pattern <- regex("Jewelry|Schmuck|Möbel|Furniture|Car|Motorcycle|Luxus|Luxury|Premium Fashion", ignore_case = TRUE)

# 2. Update des Datensatzes
KPIs_alle1864 <- KPIs_alle1864 %>%
  mutate(
    # Wir erstellen eine temporäre Bedingung: Trifft eines der Schlagworte zu?
    needs_upgrade = str_detect(Industry, search_pattern),
    
    # Update des Text-Clusters
    involvement_cluster = case_when(
      needs_upgrade ~ "High-Involvement",  # Wenn Treffer -> High
      TRUE ~ involvement_cluster           # Sonst -> alten Wert behalten
    ),
    
    # Update der Zahl (Konsistenz wahren)
    Involvement = case_when(
      needs_upgrade ~ 1,                   # Wenn Treffer -> 1
      TRUE ~ Involvement                   # Sonst -> alten Wert behalten
    )
  ) %>%
  select(-needs_upgrade) # Hilfsspalte wieder entfernen

# --- Überprüfung ---
# Wir schauen uns exemplarisch Marken an, die betroffen sein müssten
check_results <- KPIs_alle1864 %>%
  filter(str_detect(Industry, "Jewelry|Möbel|Car|Luxus")) %>%
  select(Name, Industry, involvement_cluster) %>%
  head(15) # Zeigt die ersten 15 geänderten Einträge

print(check_results)


library(dplyr)

# Wir überschreiben den Datensatz mit der bereinigten Version
# .keep_all = TRUE sorgt dafür, dass alle anderen Spalten (Werte, Industry, etc.) erhalten bleiben
KPIs_alle1864 <- KPIs_alle1864 %>%
  distinct(Name, .keep_all = TRUE)

# --- Überprüfung ---
# 1. Gesamtzahl der Zeilen checken (sollte jetzt deutlich kleiner sein als 1864, wenn viele Duplikate da waren)
print(paste("Neue Anzahl Marken:", nrow(KPIs_alle1864)))

# 2. Speziell prüfen, ob Parship jetzt nur noch einmal da ist
check_parship <- KPIs_alle1864 %>% 
  filter(grepl("Parship", Name)) %>%
  select(Name, Industry, Involvement)

print(check_parship)




