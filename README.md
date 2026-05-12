# Auswirkungen von Brand Buzz auf zentrale Marketing-KPIs

Kurzbeschreibung
----------------
Dieses Repository dokumentiert eine empirische Untersuchung, wie Brand Buzz zentrale Marketingkennzahlen (KPIs) beeinflusst. Analysen wurden hauptsächlich in R durchgeführt; das Repo enthält Skripte, Datensätze, Ergebnis-CSV-Dateien und ein interaktives Dashboard (index.html).

Inhaltsverzeichnis
------------------
- Status & Ziel
- Ordnerstruktur & Wichtige Dateien
- Kurze Skriptbeschreibungen

Status & Ziel
-------------
- Status: Analysen abgeschlossen; Dashboard (index.html) und Ergebnisdateien vorhanden.  
- Ziel: Untersuchung der Effekte von Brand Buzz auf mehrere KPIs (u. a. mittels Beta-Regression, Moderationstests, Reliabilitätsprüfungen und Wear‑Out-Analysen).

Ordnerstruktur & Wichtige Dateien 
------------------------------------------
- R-Skripte (Analyse & Cleaning)
  - beta_high.2.R
  - beta_super_low.R
  - HYPOTHESE_5_COMPLETE SUITE_(TEST, RELIABILITY & VIZ)_UPDATE v5.R
  - HYPOTHESE 6_a_e_ MODERATION DURCH INVOLVEMENT_Z_TEST mit BETA-REGRESSION.R
  - BETA_high_alle.R
  - Hypothese_7_WearOut.R / Wear_Out_Identification.R
  - BRAND_PERFORMANCE_ATLAS.R
  - Clean 13.R
  - DJL 2.R, DJL 3.R
  - Deskreptive Statistilekn.R
- Dashboard / HTML
  - index.html
- Datensätze / Ergebnisse
  - KPI_dataframe_1807.xlsx
  - Results_*.csv (verschiedene Hypothesen-Ergebnisse)
  - Wear_Out_Brands_List.csv
- Visualisierungen
  - BRAND_PERFORMANCE_ATLAS.pdf

Kurze Beschreibungen der Skripte 
------------------------------------------

- Clean 13.R  
  Zweck: Datenaufbereitung — Einlesen Rohdaten, Typkorrekturen, Umgang mit NAs, Erstellung analysis-ready Dataframes.  
  Eingabe: Roh-Excel/CSV; Ausgabe: cleaned_data (.csv/.rds). Tipp: Als erstes Script ausführen.

- Deskreptive Statistilekn.R  
  Zweck: Deskriptive Statistiken und Korrelations- / Heatmap-Übersichten für KPIs.  
  Ausgabe: Tabellen (N, Mittel, SD) und Heatmaps.

- DJL 2.R / DJL 3.R (Double Jeopardy Law)  
  Zweck: Prüfung des Double Jeopardy-Phänomens (Penetration vs. Loyalty) — Zusammenfassungen und Visualisierungen.  
  Ausgabe: Plots und Tabellen mit Regressions-/Korrelationsbefunden.

- beta_high.2.R  
  Zweck: Beta-Regressionen für KPIs im High-Involvement-Cluster.  
  Eingabe: bereinigte KPIs (0,1-Skala), Buzz-Variablen, Kontrollen.  
  Ausgabe: Modelltabellen (CSV) und Plots. Achtung: Umgang mit 0/1-Werten erforderlich.

- beta_super_low.R  
  Zweck: Entsprechende Beta-Regressionen für Low-Involvement-Cluster.  
  Hinweise: Gleiche Struktur wie beta_high.2.R, getrennte Filterung nach Involvement.

- BETA_high_alle.R  
  Zweck: Batch-Ausführung mehrerer Beta-Modelle (H2–H4) für High-Involvement; sammelt Ergebnisse in Summary-CSV.  
  Gut für Reproduzierbarkeit (Modellformeln oben konfigurieren).

- HYPOTHESE_5_COMPLETE SUITE_(TEST, RELIABILITY & VIZ)_UPDATE v5.R  
  Zweck: Aggregation mehrerer KPIs zu einer Composite, Prüfung der Reliabilität (Cronbach’s Alpha) und Test von H5.  
  Ausgabe: Alpha-Werte, aggregierte Kennzahl, Modell- und Visualisierungsergebnisse.

- HYPOTHESE 6_a_e_ MODERATION DURCH INVOLVEMENT_Z_TEST mit BETA-REGRESSION.R  
  Zweck: Tests der Moderation durch Involvement (Interaktion Buzz × Involvement) mittels Beta-Regressionen; simple-slopes / Marginaleffekte ausgeben.  
  Hinweis: Involvement vorher (z-)skalieren oder kategorial splitten.

- Hypothese_7_WearOut.R / Wear_Out_Identification.R  
  Zweck: Identifikation von Sättigungs- / Wear-Out-Punkten (z. B. über quadratische Terme, Splines, piecewise oder GAMs).  
  Ausgabe: Schätzung von Kipp-Punkten und Liste von Marken jenseits des Punkts (Wear_Out_Brands_List.csv).

- BRAND_PERFORMANCE_ATLAS.R  
  Zweck: Erstellung atlasartiger Visualisierungen zur Identifikation von Ausreißern und Performance‑Landscapes (Export als PDF/Plots).  
  Ausgabe: BRAND_PERFORMANCE_ATLAS.pdf, annotierte Plots.







