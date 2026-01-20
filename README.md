# Masterarbeit-und-Dashboard
Dieses Repository dokumentiert eine empirische Masterarbeit, die die Auswirkungen von Brand Buzz auf zentrale Marketingkennzahlen untersucht. Das Projekt analysiert einen umfassenden Datensatz von 1.804 Marken across 100 Branchensektoren mithilfe statistischer Verfahren und Datenvisualisierung. Der Code kombiniert R-basierte quantitative Analysen—einschließlich Korrelationsanalysen, Regressionsmodellen und Effektgrößenberechnungen—mit interaktiven HTML-Dashboards zur Ergebnispräsentation. Die Arbeit wird von rigoroser methodischer Literaturrecherche unterstützt und adressiert die zentrale Forschungsfrage, wie Brand Buzz auf Markenwahrnehmung, Popularität, Nutzung und Loyalität wirkt. Bei der Konzeption und Implementierung dieses Projekts wurde Google Gemini als unterstützendes Werkzeug zur Code-Optimierung, methodologischen Beratung und Dokumentation eingesetzt.
Inhalte

    R-Skripte: Vollständige statistische Analysepipeline mit Datenverarbeitung, Hypothesentests und Modellierung
    - beta_high.2.R (Dieser Code analysiert mittels Beta-Regression den Brancheneffekt von Buzz auf vier KPIs speziell für Produkte mit hohem Involvement (Engagement).)
    - beta_super_low.R (Dieser Code analysiert mittels Beta-Regression den Brancheneffekt von Buzz auf vier KPIs speziell für Produkte mit geringem Involvement (Engagement).)
    - HYPOTHESE_5_COMPLETE SUITE_(TEST, RELIABILITY & VIZ)_UPDATE v5.R (Dieser Code prüft H5, indem er vier KPIs aggregiert, deren Reliabilität mittels Cronbachs Alpha testet und Buzz-Effekte per Beta-Regression visualisiert)
    -
    -HYPOTHESE 6_a_e_ MODERATION DURCH INVOLVEMENT_Z_TEST mit BETA-REGRESSION.R (enthält R-Code zum Testen der Hypothesen 6a bis 6e)
    -BETA_high_alle.R (enthält R-Code zum Testen der Hypothesen 2-4 für das high invlvementcluster)
    - Hypothese_7_WearOut.R / Wear_Out_Identification.R (Dieser Code berechnet den Sättigungspunkt für High-Involvement-Marken, identifiziert jene jenseits des Kipp-Punkts und visualisiert diese gefährdeten Marken in einer Danger-Zone.)
    -
    -BRAND_PERFORMANCE_ATLAS.R (enthält R-code für das Erstellen der Visaliserungen der Ausreißer)
    -Clean 13.R (enthält R-code zum Data-cleaning)
    -DJL 2.R und DJL 3.R Double Jeopardy law.R (Dieser Code prüft statistisch und grafisch, ob Marken mit hoher Nutzung gleichzeitig höhere Kundentreue aufweisen.)
    -Deskreptive Statistilekn.R (Der Code berechnet Statistiken und Korrelationen für fünf KPIs, vergleicht Gruppen mit unterschiedlichem Involvement und visualisiert die Gesamtergebnisse als Heatmap.)
    
    HTML-Visualisierungen: Interaktive Reports und Dashboards zur Darstellung von Analyseergebnissen
    - index.html (Code zum Dashboard)

    Datensets: Aufbereitete und verarbeitete Marken- und KPI-Daten
    - KPI_dataframe_1807.xlsx (ist der Datensatz der zur Überfrpung der Modelle verwendete wurde)
    - Results_High_Involvement_MultiKPI.csv (gibt die ergebnisse der Ersten 4 hypothesen für high involvement Cluster an) 
    -Results_Low_Involvement_MultiKPI.csv / Results_Low_Involvement_MultiKPI_2.csv (gibt die ergebnisse der Ersten 4 hypothesen für low involvement Cluster an) 
    - Ergebnisse_H5_Performance.csv (Die Tabelle zeigt die Ergebsnisse von H5)
    - H6_Moderation_Results.csv (Die Tabelle zeigt für H6 Moderationseffekte)
    - Wear_Out_Brands_List.csv (gibt die ergebnisse von H7 an)

    Visualisierungen
    - BRAND_PERFORMANCE_ATLAS.pdf (Visaliserungen der Ausreißer)
    

    
