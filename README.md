Es wurde mit R version 4.2.0 gearbeitet. 
Folgende Pakete werden benutzt:

-caret    6.0-92 \\
-cowplot   1.1.1
-dplyr  1.0.9
-forcats  0.5.1
-gdata  2.18.0.1
-ggplot2  3.3.6
-grid   4.2.0
-gridExtra  2.3
-Metrics    0.1.4
-openxlsx   4.2.5
-patchwork  1.1.1
-radiant.data   1.4.4
-ranger   0.13.1
-RColorBrewer   1.1-3
-splitstackshape  1.4.8
-splitTools   0.3.2
-stringr  1.4.0
-tidyr   1.2.0

Alle Datensätze sind im Ordner "Data" zu finden. Der Ordner "Plots" enthält alle 
Plots und Tabellen.

"get_data.R" erzeugt den Datensatz "wave2_clean" und "wave3_clean" welcher
die Grundlage aller Analysen bildet (nur nötige Variablen, ohne Nichtwähler bzw. Sonstige,
nur Kombinationen mit 20 Ausprägungen, usw.). Diese Datei muss nicht ausgeführt
werden, alle weiteren Skripte greifen direkt auf "wave2_clean" und "wave3_clean" zu.

"describe_data.R" erstellt einige deskriptive Plots z.B. zu den fehlenden Werten,
der Vergleich des gesamten Datensatzen und der Stichprobe in der 2. Welle, usw.

"modelling.R" stellt die verschiedenen Modelle auf. Zum Einen das Random Forest Modell 
und zum Anderen die drei verschiedenen Dempster Bounds. 

"weights.R" führt die Simulation der Gewichtungsvariable durch, die für die Performance
Beurteilund des Random Forests benötigt wird.

Datensätze:
-"wave2_clean": Datensatz der 2. Welle nach cleaning, Grundlage aller Analysen
-"wave3_clean": Datensatz der 3. Welle nach cleaning, Grundlage aller Analysen

