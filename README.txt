Es wurde mit R version 4.2.0 gearbeitet. 
Folgende Pakete werden benutzt:
-caret    6.0-92 
-cowplot   1.1.1
-dplyr  1.0.9
-forcats  0.5.1
-gdata  2.18.0.1
-ggplot2  3.3.6
-ggpubr   0.4.0
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

Der Ordner "Plots" enthält alle erstellten Plots.

"get_data.R" erzeugt die Datensätz "wave2_clean" und "wave3_clean" welcher
die Grundlage aller Analysen bildet (nur nötige Variablen, ohne Nichtwähler bzw. Sonstige,
nur Kombinationen mit 20 Ausprägungen, usw.). Außerdem wird für den Vergleich des Gesamtdatensatzes und der Stichprobe
der Datensatz "data_all_clean" erstellt. "wave2_missings" wird für den in "decdribe_data.R" erstellten missings-plot gebraucht.
Diese Datei muss nicht ausgeführt werden, alle weiteren Skripte greifen direkt auf "wave2_clean", "wave3_clean", 
"data_all_clean" und "wave2_missings" zu.

"describe_data.R" erstellt einige deskriptive Plots anhand der Datei "functions_modeling.R" 
z.B. zu den fehlenden Werten, der Vergleich des gesamten Datensatzen und der Stichprobe in der 2. Welle, usw. 

"functions_modeling.R" beschreibt alle implementierten Funktionen, die in der Datei "describe_data.R" und "modelling.R"
verwendet werden. 

"modeling.R" stellt die verschiedenen Modelle anhand der Funktionen aus "functions_modeling.R" auf. 
Zum Einen das Random Forest Modell und zum Anderen die drei verschiedenen Dempster Bounds. Außerdem werden 
die Plots zur Darstellung der Ergebnisse erstellt.

Datensätze:
-"wave2_clean": Datensatz der 2. Welle nach cleaning, Grundlage aller Analysen
-"wave3_clean": Datensatz der 3. Welle nach cleaning, Grundlage aller Analysen
-"wave2_missings.csv": Datensatz für die Erstellung der Missing-plots
-"data_all_clean": Datensatz, um den Vergleich zwischen dem gesamten Datensatz und der Stichprobe heranzuziehen

Die Datensätze sind nicht öffentlich zugänglich. Für weitere Fragen: Maren.Kellerer@gmail.com.

