# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

py_install("basketball-reference-scraper", pip = TRUE)

library(reticulate)

# 1. Charger et exécuter tout le script Python
source_python("./scrapnbaStats.py")  
# Tous les objets Python sont automatiquement convertis en objets R
