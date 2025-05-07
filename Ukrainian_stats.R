install.packages("haven")
library(haven)

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Lire le fichier .sav
data <- read_sav("../Projet/MICS_Datasets/Ukraine2012_MICS_Datasets/Ukraine_MICS4_Datasets/Ukraine_MICS_2012_SPSS_Datasets/wm.sav")

# Afficher les premières lignes
head(data)
