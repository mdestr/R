# étude NBA stats

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# 1. Charger les données
nba <- read.table("./nba_players_stats_merged.csv", 
                  header = TRUE,    # La première ligne contient les noms de colonnes
                  sep = "",         # Séparateur = espace
                  dec = ".",        # Point comme séparateur décimal
                  stringsAsFactors = FALSE)

head(nba)
summary(nba)

install.packages("psych")
library(psych)

describe(nba)
