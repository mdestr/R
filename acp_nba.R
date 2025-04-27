# acp NBA stats

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))



# 1. Charger les données avec les bons paramètres
nba <- read.table("./nbastats.csv", 
                        header = TRUE,    # La première ligne contient les noms de colonnes
                        sep = "",         # Séparateur = espace
                        dec = ".",        # Point comme séparateur décimal
                        stringsAsFactors = FALSE)


head(nba)

summary(nba)


# Install ggpubr pour les boxplots
install.packages("ggpubr")


library(ggpubr)
# Graphical display of distributions
ggboxplot(nba, y = "PpG.Points", width = 0.5)

# QQ plots is used to check whether the data is normally distributed. 
ggqqplot(nba, x = "PpG.Points")

install.packages("dplyr")

# Descriptive statistics by positions
# C = Center, F = Forward, G = Guard
library(dplyr)
group_by(nba, POS) %>% 
  summarise(
    count = n(), 
    mean = mean(PpG.Points, na.rm = TRUE),
    sd = sd(PpG.Points, na.rm = TRUE)
  ) %>% 
  print(n = 200)



# Graphics for grouped data:
library("ggpubr")
# Box plot colored by groups: Species
ggboxplot(nba, x = "POS", y = "PpG.Points",
          color = "POS",
          palette = c("#00AFBB", "#E7B800", "#FC4E07", 'lightsalmon', 'mediumpurple4', 'darkorange', 'deeppink1'))

# Afficher les couleurs si besoin
# colors()

install.packages("ragg")
install.packages("devtools")

library("devtools")
devtools::install_github("abresler/nbastatR")

# https://rpubs.com/thecodingone/nbastatRTutorial
# Install and Load the nbastatR package
library('nbastatR')
Sys.setenv("VROOM_CONNECTION_SIZE" = 5000000)


# Access the game tables for a particular season
gamedata <- game_logs(seasons = 2025)

# Take a quick look at the data
head(gamedata)

install.packages("hoopR")

library(hoopR)


nba_pbp <- hoopR::load_nba_pbp(2024:hoopR::most_recent_nba_season())
head(nba_pbp)



# install.packages("FactoMineR")
library(FactoMineR)

# on effectue l'ACP (réduite et avec 5 dimensions par défaut)
res.pca <- PCA(nba,
               quanti.sup = c(4, 29),
               quali.sup = c(1, 2, 3),
               graph = FALSE)

# export des résultats dans un fichier
write.infile(res.pca, file = "./resultats_ACP.csv")

#valeurs propres
res.pca$eig

barplot(res.pca$eig[,1], las = 2)

# On analyse d’abord le plan principal (appelé aussi plan 1-2).
#graphes des individus
plot.PCA(res.pca, choix = "ind")

#graphes des variables
plot.PCA(res.pca, choix = "var")
