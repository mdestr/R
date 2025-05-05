# étude NBA stats

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# 1. Charger les données
nba <- read.table("./nba_players_stats_merged.csv", 
                  header = TRUE,    # La première ligne contient les noms de colonnes
                  sep = "",         # Séparateur = espace
                  dec = ".",        # Point comme séparateur décimal
                  stringsAsFactors = TRUE)



# K-means pour discrétiser la taille
set.seed(123)
km <- kmeans(nba$Height, centers = 3)

# Récupère les centres triés (par ordre croissant)
ordered_centers <- order(km$centers)

# Associe à chaque cluster son rang : petit (1), moyen (2), grand (3)
cluster_to_label <- c("petit", "moyen", "grand")
names(cluster_to_label) <- ordered_centers

# Applique la bonne étiquette
nba$Height_cat <- cluster_to_label[as.character(km$cluster)]


# Crée un tableau disjonctif à partir de la variable Height_cat
dummies <- model.matrix(~ Height_cat - 1, data = nba)

# S'assurer que Player est du type caractère
nba$Player <- as.character(nba$Player)

# Combiner correctement avec les colonnes disjonctives
nba_dummies <- cbind(Player = nba$Player, nba$Height, dummies)

# Afficher les premières lignes
head(nba_dummies)
# Exporter le tableau disjonctif avec les noms des joueurs
write.csv(nba_dummies, file = "nba_height_dummies.csv", row.names = FALSE)



nba[, c("Player", "Height", "Height_cat")]





head(nba)
summary(nba)

install.packages("psych")
library(psych)

describe(nba)

colSums(is.na(nba))


# Compter modalités variables qualitatives :
lapply(nba[, sapply(nba, is.factor)], table)

# Chercher les valeurs aberrantes sur  les variables quantitatives
vars_quanti <- sapply(nba, is.numeric)
nba_quanti <- nba[, vars_quanti]

boxplot(nba_quanti, outline = TRUE, las = 2, main = "Boxplots des variables quantitatives")

boxplot(nba_quanti[, 1:10], las = 2, main = "Boxplot - variables 1 à 10")
boxplot(nba_quanti[, 11:20], las = 2, main = "Boxplot - variables 1 à 10")
boxplot(nba_quanti[, 21:30], las = 2, main = "Boxplot - variables 1 à 10")
boxplot(nba_quanti[, 31:40], las = 2, main = "Boxplot - variables 1 à 10")
boxplot(nba_quanti[, 41:50], las = 2, main = "Boxplot - variables 1 à 10")
boxplot(nba_quanti[, 51:60], las = 2, main = "Boxplot - variables 1 à 10")
boxplot(nba_quanti[, 61:70], las = 2, main = "Boxplot - variables 1 à 10")

boxplot(nba_quanti[, 2], outline = TRUE, las = 2, main = "Boxplot")

library(ggplot2)
i=24
ggplot(nba_quanti, aes(y = .data[[colnames(nba_quanti)[i]]])) +
  geom_boxplot() +
  labs(title = "Boxplot", 
       y = colnames(nba_quanti)[i]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# nombre de valeurs par variable qui s’éloignent de plus de 3 écarts-types de la moyenne
z_scores <- scale(nba_quanti)
outliers <- abs(z_scores) > 3
summary(outliers)



# Matrice de corrélation
cor_matrix <- cor(nba_quanti, use = "pairwise.complete.obs")

# install.packages("corrplot")
library(corrplot)


# install.packages("plotly")
library(plotly)
plot_ly(x = colnames(cor_matrix),
        y = rownames(cor_matrix),
        z = cor_matrix, 
        type = "heatmap",
        colorscale = "RdBu")

str(nba) 
names(nba)[sapply(nba, is.numeric)]
cor.test(nba$Height, nba$BLK, method = "pearson")

anova_result <- aov(nba$W ~ nba$COLLEGE)
summary(anova_result)


boxplot(nba$W ~ nba$COLLEGE)


############################





library(FactoMineR)
# FAMD basique
res_famd <- FAMD(nba, graph = TRUE)

install.packages("factoextra")
library(factoextra)


fviz_famd_ind(res_famd, repel = TRUE)

