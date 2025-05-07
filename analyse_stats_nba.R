# étude NBA stats

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# 1. Charger les données
nba <- read.table("./nba_players_stats_merged.csv", 
                  header = TRUE,    # La première ligne contient les noms de colonnes
                  sep = "",         # Séparateur = espace
                  dec = ".",        # Point comme séparateur décimal
                  stringsAsFactors = TRUE)

############################
# Nettoyages préparatoires #
############################
######################################
# K-means pour discrétiser la taille #
######################################
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


##################################
# K-means pour discrétiser l'âge #
##################################
set.seed(123)
km <- kmeans(nba$Age, centers = 4)

# Récupère les centres triés (par ordre croissant)
ordered_centers <- order(km$centers)

# Associe à chaque cluster son rang : petit (1), moyen (2), grand (3)
cluster_to_label <- c("jeune_joueur", "joueur_mature", "joueur_experimente", "joueur_veteran")
names(cluster_to_label) <- ordered_centers

# Applique la bonne étiquette
nba$Age_cat <- cluster_to_label[as.character(km$cluster)]


# Crée un tableau disjonctif à partir de la variable Age_cat
ages <- model.matrix(~ Age_cat - 1, data = nba)

# S'assurer que Player est du type caractère
nba$Player <- as.character(nba$Player)

# Combiner correctement avec les colonnes disjonctives
nba_ages <- cbind(Player = nba$Player, nba$Age, ages)

# Afficher les premières lignes
head(nba_ages)
# Exporter le tableau disjonctif avec les noms des joueurs
write.csv(nba_ages, file = "nba_Age_ages.csv", row.names = FALSE)

nba[, c("Player", "Age", "Age_cat")]


#######################################
# Transformer poids décimal en entier #
#######################################

# Supposons que ta colonne s'appelle "MyColumn" dans un data frame nommé "df"
nba$WeightInteger <- trunc(nba$Weight)

# Exporter cette colonne uniquement dans un CSV
write.csv(nba["WeightInteger"], file = "colonne_sans_decimales.csv", row.names = FALSE)



#####################################
# K-means pour discrétiser le poids #
#####################################
set.seed(123)
km <- kmeans(nba$WeightInteger, centers = 3)

# Récupère les centres triés (par ordre croissant)
ordered_centers <- order(km$centers)

# Associe à chaque cluster son rang : petit (1), moyen (2), grand (3)
cluster_to_label <- c("light_player", "medium_player", "heavy_player")
names(cluster_to_label) <- ordered_centers

# Applique la bonne étiquette
nba$Weight_cat <- cluster_to_label[as.character(km$cluster)]

# Crée un tableau disjonctif à partir de la variable Age_cat
Weights <- model.matrix(~ Weight_cat - 1, data = nba)

# S'assurer que Player est du type caractère
nba$Player <- as.character(nba$Player)

# Combiner correctement avec les colonnes disjonctives
nba_weights <- cbind(Player = nba$Player, nba$Weight, Weights)

# Afficher les premières lignes
head(nba_weights)
# Exporter le tableau disjonctif avec les noms des joueurs
write.csv(nba_weights, file = "nba_weights.csv", row.names = FALSE)

nba[, c("Player", "Weight", "Weight_cat")]


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
# library(corrplot)


# install.packages("plotly")
# library(plotly)
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

# install.packages("factoextra")
# library(factoextra)


fviz_famd_ind(res_famd, repel = TRUE)








#################################
# "dé-discrétiser" des colonnes #
#################################
head(nba)
# Création de la variable 'blessure'
nba$blessure <- with(nba, ifelse(blessure_haut_du_corps == 1 & blessure_bas_du_corps == 1, "blessure haut et bas",
                                 ifelse(blessure_haut_du_corps == 1, "blessure haut",
                                        ifelse(blessure_bas_du_corps == 1, "blessure bas", "Non blessé"))))

# Création de la variable 'age_cat'
nba$age_cat <- apply(nba[, c("Age_catjeune_joueur", "Age_catjoueur_experimente", "Age_catjoueur_mature", "Age_catjoueur_veteran")], 1, function(x) {
  levels <- c("jeune joueur", "joueur expérimenté", "joueur mature", "joueur vétéran")
  levels[which(x == 1)]
})

# Création de la variable 'height_cat'
nba$height_cat <- apply(nba[, c("Height_catgrand", "Height_catmoyen", "Height_catpetit")], 1, function(x) {
  levels <- c("grand", "moyen", "petit")
  levels[which(x == 1)]
})

# Création de la variable 'weight_cat'
nba$weight_cat <- apply(nba[, c("Weight_catlight_player", "Weight_catmedium_player", "Weight_catheavy_player")], 1, function(x) {
  levels <- c("léger", "moyen", "lourd")
  levels[which(x == 1)]
})

# Supprimer les colonnes binaires
nba <- nba[, !grepl("^blessure_|^Age_cat|^Height_cat|^Weight_cat", names(nba))]

# Exporter le fichier CSV
write.csv(nba, "nba_dediscretise.csv", row.names = FALSE)









########################
# Etude proprement dite #
########################
##########################################
# Sur le modèle de AFDM_data_logements.R #
##########################################
# 1. Charger les données
nba <- read.table("./nba_players_stats_merged.csv", 
                  header = TRUE,    # La première ligne contient les noms de colonnes
                  sep = "",         # Séparateur = espace
                  dec = ".",        # Point comme séparateur décimal
                  stringsAsFactors = TRUE)

dim(nba)

# résumé univarié des données
head(nba)
summary(nba)

# Identification des variables selon leur type
quali <- which(sapply(nba, is.factor))
quali <- quali[names(quali) != "Player"]
# quali <- names(Filter(is.factor, nba))
# quali <- setdiff(quali, c("Player"))  # on enlève la colonne d'identification
quali

quanti <- which(sapply(nba, is.numeric))
quanti.cont <- quanti[-quanti.discr]
quanti.fouls <- which(regexpr("FOUL", colnames(nba))>0)

head(quali)
head(quanti)
head(quanti.fouls)
# Analyses univariées
### Boxplots
# install.packages("car")
library(car)
mapply(nba[,quanti.cont],
       FUN = function(xx,name){Boxplot(xx, main = name,id.n = 2, ylab = "")},
       name = names(quanti.cont))

### Diagrammes en barres
par(mfrow = c(2, 3), mar = c(3, 3, 2, 1))  # Marges plus serrées
par(mfrow = c(2, 3))

mapply(nba[,quali],
       FUN = function(xx,name){barplot(table(xx),main = name)},
       name = names(quali))

### Tableaux
# quanti continu
# install.packages("stargazer")
library(stargazer)
stargazer(nba[,quanti.cont],
          summary.stat = c("n","min","p25","median","mean","p75","max","sd"),
          type = "text")

# quali
library(questionr)
univ_quali <- lapply(nba[,quali], freq)
univ_quali


# Analyses univariées
### lien entre variables quanti
# Pearson
# install.packages("DescTools")
library(DescTools)
matcor.pears <-cor(nba[,quanti.cont])
png("nba_matrice_corr_pearson.png", width = 1000, height = 1000)
PlotCorr(matcor.pears, cex.lab = par("cex.lab"), cex = 0.55)
dev.off()

# Spearman
matcor.spear <- cor(nba[,quanti.cont],method = "spearman")
png("nba_matrice_corr_spearman.png", width = 1000, height = 1000)
PlotCorr(matcor.spear, cex.lab = par("cex.lab"), cex = 0.55)
dev.off()


####lien entre variables quanti et quali
# creation d'une matrice vide avec en ligne les variables quantitatives et en colonne les variables qualitatives
mateta2 <- matrix(NA,length(quali),length(quanti))
rownames(mateta2) <- names(quali)
colnames(mateta2) <- names(quanti)
mateta2


library(BioStatR)

# calcul des différents eta carré
for(ii in seq(nrow(mateta2))){
  for(jj in seq(ncol(mateta2))){
    mateta2[ii, jj]<-eta2(nba[, colnames(mateta2)[jj]],
                          nba[, rownames(mateta2)[ii]])
  }
}

# affichage
png("nba_matrice_eta2.png", width = 1000, height = 1000)
PlotCorr(mateta2,
         border = NA,
         cols = colorRampPalette(c("white", "steelblue"), space = "rgb")(20),
         breaks = seq(0, 1, length=21),
         cex.lab = par("cex.lab"), cex = 0.55*par("cex"),
         args.colorlegend = list(labels = sprintf("%.1f", seq(0, 1, length = 11)), frame = TRUE))
dev.off()

#identification des variables les plus liées aux variables de fautes, par ex FOUL_CHARGE
res.eta2<-sort(mateta2[,"FOUL_CHARGE"])

#représentation
png("nba_barplot.png", width = 1000, height = 1000)
barplot(res.eta2,horiz = TRUE,
        las = 2,
        xlab = expression(eta^2),
        main = "Foul_charge",
        cex.axis = 1.5,   # Taille de police des étiquettes (valeur par défaut = 1)
        cex.names = 0.9)
dev.off()


# pour toutes les variables de fautes
png("nba_barplot_fouls.png", width = 1000, height = 1000)
mapply(as.data.frame(mateta2[,colnames(nba)[quanti.fouls]]),
       FUN=function(xx,name){
         names(xx) = rownames(mateta2)
         res.eta2 <- sort(xx)
         barplot(res.eta2,
                 horiz = TRUE,
                 las = 2,
                 xlab = expression(eta^2),
                 main = name,
                 xlim = c(0,1))
       },
       name = colnames(nba)[quanti.fouls])
dev.off()
