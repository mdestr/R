decathlon<-read.table("/home/destremonma/Master_Cnam/STA101 - Analyse des données méthodes descriptives/R/decathlon/decathlon.txt", 
                      stringsAsFactors = TRUE)

summary(decathlon)

# chargement de la librairie
library(stargazer)

# affichage de quelques indicateurs statistiques pour variables quantitatives
stargazer(decathlon,
          keep = c("X100m", "Longueur", "Poids", "Hauteur", "X400m", "X110m.haies", "Disque", "Perche", "Javelot", "X1500m", "Nb.pts"),
          summary.stat = c("n","min","p25","median","mean","p75","max","sd"),
          type = "text")

# centrage-réduction pour distinguer des valeurs atypiques et également pour situer les valeurs les unes par rapport aux autres. 
decathlon.scale <- scale(decathlon[,c("X100m", "Longueur", "Poids", "Hauteur", "X400m", "X110m.haies", 
                                      "Disque", "Perche", "Javelot", "X1500m", "Nb.pts")])
head(round(decathlon.scale, 2))

library(car)

# Boîte à moutache pour la variable Disque
Boxplot( ~ Disque, data = decathlon, main = "Boîte à moustaches pour la variable Disque")

# Histogramme pour la variable Disque

library(lattice)
## choix du nombre de classes par la regle de Sturges
n <- nrow(decathlon)#nombre d'individus
k <- ceiling(1 + log(n)/log(2))#nombre de classes
## construction de l'histogramme
histogram(decathlon$Disque, nint = k, type = "density")

#chargement de la librairie questionr qui contient la fonction freq. 
library(questionr)

#calcul des effectifs
frequence <- freq(decathlon$Epreuve)
frequence

#tracé du diagramme en barres
barplot(frequence$`%`,
        ylab = "fréquence relative",
        names.arg = rownames(frequence))

# scatterplot
pairs(decathlon[,c("X100m", "Longueur", "Poids", "Hauteur", "X400m", "X110m.haies", 
                   "Disque", "Perche", "Javelot", "X1500m", "Classement","Nb.pts")],cex=.4)

# matrice de corrélations
mat.cor <- cor(decathlon[,c("X100m", "Longueur", "Poids", "Hauteur", "X400m", "X110m.haies", "Disque", "Perche", "Javelot", "X1500m", "Classement","Nb.pts")])
print(round(mat.cor, 2))

# matrice des correlations de Spearman
mat.spearman <- cor(decathlon[,c("X100m", "Longueur", "Poids", "Hauteur", "X400m", "X110m.haies", "Disque", "Perche", "Javelot", "X1500m", "Classement","Nb.pts")],
                    method = "spearman")
print(round(mat.spearman, 2))
