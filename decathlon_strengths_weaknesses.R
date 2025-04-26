decathlon<-read.table("/home/destremonma/Master_Cnam/STA101-Analyse des données méthodes descriptives/R/decathlon/decathlon.txt", 
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

# Pour tester si une association entre deux variables quantitatives est significative 
# (et donc qu’il existe un lien entre les deux variables) on commence par vérifier 
# que l’on peut faire une hypothèse de normalité sur les variables. 
# Pour cela, on peut visualiser les histogrammes, les diagrammes quantile-quantile ou encore effectuer un test de shapiro-wilk. 
# On prend comme exemple le couple de variables 100m et 400m.
#normalité

#histogrammes
histogram(decathlon$X100m)
histogram(decathlon$X400m)

#diagramme qqplot
qqmath(decathlon$X100m)
qqmath(decathlon$X400m)

#test de shapiro
shapiro.test(decathlon$X100m)
shapiro.test(decathlon$X400m)

# La normalité des deux variables étant raisonnable au vu du nombre d’observations, on peut envisager un test de corrélation.
## test de correlation
cor.test(decathlon$X100m,decathlon$X400m)

# => il y a une corrélation positive significative entre les performances au 100m et au 400m.
# Les athlètes qui courent vite le 100m tendent aussi à bien performer sur le 400m (même si ce n’est pas une règle stricte).
# La corrélation de 0.52 est modérée, donc les deux épreuves sont liées mais pas parfaitement.
# On rejette clairement l’hypothèse selon laquelle il n’y aurait aucun lien (ρ = 0) entre les deux épreuves (p-value ≈ 0.0005)

# Variables quantitatives et qualitatives
# On représente la distribution des variables quantitatives en fonction de la variable qualitative 
# et on calcule le η2 entre ces mêmes variables. 
# Par la suite, on effectue un test pour déterminer le caractère significatif de la liaison.

# Pour la variables Disque par exemple

## via des histogrammes
histogram( ~ Disque | Epreuve,
           data = decathlon,
           type = "density",
           col = "lightgreen",
           ylab = "",
           nint  =k)

## via des boîtes à moustaches
Boxplot(Disque~Epreuve, data = decathlon)

# On ne distingue pas vraiment de différence de distribution de la variable quantitative (lancer du disque) 
# selon les modalités de la variable qualitative (JO et Decastar). 
# ces graphiques sont construits sur peu d’observations, il est donc attendu 
# d’observer de petites variations d’une compétition à l’autre.

# la variance de la variable quantitative semble similaire dans les deux groupes.
# calcul du eta2 entre ces deux variable

## Chargement du package BioStatR
install.packages("BioStatR")
library(BioStatR)

## calcul du eta2 entre Disque et Epreuve
eta2(decathlon$Disque,decathlon$Epreuve)

## test de l'association
anova(lm(decathlon$Disque~decathlon$Epreuve))

# Le coefficient η2 est très proche de zéro et le test de significativité 
# ne permet pas de mettre en évidence un lien (p.value > 5%).

