# Mise en oeuvre AFDM (Analyse Factorielle des données mixtes)

# Objectifs : 
#   - identifier les ressemblances et différences entre logements du point de vue des habitudes des ménages
#   - dresser un bilan des liaisons entre les variables décrivant ces habitudes
#   - associer à des groupes de logements qui se ressemblent ou s’opposent 
#     du point de vue des habitudes des ménages un profil d’habitudes particulier
#   - identifier le lien entre les habitudes des ménages et la présence de polluant.


# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


data_hab_type <- read.csv("data_hab_type_TD.csv",
                          header = TRUE,
                          stringsAsFactors = TRUE)

# 516 logements décrits par 57 variables (71 en comptant les duplications des variables de pollution)
dim(data_hab_type)

# résumé univarié des données
head(data_hab_type)
summary(data_hab_type)
# pas de n/a, pas de variable constante, import des données correct (types bien spécifiés)

# pour faciliter l'interprétation, modification des noms des modalités
levels(data_hab_type$VoitureDansGarage) <- paste0("garage_", levels(data_hab_type$VoitureDansGarage))
levels(data_hab_type$NettASecVetements) <- paste0("NetSecVet_", levels(data_hab_type$NettASecVetements))

# on réordonne les modalités afin pour avoir un ordre croissant
levels(data_hab_type$SortirOrduresExterieur) <- paste0("OrdureSortie_",c("3","2","1"))

# Identifier les modalités rares afin de ne pas polluer les futures analyses. 
# => modalités 2 et 3 de la variable garage préalablement fusionnées.
table(data_hab_type$VoitureDansGarage)

# Identification des variables selon leur type
quali <- which(sapply(data_hab_type, is.factor))
quali
quanti <- which(sapply(data_hab_type, is.numeric))
quanti.discr <- which(sapply(data_hab_type,
                             FUN = function(xx){
                               (is.numeric(xx))&(length(table(xx)))<10
                             })
)
quanti.cont <- quanti[-quanti.discr]
quanti.polluant <- which(regexpr("outcome", colnames(data_hab_type))>0)

# Analyses univariées
### Boxplots
# install.packages("car")
library(car)
mapply(data_hab_type[,quanti.cont],
       FUN = function(xx,name){Boxplot(xx, main = name,id.n = 2, ylab = "")},
       name = names(quanti.cont))

### Diagrammes en barres
par(mfrow = c(2, 3), mar = c(3, 3, 2, 1))  # Marges plus serrées
par(mfrow = c(2, 3))
mapply(data_hab_type[,quanti.discr],
       FUN = function(xx,name){barplot(table(xx),main = name)},
       name = names(quanti.discr))

mapply(data_hab_type[,quali],
       FUN = function(xx,name){barplot(table(xx),main = name)},
       name = names(quali))

### Tableaux
# quanti continu
# install.packages("stargazer")
library(stargazer)
stargazer(data_hab_type[,quanti.cont],
          summary.stat = c("n","min","p25","median","mean","p75","max","sd"),
          type = "text")

# quanti discret
# install.packages("questionr")
library(questionr)
univ_quanti.discr <- lapply(data_hab_type[,quanti.discr], freq)
univ_quanti.discr

# quali
univ_quali <- lapply(data_hab_type[,quali], freq)
univ_quali


# Analyses univariées
### lien entre variables quanti
# Pearson
# install.packages("DescTools")
library(DescTools)
matcor.pears <-cor(data_hab_type[,quanti.cont])
png("matrice_corr_pearson.png", width = 1000, height = 1000)
PlotCorr(matcor.pears, cex.lab = par("cex.lab"), cex = 0.55)
dev.off()

# Spearman
matcor.spear <- cor(data_hab_type[,quanti.cont],method = "spearman")
png("matrice_corr_spearman.png", width = 1000, height = 1000)
PlotCorr(matcor.spear, cex.lab = par("cex.lab"), cex = 0.55)
dev.off()



# lien entre variables quali et discrètes
matcram <- PairApply(data_hab_type[,c(quali,quanti.discr)], CramerV, symmetric = TRUE)
png("matrice_corr_cramer.png", width = 1000, height = 1000)
PlotCorr(matcram,
         cols = colorRampPalette(c("white", "steelblue"), space = "rgb")(20),
         breaks = seq(0, 1, length = 21),
         cex.lab = par("cex.lab"), cex = 0.55,
         args.colorlegend = list(labels=sprintf("%.1f", seq(0, 1, length = 11)), frame=TRUE))
dev.off()


####lien entre variables quanti et quali
# creation d'une matrice vide avec en ligne les variables quantitatives et en colonne les variables qualitatives
mateta2 <- matrix(NA,length(quali),length(quanti))
rownames(mateta2) <- names(quali)
colnames(mateta2) <- names(quanti)

# install.packages("BioStatR")
library(BioStatR)

# calcul des différents eta carré
for(ii in seq(nrow(mateta2))){
  for(jj in seq(ncol(mateta2))){
    mateta2[ii, jj]<-eta2(data_hab_type[, colnames(mateta2)[jj]],
                          data_hab_type[, rownames(mateta2)[ii]])
  }
}

# affichage
png("matrice_eta2.png", width = 1000, height = 1000)
PlotCorr(mateta2,
         border = NA,
         cols = colorRampPalette(c("white", "steelblue"), space = "rgb")(20),
         breaks = seq(0, 1, length=21),
         cex.lab = par("cex.lab"), cex = 0.55*par("cex"),
         args.colorlegend = list(labels = sprintf("%.1f", seq(0, 1, length = 11)), frame = TRUE))
dev.off()

#identification des variables les plus liées aux variables de pollution, par ex Formaldéhyde
res.eta2<-sort(mateta2[,"outcome.Formldehyde"])

mateta2

#représentation
barplot(res.eta2,horiz = TRUE,
        las = 2,
        xlab = expression(eta^2),
        main = "Formaldéhyde",
        cex.names  =.35)

# La variable relative à la concentration du logement en Formaldéhyde 
# semble être principalement liée avec les variables relatives à la présence 
# d’une voiture dans le garage, au Jardinage, à la présence 
# de protèges matelas et animaux domestiques (chats ou chiens).

# pour toutes les variables de pollution
mapply(as.data.frame(mateta2[,colnames(data_hab_type)[quanti.polluant]]),
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
       name = colnames(data_hab_type)[quanti.polluant])



# Données mixtes => classification par approche tandem, i.e. en classifiant les individus 
# à partir de leurs coordonnées sur les axes de l’AFDM. 
# => CAH suivie d’une consolidation
# Critère d’agrégation = critère de Ward 
#   (opère à chaque étape le regroupement de classes qui minimise l’augmentation de l’inertie intraclasse).
library("FactoMineR")
library("factoextra")
res.famd <- FAMD(data_hab_type,
                 ncp = Inf,
                 graph = FALSE,
                 sup.var = quanti.polluant)

# permet d'avoir les valeurs propres
round(res.famd$eig, 3)

png("pollution_matrice_eboulis_valeurs_propres.png", width = 1000, height = 1000)
barplot(res.famd$eig[,1], las = 2, cex.names = .5)
dev.off()

# 90% d’inertie du nuage avec les 47 premiers axes. 
# => CAH réalisée à partir de ces 47 premiers axes. 

ncp <- 47
D <- dist(res.famd$ind$coord[,1:ncp])#distance euclidienne entre observations
res.hclust  <-  hclust(D,method = "ward.D2")#CAH par méthode de Ward

# Hauteurs de fusion en fonction des différentes étapes d’agrégation des classes.
png("pollution_aggr_hauteurs_fusion.png", width = 1000, height = 1000)
barplot(sort(res.hclust$height,decreasing = TRUE)[1:15],
        names.arg = 1:15,
        xlab = "index",
        ylab = "hauteur de fusion")
dev.off()

# Décrochage entre la 6ème et la 5ème barre 
# => gain d’inertie intra-classe élevé lorsqu’on passe d’une partition de 6 classes à 5 classes. 
# => conserver 6 classes pour la partition.
# 
# consolidation par kmeans
nbclasse <- 6
partition <-  cutree(res.hclust, k = nbclasse) #élagage de l'arbre

#Consolidation 
centres.gravite <- by(res.famd$ind$coord[,1:ncp],
                      INDICES = partition,
                      FUN = colMeans) 

# donne un objet de type "matrix", nécessaire pour pouvoir utiliser ces centres 
# comme des valeurs initiales pour la fonction kmeans
centres.gravite <- do.call(rbind, centres.gravite)

res.kmeans <- kmeans(res.famd$ind$coord[,1:ncp],
                     centers = centres.gravite)

part.finale <- as.factor(res.kmeans$cluster)

part.finale

# calculer l’effectif des classes
# La classe 1 concentre le plus de logements, la classe 2 en rassemble le moins.
table(part.finale)

# Description des 6 classes à partir des variables
# on commence par concaténer le jeu de données avec la nouvelle variable classe
data_hab_type_part <- cbind.data.frame(data_hab_type, classe = part.finale)

catdes(data_hab_type_part, num = ncol(data_hab_type_part))


# variable classe en tant que variable illustrative de l'analyse.
res.famd <- FAMD(data_hab_type_part,
                 ncp = Inf,
                 graph = FALSE,
                 sup.var =  c(ncol(data_hab_type_part),quanti.polluant)) 
# export csv
write.infile(res.famd, file = "resultats_afdm.csv")

# Nombre d’axes
# Choix : technique de validation-croisée comme dans l’ACP avec données manquantes.
# on retire les variables illustratives qui ne sont pas gérées par la fonction 
# et inutiles pour déterminer le nombre d'axes
# install.packages("missMDA")
library("missMDA")
res.ncp <- estim_ncpFAMD(data_hab_type_part[,-c(quanti.polluant,ncol(data_hab_type_part))],
                         ncp.max = 10,
                         method.cv = "Kfold",
                         nbsim = 40 # augmenter améliore la précision des résultats, mais aussi le temps de calcul
)

png("validation_croisee_nb_axes.png", width = 1000, height = 1000)
plot(x = as.numeric(names(res.ncp$crit)),
     y = res.ncp$crit,
     xlab = "S",
     ylab = "Erreur",
     main = "Erreur de validation croisée\n en fonction du nombre d'axes",
     type = "b")
dev.off()

# 4 axes à analyser car l’erreur de validation-croisée ne décroît plus au-delà. 
# Attention car ces 4 axes ne représentent que 17% d’inertie cumulée

# Interprétation à l’aide des classes
png("interpretation_validation_croisee_nb_axes.png", width = 1000, height = 1000)
fviz_mfa_ind(res.famd, 
             habillage = "classe", # couleurs selon les modalités de la variable classe 
             palette = c("#000000B3", "#FF0000B3", "#00CD00B3","#0000FFB3","#FF00FFB3","#B8860BB3")# définition des couleurs
)
dev.off()

# barycentres des 6 classes sur le plan factoriel
png("barycentres_validation_croisee_nb_axes.png", width = 1000, height = 1000)
plot(res.famd,
     choix = "quali",
     invisible = c("quali","ind")
)
dev.off()

# Interprétation à l’aide des individus
# Voir les individus qui contribuent le plus à la construction des axes.
png("individus_validation_croisee_nb_axes.png", width = 1000, height = 1000)
plot(res.famd, choix = "ind", invisible = "quali", select = "contrib 30")
dev.off()

# Interprétation à l’aide des variables
png("variables_validation_croisee_nb_axes.png", width = 1000, height = 1000)
plot(res.famd,choix = "var", select = "contrib 15")
dev.off()

# Pour les variables quantitatives uniquement :
png("quanti_validation_croisee_nb_axes.png", width = 1000, height = 1000)
plot(res.famd, choix = "quanti", select = "coord 5", cex=.7)
dev.off()

# Pour les modalités des variables qualitatives
png("quali_validation_croisee_nb_axes.png", width = 1000, height = 1000)
plot(res.famd, choix = "quali", cex = .7)
dev.off()

# Description automatique des axes
res.dimdesc <- dimdesc(res.famd)
lapply(res.dimdesc$Dim.1, round, 3)# pour arrondir à 3 décimales les résultas portant sur la première dimension

lapply(res.dimdesc$Dim.2, round, 3)# pour la seconde
