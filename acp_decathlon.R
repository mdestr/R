# L’analyse multivariée va permettre notamment de résumer les liaisons entre les variables et d’identifier 
# des groupes d’individus aux profils de performances similaires. 
# Les variables actives sont ici celles indiquant les résultats aux épreuves. 
# En effet, nous sommes intéressés par la mise en évidence des différents profils de performances, 
# qui sont uniquement déterminés par les résultats aux 10 épreuves. 
# Les variables Nombres de points et Classement se déduisent des résultats aux épreuves et n’apportent donc pas d’information supplémentaire. 
# Aussi, la variable Epreuve ne caractérise pas les performances de l’athlète. 
# Ainsi, on considèrera les variables Nombre de points, Classement et Epreuve comme des variables supplémentaires (illustratives). 
# Ici nous appliquons une ACP car les variables actives sont toutes quantitatives. 
# Les variables actives n’ayant pas les mêmes unités, il s’impose d’effectuer une ACP réduite.

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

decathlon<-read.table("./decathlon/decathlon.txt", 
                       stringsAsFactors = TRUE)

# install.packages("FactoMineR")
library(FactoMineR)

# on effectue l'ACP (réduite et avec 5 dimensions par défaut)
res.pca <- PCA(decathlon,
               quanti.sup = c(11: 12),
               quali.sup = c(13),
               graph = FALSE)

# export des résultats dans un fichier
write.infile(res.pca, file = "./resultats_ACP.csv")

#valeurs propres
res.pca$eig

barplot(res.pca$eig[,1], las = 2)



# La règle de Kaiser suggère de retenir 4 axes, la règle du coude également (coude entre 4eme et 5eme barres)1. 
# Les 4 premiers axes cumulent environ 75% de l’inertie du nuage. 
# Ceci est relativement élevé au vu du nombre de variables actives car avec seulement 4 axes, 
# on résume à hauteur de 75% un jeu de données en dimension 10. 
# Notons qu’en analysant simplement le premier plan, l’inertie ne serait que de 50%, trop faible dans l’absolu : 
#   la moitié de l’information portée par le jeu de données ne serait pas analysée. 
# Il paraît donc nécessaire d’analyser les 4 premiers axes (i.e. les deux premiers plans).

# On analyse d’abord le plan principal (appelé aussi plan 1-2).
#graphes des individus
plot.PCA(res.pca, choix = "ind")

#graphes des variables
plot.PCA(res.pca, choix = "var")

# Pour interpréter ce premier plan, on identifie d’abord les individus aux plus fortes contributions 
# sur l’axe 1 d’une part, et sur l’axe 2 d’autre part. 
# Les individus ayant tous le même poids, ceci revient à identifier ceux qui ont les coordonnées les plus élevées en valeur absolue.
# Pour l’axe 1, Sebrle, Clay, Karpov (JO) et Bourguignon (Decastar) contribuent le plus à la construction de l’axe 1. 
# La visualisation des données centrées-réduites pour ces individus nous fournit des premiers éléments d’interprétation.
# 
# Pour l’axe 2, Casarsa (JO) et Drews (JO) contribuent le plus à la construction de cet axe.
# On regarde ensuite le cercle des corrélations pour déterminer les variables liées aux coordonnées des individus.
# On peut ainsi donner une interprétation à chaque axe.
# NB : quand le nombre de variable est élevé, on a besoin d’outils automatiques pour décrire les axes. 
# La fonction dimdesc effectue des tests sur les liaisons entre les variables et les axes 
# et ne renvoie que les variables pour lesquelles cette liaison est “significative”. 
# Attention toutefois à ne pas sur-interpréter les valeurs tests et p-valeurs car les axes ayant été construits à partir des variables, 
# il y a nécessairement un lien entre les variables et les axes et cette procédure est donc biaisée. 
# Seules les valeurs-tests et p-valeurs des variables illustratives pourront être interprétées comme usuellement. 
# Ceci n’est pas problématique ici car ce qui intéresse l’utilisateur c’est essentiellement de hiérarchiser les variables.
# 
# L’ACP répond à d’autres objectifs que l’identification des principales dimensions de variabilité : 
#   elle permet aussi d’identifier des proximités entre individus et entre variables. 
# Pour les individus, il est nécessaire de s’assurer de leur qualité de représentation en analysant les cosinus carrés.
round(res.pca$ind$cos, 2)

# Par exemple, Sebrle et Clay (JO) sont proches sur le plan 1-2, 
# et ont tous deux une bonne qualité de représentation, leurs cosinus carrés étant supérieurs à 0.74. 
# Leur proximité dans le plan traduit donc une proximité dans le nuage initial (i.e. avant projection). 
# Par conséquent, ce sont des athlètes aux profils similaires.
# 
# Pour ce qui est des liaisons entre variables, la qualité de représentation se lit directement 
# par la proximité entre les flèches et le bord du cercle. 
# Aucune d’entre elles n’est très bien projetée, ce qui empêche d’avoir une idée précise de leur corrélation.
# Mais certaines le sont suffisamment pour mettre en évidence certaines liaisons ou non-liaisons. 
# Par exemple, Disque et Poids sont des variables corrélées positivement (angle proche de 0 degrés). 
# Naturellement, pour disposer d’informations plus précises quant aux liaisons entre variables, 
# il suffit d’aller lire la matrice de corrélation, 
# mais le cercle des corrélations a l’avantage de mettre en évidence certaines d’entre elles.

# Nous arrêtons ici l’exploration de ces données, mais il est nécessaire en pratique d’aller au-delà du premier plan. 
# Pour obtenir les sorties correspondantes, il suffit de procéder comme suit :

res.pca <- PCA(decathlon,
               scale.unit = TRUE,
               quanti.sup = c(11:12),
               quali.sup = c(13),
               graph = FALSE, ncp = Inf)


#graphes des individus
plot.PCA(res.pca, axes = c(3, 4), choix = "ind")

#graphes des variables
plot.PCA(res.pca, axes=c(3, 4), choix = "var")

# Quand le nombre de variables ou d’individus est élevé il peut alors être utile d’alléger les représentations graphiques 
# en ne représentant que certains éléments : par exemple, les variables aux plus fortes contributions pour le cercle des corrélations, 
# ou les individus aux cosinus carrés les plus élevés pour le graphe des individus. 
# Ceci peut s’effectuer à l’aide des options de la fonction plot.PCA ou de façon interactive via le package R Factoshiny.